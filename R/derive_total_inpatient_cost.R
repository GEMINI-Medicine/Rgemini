# Function that derives cost of hospitalization based on
# Resource Intensity Weights (RIW), which represents weighted costs relative to
# average inpatient cost. The actual cost amount can be inferred based on CMG
# methodology year (by year, based on when the data were pulled) and needs to
# be adjused for (healthcare specific) inflation.
# The suggested general approach used in the Pediatric Complexity project:
# 1. Create a scv file for CPWC(CHSC) for each hospital number and year, based
# on cihi info
# 2. This CSV file then needs to be merged with other GEMINI tables as follows:
# 3. Merge ipcmg into your cohort by genc_id, assess missingness of methodology
#    year and riw_15
# 4. Methodology_year has some missing data. If methodology_year is empty, but
#    there is another row that is otherwise identical (in terms of cmg,
#    diagnosis_for_cmg_assessment, comorbidity_level, riw_inpatient_atypical_
#    indicator), impute the methodology_year from that row.
# 5. Methodology_year also has some values that are >1 year different than 
#    calendar year, calling into question data quality. We use the same
#    process as above to impute methodology years in rows where methodology_
#    year and calendar year are > 1 year apart.
# 6. Report all records excluded due to missing values required for total cost
#    calculation (i.e. RIW).
# 7. Merge in CHSC data by hospital_num
# 8. Multiply riw_15 by CHSC
# 9. Adjust for healthcare-specific inflation
# Once the method has been developed, we should triangulate the results with
# some publicly reported values (probably from CIHI) to make sure we are in the
# ballpark.

# useful links: 
# http://mchp-appserv.cpe.umanitoba.ca/viewConcept.php?conceptID=1100
library(DBI)
library(dplyr)
library(data.table)
library(RPostgreSQL)
library(getPass)
library(Rgemini)
library(GEMINIpkg)

# prelim function exploration

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname = "drm_cleandb_v3_1_0", host = "prime.smh.gemini-hpc.ca", port = 5432, user = "anoutchinad", pass = getPass("Pass: "))

cohort <- dbGetQuery(db, "SELECT * FROM public.admdad WHERE discharge_date_time > '2020-06-30 23:59'") %>% data.table()

# Temp table for cohort genc_ids
dbSendQuery(db, "drop table if exists temp_g")
dbWriteTable(db, c("pg_temp", "temp_g"), data.table(cohort[,.(genc_id)]), temporary = TRUE, overwrite = FALSE, row.names = FALSE)
dbSendQuery(db, "ANALYZE temp_g")

# get ipcmg
ipcmg <- dbGetQuery(db, "SELECT * FROM public.ipcmg a INNER JOIN temp_g g ON a.genc_id = g.genc_id;") %>% data.table()
ipcmg <- ipcmg %>% select(-11)

# merge ipcmg with cohort
cohort_cmg <- merge(cohort[,.(genc_id, admission_date_time, discharge_date_time)], ipcmg, by = "genc_id", all.x = TRUE) %>% data.table()
glimpse(cohort_cmg)

# remove records which are missing riw_15
cohort_cmg <- cohort_cmg %>% filter(!is.na(riw_15))
cat("\n Removing rows missing riw_15... \n\n")
n_missing(cohort_cmg$methodology_year)

# Q: is riw_15 = 0 considered missing?

missing_year <- cohort_cmg %>% filter(is.na(methodology_year) | methodology_year == "")

# impute missing years as best as you can
cat("\n Imputing missing methodology year where appropriate... \n\n")
cohort_cmg[, methodology_year := ifelse(is.na(methodology_year), na.omit(methodology_year)[1], methodology_year), by = .(cmg, diagnosis_for_cmg_assignment, comorbidity_level, riw_inpatient_atypical_indicator)]

# impute methodology year for rows w abs(year - adm_year) > 1
cohort_cmg$admission_year <- as.integer(substr(cohort_cmg$admission_date_time, 1, 4))
cohort_cmg$discharge_year <- as.integer(substr(cohort_cmg$discharge_date_time, 1, 4))
cohort_cmg[, methodology_year := ifelse(abs(methodology_year - admission_year) > 1 | abs(methodology_year - discharge_year) > 1, na.omit(methodology_year)[1], methodology_year), by = .(cmg, diagnosis_for_cmg_assignment, comorbidity_level, riw_inpatient_atypical_indicator)]

# remove rows still missing year after imputing
num_missing <- n_missing(cohort_cmg$methodology_year)
print(paste0(num_missing, " rows are missing methodology year. Removing..."))
cat("\n\n")

### Might also need to remove rows w difference between year and method year > 1
cmg_final <- cohort_cmg %>% filter(!is.na(methodology_year))

# merge CHSC data by hospital_num
chsc_data <- fread("~/repos/Rgemini/Rgemini/data/CPWC_data.csv")
chsc_data$methodology_year <- as.integer(substr(chsc_data$year_start, 1, 4))
# for the time being remove methodology year 2023
cmg_final <- cmg_final %>% filter(methodology_year != 2023)
chsc_merge <- merge(cmg_final, chsc_data[, .(hospital_id, methodology_year, cost_of_standard_hospital_stay)], by = c("hospital_id", "methodology_year"))
glimpse(chsc_merge)

chsc_merge[, derived_total_inpatient_cost := (riw_15 * cost_of_standard_hospital_stay)]

# adjust for healthcare specific inflation using Canada Health Care consumer
# price index.
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1810000408
# adjusted cost of sth in '20 in '23 = original_cost * (CPI in 2023/ CPI 2020)
# CPI data assuming 2002 = 100, and specifically Health care, not health and
# personal care
## We should determine what month we take the CPI from (start of fiscal year?)
## Might actually be the end of fiscal year since CHSC values are from apr-may
## of the following year 

# cpi values scaled to 100 for 2002 for all of Canada (or should we do Ontario)
# in the future maybe we can use scraping to get updated values
# scale to 2002 because it's the official time base in canada
cpi_values_apr <- data.table(
    methodology_year = c(2018, 2019, 2020, 2021, 2022, 2023, 2024),
    cpi_health_and_personal_care = c(126.2, 127.1, 128.7, 132.5, 137.0, 145.7, 150.0),
    cpi_health_care = c(128.6, 129.4, 131.6, 134.5, 137.4, 144.7, 148.5)
)
