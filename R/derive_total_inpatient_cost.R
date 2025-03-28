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
library(cansim)

# load dbconnection for testing
drv <- dbDriver("PostgreSQL")
dbcon <- dbConnect(drv, dbname = "drm_cleandb_v3_1_0", host = "prime.smh.gemini-hpc.ca", port = 5432, user = "anoutchinad", pass = getPass("Pass: "))

cohort <- dbGetQuery(dbcon, "SELECT * FROM public.admdad WHERE discharge_date_time > '2020-06-30 23:59'") %>% data.table()

derive_total_inpatient_cost <- function(dbcon, cohort, reference_year = NA) {
  ## check user inputs
  check_input(dbcon, "DBI")
  check_input(cohort, c("data.table", "data.frame"), colnames = "genc_id")
  if (!is.na(reference_year)) {
      check_input(reference_year, "integer") # maybe add interval constraint to be greater than smallest year that has CPI available
  }

  ## TODO:: Detect if we're using hospital_id or hospital_num
  
  # create temp table for cohort_gencs to pull ipcmg
  DBI::dbSendQuery(dbcon, "DROP TABLE IF EXISTS temp_g;")
  DBI::dbWriteTable(dbcon, c("pg_temp", "temp_g"), data.table(cohort[, .(genc_id)]))
  # Analyze to speed up table
  DBI::dbSendQuery(dbcon, "ANALYZE temp_g;")

  # get ipcmg for cohort
  ipcmg <- DBI::dbGetQuery(dbcon, "SELECT i.genc_id, cmg, diagnosis_for_cmg_assignment, comorbidity_level, riw_inpatient_atypical_indicator, methodology_year, riw_15, hospital_id FROM public.ipcmg i INNER JOIN temp_g t ON i.genc_id = t.genc_id;") %>% data.table()

  # merge ipcmg with cohort to have adm/dis dates
  cohort_cmg <- merge(cohort[, .(genc_id, admission_date_time, discharge_date_time)], ipcmg, by = "genc_id", all.x = TRUE) %>% data.table()

  ########################### Handle Missingness ###########################
  ## Impute Methodology years
  cat("\n Imputing missing methodology year where appropriate... \n\n")

  # Impute missing years based on cmg, diagnosis_for_cmg_assignment, 
  # comorbidity_level, and riw_inpatient_atypical_indicator
  cohort_cmg[, methodology_year := ifelse(is.na(methodology_year), na.omit(methodology_year)[1], methodology_year), by = .(cmg, diagnosis_for_cmg_assignment, comorbidity_level, riw_inpatient_atypical_indicator)]

  # impute methodology year for rows w abs(year - adm_year) > 1
  cohort_cmg$admission_year <- as.integer(substr(cohort_cmg$admission_date_time, 1, 4))
  cohort_cmg$discharge_year <- as.integer(substr(cohort_cmg$discharge_date_time, 1, 4))

  cohort_cmg[, methodology_year := ifelse(abs(methodology_year - admission_year) > 1 | abs(methodology_year - discharge_year) > 1, na.omit(methodology_year)[1], methodology_year), by = .(cmg, diagnosis_for_cmg_assignment, comorbidity_level, riw_inpatient_atypical_indicator)]
  
  # Q: Are we removing rows w abs(year - adm_year) > 1?

  ## Remove rows where methodology_year is still missing after imputing
  missing_yr <- n_missing(cohort_cmg$methodology_year)
  print(paste0(missing_yr, " rows are missing methodology year. Removing..."))
  cat("\n\n")
  cmg_final <- cohort_cmg %>% filter(!is.na(methodology_year))

  ## Remove rows missing riw_15
  # Q: Can I impute riw_15 using the same imputation method for method year?
  # Q: Is riw_15 = 0 considered missing? Since the result effectively says
  #    that for rows with riw_15 = 0 total cost = 0.
  missing_riw <- n_missing(cohort_cmg$riw_15)
  print(paste0(missing_riw, " rows are missing riw_15. Removing..."))
  cat("\n\n")
  cohort_cmg <- cohort_cmg %>% filter(!is.na(riw_15))

  ########################### Compute Inpatient Cost ###########################
  ## merge CHSC data by hospital num
  # TODO: Add hospital num into chsc_data csv rather than hospital name,
  #       And change merge to merge by hospital number

  # Q: CHSC data only has data for up to fiscal/methodology year 2022 and only
  #    since fiscal/methodology year 2018. How should we handle these values?
  #    We definitely need to document this in the function, but should we remove
  #    rows from the output?
  chsc_data <- fread("~/repos/Rgemini/Rgemini/data/CPWC_data.csv")
  setnames(chsc_data, old = "fiscal_year", new = "methodology_year")
  chsc_merge <- merge(cohort_cmg, chsc_data, by = c("hospital_id", "methodology_year"))

  # compute unadjusted derived total inpatient cost by multiplying
  # riw_15 by cost of standar hospital stay for that year and hospital.
  chsc_merge[, derived_total_inpatient_cost := (riw_15 * cost_of_standard_hospital_stay)]


  ################## Adjust derived total cost for inflation ##################
  # Adjusting cost by doing:
  # current_cost = original_cost * (CPI_curr_yr / CPI_orig_year), specifically
  # taking CPI for 'Health Care' within the 'Health and Personal Care'
  # grouping in April of that fiscal/methodology year.
  # Using CPI scaled to 2002 = 100 as this is the official time base in Canada

  ## get cpi values for ref_dates ending in -04
  # (i.e. cpi value for 2018 is given at REF_DATE = '2018-04') from
  # statcan table: 18-10-0004-01.

  ### Q: Do we agree the CPI for that fiscal year should be the one taken
  ###    at the start of the fiscal year?
  ### Q: Currently I'm taking CPI for health care for all of Canada, should I 
  ###    restrict it to Ontario only?

  cpi_values_apr <- get_cansim_connection("18-10-0004-01") |>
      dplyr::filter(GEO == "Canada" & UOM == "2002=100" & `Products and product groups` == "Health care" & REF_DATE >= "2018-04" & grepl("-04", REF_DATE)) |>
      collect_and_normalize()
  
  cpi_values_apr <- cpi_values_apr %>%
      select(REF_DATE, VALUE, UOM) %>%
      data.table()
  # add methodology year column for merging into chsc_merge
  cpi_values_apr[, methodology_year := as.integer(substr(REF_DATE, 1, 4))]

  ## Get reference year to which we adjust costs to
  ### Q: Should we give the user an option to choose what year to adjust to?
  ###    Currently I'm adjusting to the largest discharge year in the cohort
  ###    
  if (is.na(reference_year)) {
      max_discharge_date <- substr(max(cohort$discharge_date_time), 1, 10)
      if (as.integer(substr(max_discharge_date, 7, 7)) < 4) {
        reference_year <- as.integer(substr(max_discharge_date, 1, 4)) - 1
      } else {
        reference_year <- as.integer(substr(max_discharge_date, 1, 4))
      }
  }
  print(paste0("Adjusting derived total inpatient cost to fiscal year ", reference_year, "..."))
  cat("\n\n")

  ## Adjust derived costs to reference year
  # select relevant columns from chsc_merge
  chsc_merge_small <- chsc_merge[, .(genc_id, admission_date_time, discharge_date_time, methodology_year, hospital_id, derived_total_inpatient_cost)]
  
  # merge derived costs with cpi values
  result <- merge(chsc_merge_small, cpi_values_apr, by = "methodology_year")
  reference_cpi <- cpi_values_apr %>% filter(methodology_year == as.integer(reference_year))

  # get percent change in cpi for methodology year to ref year
  result[, inflation_rate := reference_cpi$VALUE / VALUE]

  # multiply derived total inpatient cost by inflation percent change
  result[, paste0("derived_total_inpatient_cost_adjusted_", reference_year) := derived_total_inpatient_cost * inflation_rate]
  
  ## Return (what do we want to return aside from )
  result <- result %>% select(genc_id, derived_total_inpatient_cost, inflation_rate, grep("adjusted", names(result), value = TRUE))

  cat("\nDONE!")
  return(result)
}



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

## trying scraping
library(rvest)
library(jsonlite)
library(tidyverse)
library(lubridate)

# useful for pulling CIHI your healthy system data
# https://yourhealthsystem.cihi.ca/hspidas/docs/api/indicator/trend.jsp

# JSON pull of sbk data https://yourhealthsystem.cihi.ca/hspidas/indicator/trend?indicatorCode=015&zoneCode=O10093

# read static html


test_data <- jsonlite::fromJSON(test_url)
test_url <- "https://yourhealthsystem.cihi.ca/hspidas/indicator/trend?indicatorCode=015&zoneCode=O10093"
trends <- test_data$zones$fiscalYears
metrics <- test_data$zones$fiscalYears[]
test2 <- trends %>% data.table()
zones <- test_data$zones
trends[2]

trends[[1]]$metrics[1]
