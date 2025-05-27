#' @title
#' Derive total inpatient cost for each hospital stay.
#'
#' @description
#' `derive_total_inpatient_cost.R` derives the total cost of an encounter's
#' hospitalization based on Resource Intensity Weights (RIW), which represent
#' weighted costs relative to average inpatient cost. This function is currently
#' only able to derive inpatient costs for encounters with a methodology year 
#' that's between 2017 and 2022.
#' 
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#' @param cohort (`data.frame` or `data.table`)\cr
#' User specified cohort that's a restricted subset of all encounters in the DRM
#' table "ipadmdad" (see [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/)).
#' Must contain `genc_id` as the identifier. 
#' 
#' @param reference_year (`int`)\cr
#' The year to which the derived costs should be adjusted to due to inflation.
#' As a default, reference year is decided to be the fiscal year of the latest
#' discharge in the provided cohort. For example, if the latest discharge is
#' on 2023-03-24, the reference year will be 2022 aligning with the fiscal year.
#' 
#' @import RPostgreSQL cansim stringr
#' 
#' @return
#' This function returns a `data.table` containing `genc_id`,
#' `derived_total_inpatient_cost`, `inflation_rate`, and
#' 'derived_total_inpatient_cost_adjusted', the last of which corresponds
#' to the total inpatient costs are adjusted to prices in the reference year 
#' if provided. If the reference year is not provided, then costs are adjusted
#' to the most recent fiscal year in the cohort.
#' 
#' @references 
#' include references to CIHI's CSHS table

# Function that derives cost of hospitalization based on
# Resource Intensity Weights (RIW), which represents weighted costs relative to
# average inpatient cost. The actual cost amount can be inferred based on CMG
# methodology year (by year, based on when the data were pulled) and needs to
# be adjused for (healthcare specific) inflation.
# The suggested general approach used in the Pediatric Complexity project:
# 1. Create a scv file for CPWC(CSHS) for each hospital number and year, based
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
# 7. Merge in CSHS data by hospital_num
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
library(stringr)

# Useful documentation 

# CSHS methodology: https://www.cihi.ca/sites/default/files/document/cost-standard-hospital-stay-methodology-notes-en.pdf
# CSHS by hospital computation
# Source: https://www.cihi.ca/sites/default/files/document/cmdb-user-guide-2021-2022-en.pdf
# CSHS_hospital = total_hospital_inpatient_costs / total_hospital_RIW
# "CSHS is an indicator that measures the relative cost efficiency of a 
# hospital's ability to provide acute inpatient care. This indicator compares a
# hospital’s total acute inpatient care expenses with the number of acute
# inpatient weighted cases related to the inpatients that it provided
# care for. The result is the hospital’s average full cost of treating the
# average acute inpatient. A high CSHS indicates a relatively high cost of
# treating the average acute inpatient; a low CSHS indicates that the cost of
# treating the average acute inpatient is relatively low"

# soursc relevant Rgemini functions
source("~/repos/Rgemini/Rgemini/R/n_missing.R")
source("~/repos/Rgemini/Rgemini/R/utils.R")
# load dbconnection for testing
#drv <- dbDriver("PostgreSQL")
#dbcon <- dbConnect(drv, dbname = "drm_cleandb_v3_1_0", host = "prime.smh.gemini-hpc.ca", port = 5432, user = "anoutchinad", pass = getPass("Pass: "))

#cohort <- dbGetQuery(dbcon, "SELECT * FROM public.admdad WHERE discharge_date_time < '2020-06-30 23:59'") %>% data.table()

derive_total_inpatient_cost <- function(dbcon, cohort, reference_year = NA) {
  ## check user inputs
  check_input(dbcon, "DBI")
  check_input(cohort, c("data.table", "data.frame"), colnames = "genc_id")
  if (!is.na(reference_year)) {
      check_input(reference_year, "integer") 
  }

  ## Detect if we're using hospital_id or hospital_num
  if ("hospital_num" %in% names(cohort)) {
    hosp_identifier <- "hospital_num"
  } else {
    hosp_identifier <- "hospital_id"
  }


  ## TODO: CITE COVID FEATURES IN RIW
  cat("\n *** WARNING: Resource Intensity Weights (RIW) and Case Mix Groups (CMG) are calculated differently year by year, and the inpatient costs derived by this function use the methodologies of the year an encounter was discharged. Year by year the features to model RIW values change, for example, in 2020 covid specific features were added to the computation. These RIW values are directly used to compute total inpatient cost, as they represent the relative resources, intensity, and weight of each inpatient case compared to an average case. Please keep in mind that as a result, the derived costs are not standardized in terms of methodologies across years. ***\n")

  # Resource intensity weight represents the relative resources, intensity, and weight of each inpatient case compared with the typical average case that has a value of 1.0000. 
  
  # create temp table for cohort_gencs to pull ipcmg
  DBI::dbSendQuery(dbcon, "DROP TABLE IF EXISTS temp_g;")
  DBI::dbWriteTable(dbcon, c("pg_temp", "temp_g"), data.table(cohort[, .(genc_id)]))
  # Analyze to speed up table
  DBI::dbSendQuery(dbcon, "ANALYZE temp_g;")

  # get ipcmg for cohort
  ipcmg <- DBI::dbGetQuery(dbcon, "SELECT i.genc_id, cmg, diagnosis_for_cmg_assignment, comorbidity_level, riw_inpatient_atypical_indicator, methodology_year, riw_15, hospital_id FROM public.ipcmg i INNER JOIN temp_g t ON i.genc_id = t.genc_id;") %>% data.table()

  # TODO: Make sure cohort contains admission_date_time + discharge_date_time or just pull it here 

  # merge ipcmg with cohort to have adm/dis dates
  cohort_cmg <- merge(cohort[, .(genc_id, admission_date_time, discharge_date_time)], ipcmg, by = "genc_id", all.x = TRUE) %>% data.table()

  ########################### Handle Missingness ###########################
  ## Impute Methodology years
  cat("\n Imputing missing methodology year where appropriate. \n\n")

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
  print(paste0(missing_yr, " rows are missing methodology year. Removing."))
  cat("\n\n")
  cohort_cmg <- cohort_cmg %>% filter(!is.na(methodology_year))


  # Q: Is riw_15 = 0 considered missing? Since the result effectively says
  #    that for rows with riw_15 = 0 total cost = 0. A: Find out what RIW_15 = 0
  #    means in CIHI definitions. If there's some meaning, then decide what we
  #    should do. A: RIW_15 is not a valid RIW value according to CIHI.
  #    should be dropped.

  # impute RIW for missing rows
  cat("\n Imputing missing riw_15 where appropriate. \n\n")
  cohort_cmg[, riw_15 := ifelse(is.na(riw_15) | riw_15 == 0, na.omit(riw_15)[1], riw_15), by = .(cmg, diagnosis_for_cmg_assignment, comorbidity_level, riw_inpatient_atypical_indicator, methodology_year)]
  
  # remove rows missing riw or have riw_15 = 0 as the latter is not a valid
  # riw value as per cihi
  missing_riw <- n_missing(cohort_cmg$riw_15)
  print(paste0(missing_riw, " rows are missing riw_15 or have riw_15 = 0. Removing."))
  cat("\n\n")
  cohort_cmg <- cohort_cmg %>% filter(!is.na(riw_15) & riw_15 != 0)

  ########################### Compute Inpatient Cost ###########################
  ## merge CSHS data by hospital num

  # Q: CHSC data only has data for up to fiscal/methodology year 2022 and only
  #    since fiscal/methodology year 2018. How should we handle these values?
  #    We definitely need to document this in the function, but should we remove
  #    rows from the output? A: Don't remove, see what interpolation techniques
  #    we could use until the data is available. Let the user know.


  ## TODO: How do we handle encounters whose methodology year isn't included in
  ## the CPWC data that we have? Currently just not computing.

  load("Rgemini/data/mapping_cihi_cshs.rda")
  cshs_data <- data.table(hospital_id, hospital_name, fiscal_year, cost_of_standard_hospital_stay, hospital_num)
  setnames(cshs_data, old = "fiscal_year", new = "methodology_year")

  # Check if cohort contains any methodology years that fall outside of
  # the years for which we have cpwc for.

  if (min(cohort_cmg$methodology_year) < min(cshs_data$methodology_year) | 
      max(cohort_cmg$methodology_year) > max(cshs_data$methodology_year)) {
    print(paste0("*** WARNING: The provided cohort has rows with methodology years that are earlier or later than the years that we have Cost of Standard Hospital Stay (CSHS) values available for. These rows will not have a inpatient cost calculation.", "The earliest year with CSHS data is ", min(cshs_data$methodology_year), " and the latest year with CSHS data is ", max(cshs_data$methodology_year), ". The min and max methodology years in the provided cohort are ", min(cohort_cmg$methodology_year), " and ", max(cohort_cmg$methodology_year), " respectively. ***"))
  }

  # merge cshs table with cohort.
  cshs_merge <- cshs_data[cohort_cmg, on = c(hosp_identifier, "methodology_year")]

  # compute unadjusted derived total inpatient cost by multiplying
  # riw_15 by cost of standard hospital stay for that year and hospital.
  cshs_merge[, derived_total_inpatient_cost := (riw_15 * cost_of_standard_hospital_stay)]


  ################## Adjust derived total cost for inflation ##################
  # Adjusting cost by doing:
  # current_cost = original_cost * (CPI_curr_yr / CPI_orig_year), specifically
  # taking CPI for 'Health Care' within the 'Health and Personal Care'
  # grouping in April of that fiscal/methodology year.
  # Using CPI scaled to 2002 = 100 as this is the official time base in Canada

  ## get cpi values for ref_dates ending in -04
  # (i.e. cpi value for 2018 is given at REF_DATE = '2018-04') from
  # statcan table: 18-10-0004-01.

  cpi_values_apr <- get_cansim_connection("18-10-0004-01") |>
      dplyr::filter(GEO == "Canada" & UOM == "2002=100" & `Products and product groups` == "Health care" & REF_DATE >= "2018-04" & grepl("-04", REF_DATE)) |>
      collect_and_normalize()
  
  cpi_values_apr <- cpi_values_apr %>%
      select(REF_DATE, VALUE, UOM) %>%
      data.table()
  # add methodology year column for merging into cshs_merge
  cpi_values_apr[, methodology_year := as.integer(substr(REF_DATE, 1, 4))]

  ## Get reference year to which we adjust costs to
  # If month is < 4, subtract 1 from max_discharge date year as we're still
  # in the previous fiscal year.

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
  # select relevant columns from cshs_merge
  cshs_merge_small <- cshs_merge[, .(genc_id, admission_date_time, discharge_date_time, methodology_year, hospital_id, derived_total_inpatient_cost)]
  
  # merge derived costs with cpi values
  result <- cpi_values_apr[cshs_merge_small, on = "methodology_year"]
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


#cost <- derive_total_inpatient_cost(dbcon, cohort)



## trying scraping

#library(rvest)
#library(jsonlite)
#library(tidyverse)
#library(lubridate)

# useful for pulling CIHI your healthy system data
# https://yourhealthsystem.cihi.ca/hspidas/docs/api/indicator/trend.jsp

# JSON pull of sbk data https://yourhealthsystem.cihi.ca/hspidas/indicator/trend?indicatorCode=015&zoneCode=O10093


#chsc_data_temp <- chsc_data
#codes <- c("O10093", "O10027", "O10027", "O10027", "O5137", "O80258", "O80258", "O5210", "O80380", "O5142", "O5142", "O80169", "O80169", "O1096", "O1096", "O80290", "O10020", "O10020", "O10020", "O5224", "O5159", "O5302", "O80497", "O81124", "O80497", "O5141", "O81100", "O20392", "O20392", "O5159", "O5159", "O10018", "O10018", "O10018")
#hosp_id <- c("SBK", "HHCO", "HHCM", "HHCG", "GRH", "HHSH", "HHSJ", "HRH", "KGH", "LHSCU", "LHSCV", "MKHR", "MKHV", "MKSH", "MKSHX", "MSH", "NHGN", "NHSC", "NHWH", "NYGH", "PMH", "SAH", "SJHC", "SMGH", "SMH", "TBRH", "TEHNM", "THPC", "THPM", "UHNTG", "UHNTW", "WOHSB", "WOHSE", "WOHSR")

#cihi_hosp_codes <- data.table(hosp_id, codes)


#pulls <- data.table()
#glimpse(pulls)
#for(i in 1:nrow(cihi_hosp_codes)) {
#    cihi_url <- paste0("https://yourhealthsystem.cihi.ca/hspidas/indicator/trend?indicatorCode=015&zoneCode=", cihi_hosp_codes[i]$codes)
#    data <- jsonlite::fromJSON(cihi_url)
#    trends <- data$zones$fiscalYears
#    site_data <- rbind(trends[[1]]$metrics[[1]], trends[[1]]$metrics[[2]], trends[[1]]$metrics[[3]], trends[[1]]$metrics[[4]], trends[[1]]$metrics[[5]]) %>% select(indicatorValue, dataPeriodEDesc) %>% rename(methodology_year = dataPeriodEDesc)
#    site_data$site <- cihi_hosp_codes[i]$hosp_id
#    site_data$methodology_year <- substr(site_data$methodology_year, 1, 4)
#    pulls <- rbind(pulls, site_data)
#}

