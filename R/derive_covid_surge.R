#' @title
#' Derive COVID Surge
#'
#' @description
#' A function that derives the COVID-19 surge index for a given time period.
#' For time periods before 2020, or where COVID was not yet diagnosed, the surge index will be 0.
#'
#' @param db (`DBIConnection`)\cr
#' RPostgres DB connection.
#'
#' @param time_period (`vecotr`)\cr
#' A time period of interest to derive a surge index for. 
#' For example, if the user would like to investigate the surge from 
#' 2020-10-01 to 2023-01-01, the user should supply 
#' `c('2020-10-01', '2023-01-01')`.
#'
#' @param gim_only (`logical`)\cr
#' Flag denoting if the user would like the surge index for GIM patients only.
#' Note: This argument is set to FALSE by default.
#' 
#' @param include_er (`logical`)\cr
#' Flag denoting if ER intervention codes are to be included in the calculation.
#' Note: This argument is set to FALSE by default.
#'
#' @return (`data.frame`)\cr
#' `hospital_id` (`character`),\cr
#' `month_year` (`character`),\cr
#' `surge_index` (`numeric`)
#'
#' @import DBI RPostgreSQL
#' @importFrom lubridate ymd_hm floor_month
#' @export
#'
#' @examples
#' \dontrun{
#' drv <- DBI::dbDriver("PostgreSQL")
#' db <- DBI::dbConnect(
#'     drv,
#'     dbname = "db_name",
#'     host = "domain_name.ca",
#'     port = 1234,
#'     user = getPass::getPass("Enter Username"),
#'     password = getPass::getPass("Enter Password")
#' )
#'
#' covid_surge <- derive_covid_surge(db = db, 
#' time_period = c('2020-10-01','2023-01-01'),
#' gim_only = FALSE,
#' include_er = FALSE)
#'
#' }
#'
#' @references
#' The formula to derive the COVID surge index is from :
#' https://doi.org/10.1001/jamanetworkopen.2023.23035
#'

## to address:
### - er vs ip codes
### - adding in gim vs all-med functionality
### - cell supression: since bed counts are easily accessible online, is it possible to back-calculate number of encounters in a given month? (especially on fringe months)


derive_covid_surge <- function(db, time_period = c("2015-04-01", "2023-06-30"), gim_only = FALSE, include_er = FALSE){
    # Surge index (per hospital-month) = ([(n without ICU, noninvasive positive pressure ventilation, or mechanical ventilation) + 2 × (n with noninvasive positive pressure ventilation or ICU) + 5 × (n with mechanical ventilation)] × 10) / (pre–COVID-19 baseline bed capacity of medical and ICU beds).

    # where n = # of COVID admissions

    ## invasive - 1GZ31CBND
    ## non-invasive - 1GZ31CAND
    ### pull icu encounters
    icu_encounters <- dbGetQuery(db, "select distinct genc_id from ipscu where icu_flag = TRUE;") %>% data.table()

    ## pull invasive ventilation encounters
    invasive_vent_encounters <- dbGetQuery(db, "select distinct genc_id from ipintervention where intervention_code = '1GZ31CBND'") %>% data.table()

    ## pull non-invasive ventilation encounters
    non_invasive_vent_encounters <- dbGetQuery(db, "select distinct genc_id from ipintervention where intervention_code = '1GZ31CAND'") %>% data.table()

    ## now pull covid encounters for time period
    covid_query <- paste0("select distinct ip.genc_id, ip.hospital_id, a.discharge_date_time from ipdiagnosis ip join admdad a on ip.genc_id = a.genc_id where ip.diagnosis_code ~ '^U071' and a.discharge_date_time >= '", time_period[1], "' and a.discharge_date_time <= '", time_period[2], "'")

    covid_encounters <- dbGetQuery(db, covid_query) %>%
        data.table()

    ### now that all genc_ids are pulled, can make numerator
    ## encounters without ventilation or ICU
    covid_encounters[, no_icu_vent := genc_id %ni% c(icu_encounters$genc_id, non_invasive_vent_encounters$genc_id, invasive_vent_encounters$genc_id)]

    ### encounters with non-invasive OR ICU
    covid_encounters[, icu_or_non_invasive_vent := genc_id %in% c(icu_encounters$genc_id, non_invasive_vent_encounters$genc_id)]

    ### encounters with invasive vent
    covid_encounters[, invasive_vent := genc_id %in% invasive_vent_encounters$genc_id]

    ### now make numerator
    ## make month-year
    covid_encounters <- covid_encounters %>%
        mutate(month_year = floor_date(ymd_hm(discharge_date_time), unit = "month"))

    ## pull encounters in time period
    #### keeping hospital_id since num is not on cleandb dad rn
    ## create query first
    cohort_query <- paste0("select a.genc_id, l.hospital_num, a.hospital_id, a.admission_date_time, a.discharge_date_time from admdad a join lookup_hospital l on a.hospital_id = l.hospital_id where discharge_date_time >= '", time_period[1], " 00:00' and discharge_date_time <= '", time_period[2], " 23:59';")

    cohort <- dbGetQuery(db, cohort_query) %>%
        data.table()

    ## merge in surge table data, replace NAs in non-COVID encounters
    ## create month_year for aggregating
    cohort <- cohort %>%
        merge(covid_encounters, all.x = TRUE) %>%
        mutate(across(c("no_icu_vent", "icu_or_non_invasive_vent", "invasive_vent"), ~ replace_na(., FALSE))) %>%
        mutate(month_year = floor_date(ymd_hm(discharge_date_time), unit = "month"))

    ## group by month-year, get sum of each grouping
    surge_table <- cohort[, .(no_icu_count = sum(no_icu_vent), icu_vent_count = sum(icu_or_non_invasive_vent), invasive_vent_count = sum(invasive_vent)), by = c("hospital_id", "month_year")]

    ## calculate numerator using above
    surge_table[, surge_index_num := 10 * (no_icu_count + 2 * (icu_vent_count) + 5 * (invasive_vent_count))]

    ### now calculate the denominator using daily_census as approximation
    ### currently using 2019-12-31 as end baseline as there are covid diagnoses
    ### in Jan 2020
    # convert to factor for merging
    cohort$hospital_num <- as.factor(cohort$hospital_num)

    ### now pull census data, take most recent data
    daily_census_data <- quiet(Rgemini::daily_census(cohort, time_period = c("2015-04-01", "2019-12-31"), capacity_func = "max")) %>%
        group_by(hospital_num) %>%
        slice_max(date_time) %>%
        data.table()
    
    ## add hospital_id back in
    daily_census_data <- merge(daily_census_data, unique(cohort[, .(hospital_num, hospital_id)]), by = "hospital_num")

    ### pull max bed #
    # since capacity_ratio = census/measure
    # max_beds = census/capacity_ratio
    daily_census_data[, max_beds := round(census/capacity_ratio)]

    ## add denominator to surge_table
    surge_table <- merge(surge_table, daily_census_data[, .(hospital_id, max_beds)], by = "hospital_id")

    ## calculate surge index
    surge_table[, surge_index := surge_index_num/max_beds]

    ## select hospital_id, month_year, numerator
    return(surge_table[, .(hospital_id, month_year, surge_index)][order(hospital_id, month_year)])
}
