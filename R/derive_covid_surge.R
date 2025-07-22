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
#' Data for hospital bed availability to be pulled from:
#' https://www.cihi.ca/sites/default/files/document/beds-staffed-and-in-operation-2018-2019-en-web.xlsx
#'

## to address:
### - er vs ip codes
### - adding in gim vs all-med functionality
### - factoring in baseline bed counts. Are paeds beds included?
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

    ### now that all genc_ids are pulled, can filter to make numerator
    ## without ventilation or ICU
    covid_encounters[, no_icu_vent := genc_id %ni% c(icu_encounters$genc_id, non_invasive_vent_encounters$genc_id, invasive_vent_encounters$genc_id)]

    ### non-invasive OR ICU
    covid_encounters[, icu_or_non_invasive_vent := genc_id %in% c(icu_encounters$genc_id, non_invasive_vent_encounters$genc_id)]

    ### invasive vent
    covid_encounters[, invasive_vent := genc_id %in% invasive_vent_encounters$genc_id]

    ### now make numerator
    ## make month-year
    covid_encounters[, month_year := substr(discharge_date_time, 1, 7)]

    ## group by month-year, get sum of each grouping
    ### 
    surge_table <- covid_encounters[, .(no_icu_count = sum(no_icu_vent), icu_vent_count = sum(icu_or_non_invasive_vent), invasive_vent_count = sum(invasive_vent)), by = c("hospital_id", "month_year")]

    ## calculate numerator using above
    surge_table[, surge_index_num := 10 * (no_icu_count + 2 * (icu_vent_count) + 5 * (invasive_vent_count))]

    ## select hospital_id, month_year, numerator
    return(surge_table[, .(hospital_id, month_year, surge_index_num)][order(hospital_id, month_year)])
}
