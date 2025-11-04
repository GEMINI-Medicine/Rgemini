#' @title
#' Derive COVID Surge
#'
#' @description
#' A function that derives the COVID-19 surge index for a given time period.
#' For time periods before 2020, or where COVID-19 was not yet diagnosed, the surge index will be 0.
#'
#' @param db (`DBIConnection`)\cr
#' RPostgres DB connection.
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
#' @import DBI RPostgreSQL dplyr tidyr
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom tidyr replace_na
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
#' gim_only = FALSE,
#' include_er = FALSE)
#'
#' }
#'
#' @references
#' The formula to derive the COVID surge index is from :
#' https://doi.org/10.1001/jamanetworkopen.2023.23035
#'

derive_covid_surge <- function(db, gim_only = FALSE, include_er = FALSE){
    # Surge index (per hospital-month) = ([(n without ICU, noninvasive positive pressure ventilation, or mechanical ventilation) + 2 × (n with noninvasive positive pressure ventilation or ICU) + 5 × (n with mechanical ventilation)] × 10) / (pre–COVID-19 baseline bed capacity of medical and ICU beds).

    # where n = # of COVID admissions

    ## invasive - 1GZ31CBND
    ## non-invasive - 1GZ31CAND
    ### pull icu encounters
    icu_encounters <- dbGetQuery(db, "select distinct icu.genc_id from ipscu icu join admdad a on icu.genc_id = a.genc_id where icu_flag = TRUE and a.age >= 18;") %>% data.table()

    ## account for include_er == TRUE
    if (include_er) {
        ## pull invasive ventilation encounters
        invasive_vent_encounters_ip <- dbGetQuery(db, "select distinct ip.genc_id from ipintervention ip join admdad a on ip.genc_id = a.genc_id where intervention_code = '1GZ31CBND' and a.age >= 18") %>% data.table()

        invasive_vent_encounters_er <- dbGetQuery(db, "select distinct er.genc_id from erintervention er join admdad a on er.genc_id = a.genc_id where intervention_code = '1GZ31CBND' and a.age >= 18") %>% data.table()

        invasive_vent_encounters <- c(invasive_vent_encounters_ip, invasive_vent_encounters_er) %>% unique()

        ## pull non-invasive ventilation encounters
        non_invasive_vent_encounters_ip <- dbGetQuery(db, "select distinct ip.genc_id from ipintervention ip join admdad a on ip.genc_id = a.genc_id where intervention_code = '1GZ31CAND' and a.age >= 18") %>% data.table()

        non_invasive_vent_encounters_er <- dbGetQuery(db, "select distinct er.genc_id from erintervention er join admdad a on er.genc_id = a.genc_id where intervention_code = '1GZ31CAND' and a.age >= 18") %>% data.table()

        non_invasive_vent_encounters <- c(non_invasive_vent_encounters_ip, non_invasive_vent_encounters_er) %>% unique()


        ## now pull covid encounters
        covid_encounters_ip <- dbGetQuery(db, "select distinct ip.genc_id, ip.hospital_id, a.discharge_date_time from ipdiagnosis ip join admdad a on ip.genc_id = a.genc_id where ip.diagnosis_code ~ '^U071' and a.age >= 18") %>%
            data.table()

        covid_encounters_ed <- dbGetQuery(db, "select distinct er.genc_id, er.hospital_id, a.discharge_date_time from erdiagnosis er join admdad a on er.genc_id = a.genc_id where er.er_diagnosis_code ~ '^U071' and a.age >= 18") %>%
            data.table()

        covid_encounters <- rbind(covid_encounters_ip, covid_encounters_ed) %>% unique()
    } else {
        ## pull invasive ventilation encounters
        invasive_vent_encounters <- dbGetQuery(db, "select distinct ip.genc_id from ipintervention ip join admdad a on ip.genc_id = a.genc_id where intervention_code = '1GZ31CBND' and a.age >= 18") %>% data.table()

        ## pull non-invasive ventilation encounters
        non_invasive_vent_encounters <- dbGetQuery(db, "select distinct ip.genc_id from ipintervention ip join admdad a on ip.genc_id = a.genc_id where intervention_code = '1GZ31CAND' and a.age >= 18") %>% data.table()

        ## now pull covid encounters
        covid_encounters <- dbGetQuery(db, "select distinct ip.genc_id, ip.hospital_id, a.discharge_date_time from ipdiagnosis ip join admdad a on ip.genc_id = a.genc_id where ip.diagnosis_code ~ '^U071' and a.age >= 18") %>%
            data.table()
    }

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
    cohort <- dbGetQuery(db, "select a.genc_id, l.hospital_num, a.hospital_id, a.admission_date_time, a.discharge_date_time from admdad a join lookup_hospital l on a.hospital_id = l.hospital_id where a.age >= 18") %>%
        data.table()

    ## merge in surge table data, replace NAs in non-COVID encounters
    ## create month_year for aggregating
    if (gim_only == TRUE) {
        ## pull GIM encounters
        gims <- dbGetQuery(db, "select d.genc_id, hospital_id from derived_variables d join admdad a on d.genc_id = a.genc_id where gim = 't' and a.age >= 18;") %>% data.table()

        cohort <- cohort %>%
            merge(covid_encounters, all.x = TRUE) %>%
            mutate(across(c("no_icu_vent", "icu_or_non_invasive_vent", "invasive_vent"), ~ replace_na(., FALSE))) %>%
            mutate(month_year = floor_date(ymd_hm(discharge_date_time), unit = "month")) %>%
            filter(genc_id %in% gims$genc_id) %>%
            data.table()
    } else {
        cohort <- cohort %>%
            merge(covid_encounters, all.x = TRUE) %>%
            mutate(across(c("no_icu_vent", "icu_or_non_invasive_vent", "invasive_vent"), ~ replace_na(., FALSE))) %>%
            mutate(month_year = floor_date(ymd_hm(discharge_date_time), unit = "month"))
    }

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
    ## semi hard-code start and end dates
    start_date <- substr(min(cohort$discharge_date_time), 1, 10)
    end_date <- substr(max(cohort$discharge_date_time), 1, 10)

    ### pull max bed #, get median by site
    # since capacity_ratio = census/measure
    # max_beds = census/capacity_ratio
    daily_census_data <- quiet(Rgemini::daily_census(cohort, time_period = c(start_date, end_date), capacity_func = "max")) %>%
        filter(!is.na(capacity_ratio)) %>%
        mutate(max_beds = census/capacity_ratio) %>% 
        group_by(hospital_num) %>%
        summarise(max_beds = median(max_beds, na.rm = TRUE)) %>% 
        data.table()
    
    ## add hospital_id back in
    daily_census_data <- merge(daily_census_data, unique(cohort[, .(hospital_num, hospital_id)]), by = "hospital_num")

    ## add denominator to surge_table
    surge_table <- merge(surge_table, daily_census_data[, .(hospital_id, max_beds)], by = "hospital_id")

    ## calculate surge index
    surge_table[, surge_index := surge_index_num/max_beds]

    ## select hospital_id, month_year, numerator
    return(surge_table[, .(hospital_id, month_year, surge_index)][order(hospital_id, month_year)])
}
