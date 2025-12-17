#' @title
#' Derive COVID Surge Index
#'
#' @description
#' A function that derives the COVID-19 surge index for a given time period.
#' For time periods before 2020, or where COVID-19 was not yet diagnosed, the surge index will be 0.
#' The function filters for the All-Medicine + ICU cohort. This includes any
#' encounter admitted/discharged from a medical service or encounters who
#' entered the ICU at any point (specialized or stepdown unit).
#'
#' The function needs to be run on the entire cohort to create accurate values.
#' If users have pre-filtered cohorts, please reach out to the GEMINI team to
#' derive the table.
#'
#' @details
#' The COVID surge index is a "severity-weighted measure of COVID-19
#' caseload relative to pre-COVID-19 bed capacity" (Kadri et al, 2021)
#' The index looks at overall COVID admissions, as well as COVID
#' admissions who entered the ICU and underwent mechanical ventilation.
#'
#' @param dbcon (`DBIConnection`)\cr
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
#' A data.table containing each hospital and the COVID surge index for the given
#' month year.
#'
#' @import DBI RPostgreSQL dplyr
#' @importFrom lubridate ymd_hm floor_date
#' @importFrom tidyr replace_na
#' @export
#'
#' @examples
#' \dontrun{
#' drv <- DBI::dbDriver("PostgreSQL")
#' db <- DBI::dbConnect(
#'   drv,
#'   dbname = "db_name",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass::getPass("Enter Username"),
#'   password = getPass::getPass("Enter Password")
#' )
#'
#' covid_surge <- derive_covid_surge(
#'   dbcon = db,
#'   gim_only = FALSE,
#'   include_er = FALSE
#' )
#' }
#'
#' @references
#'
#' Kadri S, et al. Annals of Internal Medicine, 2021.
#' https://doi.org/10.7326/m21-1213
#'
#' McAlister FA et al. JAMA Network Open, 2023.
#' https://doi.org/10.1001/jamanetworkopen.2023.23035

covid_surge_index <- function(dbcon, gim_only = FALSE, include_er = FALSE) {
  ### pull adult all-med + ICU encounters from 2019 onwards
  ## get admdad table name
  admdad_name <- find_db_tablename(dbcon, "admdad", verbose = FALSE)
  er_name <- find_db_tablename(dbcon, "er", verbose = FALSE)
  derived_variables_name <- find_db_tablename(dbcon, "derived_variables", verbose = FALSE)
  ipscu_name <- find_db_tablename(dbcon, "ipscu", verbose = FALSE)
  ipintervention_name <- find_db_tablename(dbcon, "ipintervention", verbose = FALSE)
  erintervention_name <- find_db_tablename(dbcon, "erintervention", verbose = FALSE)
  ipdiagnosis_name <- find_db_tablename(dbcon, "ipdiagnosis", verbose = FALSE)
  erdiagnosis_name <- find_db_tablename(dbcon, "erdiagnosis", verbose = FALSE)

  ## find which hospital identifier to use
  # to do minimial changes to querying one row to get all the column names instead
  admdad_cols <- dbGetQuery(dbcon, paste0("SELECT * from ", admdad_name, " limit 1;")) %>% data.table()

  admdad_cols <- data.table(column_name = colnames(admdad_cols))

  hospital_var <- admdad_cols[column_name %in%
    c("hospital_id", "hospital_num")]$column_name[1] # if multiple, use first identified variable

  ## filter for All Med + ICU patients, exclude paeds only sites
  cohort <- dbGetQuery(dbcon, paste0(
    "select genc_id, ", hospital_var, ", ", "
  admission_date_time, discharge_date_time from ",
    admdad_name, " where genc_id in ((select genc_id from ",
    derived_variables_name, " where all_med='t') union
  (select genc_id from ", ipscu_name, ")) and age >= 18
  and discharge_date_time >= '2019-01-01 00:00' and hospital_num != '134'"
  )) %>% data.table()

  ### pull icu encounters
  icu_encounters <- dbGetQuery(dbcon, paste0(
    "select distinct genc_id from ",
    derived_variables_name, " where icu_entry_derived = 't';"
  )) %>%
    data.table()

  ## account for include_er == TRUE
  if (include_er) {
    ## pull invasive ventilation encounters
    invasive_vent_encounters_ip <- dbGetQuery(dbcon, paste0("select distinct
    ip.genc_id from ", ipintervention_name, " ip join ", admdad_name, " a on
    ip.genc_id = a.genc_id where intervention_code = '1GZ31CBND'
    and a.age >= 18")) %>% data.table()

    invasive_vent_encounters_er <- dbGetQuery(dbcon, paste0("select distinct
    er.genc_id from ", erintervention_name, " er join ", admdad_name, " a on
    er.genc_id = a.genc_id where intervention_code = '1GZ31CBND'
    and a.age >= 18")) %>% data.table()

    invasive_vent_encounters <- unique(c(
      invasive_vent_encounters_ip,
      invasive_vent_encounters_er
    ))

    ## pull non-invasive ventilation encounters
    non_inv_vent_encounters_ip <- dbGetQuery(dbcon, paste0("select distinct
    ip.genc_id from ", ipintervention_name, " ip join ", admdad_name, " a on
    ip.genc_id = a.genc_id where intervention_code = '1GZ31CAND' and
    a.age >= 18")) %>% data.table()

    non_inv_vent_encounters_er <- dbGetQuery(dbcon, paste0("select distinct
    er.genc_id from ", erintervention_name, " er join ", admdad_name, " a on
    er.genc_id = a.genc_id where intervention_code = '1GZ31CAND' and
    a.age >= 18")) %>% data.table()

    non_invasive_vent_encounters <- unique(c(
      non_inv_vent_encounters_ip,
      non_inv_vent_encounters_er
    ))

    ## now pull covid encounters
    covid_encounters_ip <- dbGetQuery(dbcon, paste0("select distinct ip.genc_id,
    ip.", hospital_var, ", a.discharge_date_time from ", ipdiagnosis_name, " ip
    join ", admdad_name, " a on ip.genc_id = a.genc_id where
    ip.diagnosis_code ~ '^U071' and a.age >= 18")) %>%
      data.table()

    covid_encounters_ed <- dbGetQuery(dbcon, paste0("select distinct er.genc_id,
    er.", hospital_var, ", a.discharge_date_time from ", erdiagnosis_name, " er
    join ", admdad_name, " a on er.genc_id = a.genc_id where
    er.er_diagnosis_code ~ '^U071' and a.age >= 18")) %>%
      data.table()

    covid_encounters <- unique(rbind(
      covid_encounters_ip,
      covid_encounters_ed
    ))
  } else {
    ## pull invasive ventilation encounters
    invasive_vent_encounters <- dbGetQuery(dbcon, paste0("select distinct
    ip.genc_id from ", ipintervention_name, " ip join ", admdad_name, " a on
    ip.genc_id = a.genc_id where intervention_code = '1GZ31CBND' and
    a.age >= 18")) %>% data.table()

    ## pull non-invasive ventilation encounters
    non_invasive_vent_encounters <- dbGetQuery(dbcon, paste0("select distinct
    ip.genc_id from ", ipintervention_name, " ip join ", admdad_name, " a on
    ip.genc_id = a.genc_id where intervention_code = '1GZ31CAND' and
    a.age >= 18")) %>% data.table()

    ## now pull covid encounters
    covid_encounters <- dbGetQuery(dbcon, paste0("select distinct
    ip.genc_id, ip.", hospital_var, ", a.discharge_date_time from ", ipdiagnosis_name, " ip
    join ", admdad_name, " a on ip.genc_id = a.genc_id where
    ip.diagnosis_code ~ '^U071' and a.age >= 18")) %>%
      data.table()
  }

  ### now that all genc_ids are pulled, can make numerator
  ## encounters without ventilation or ICU
  covid_encounters[, no_icu_vent := genc_id %ni%
    c(
      icu_encounters$genc_id,
      non_invasive_vent_encounters$genc_id,
      invasive_vent_encounters$genc_id
    )]

  ### encounters with non-invasive OR ICU
  covid_encounters[, icu_or_non_invasive_vent := genc_id %in%
    c(
      icu_encounters$genc_id,
      non_invasive_vent_encounters$genc_id
    )]

  ### encounters with invasive vent
  covid_encounters[, invasive_vent := genc_id %in% invasive_vent_encounters$genc_id]

  ### now make numerator
  ## make month-year
  covid_encounters <- covid_encounters %>%
    mutate(month_year = floor_date(ymd_hm(discharge_date_time), unit = "month"))

  ## merge in surge table data, replace NAs in non-COVID encounters
  ## create month_year for aggregating
  if (gim_only == TRUE) {
    ## pull GIM encounters
    gims <- dbGetQuery(dbcon, paste0(
      "select d.genc_id, d.", hospital_var,
      " from ", derived_variables_name, " d join ", admdad_name, " a on
    d.genc_id = a.genc_id where gim = 't' and
    a.age >= 18;"
    )) %>% data.table()

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
      mutate(month_year = floor_date(ymd_hm(discharge_date_time), unit = "month")) %>%
      data.table()
  }

  ## group by month-year, get sum of each grouping
  surge_table <- cohort[, .(
    no_icu_count = sum(no_icu_vent),
    icu_vent_count = sum(icu_or_non_invasive_vent),
    invasive_vent_count = sum(invasive_vent)
  ), by = c(hospital_var, "month_year")]

  ## calculate numerator using above
  surge_table[, surge_index_num := 10 * (no_icu_count + 2 * (icu_vent_count) + 5 * (invasive_vent_count))]

  ### now calculate the denominator using daily_census as approximation
  ### currently using 2019-12-31 as end baseline as there are covid diagnoses
  ### in Jan 2020

  ### pull census data, get 95th percentile by site
  ## pull census
  census <- quiet(daily_census(cohort,
    time_period = c("2019-01-01 00:00", "2019-12-31 23:59")
  ))

  #### if gim only get 95th percentile as-is
  if (gim_only == TRUE) {
    daily_census_data <- census %>%
      group_by(!!rlang::sym(hospital_var)) %>%
      summarise(bed_estimate = quantile(census, probs = 0.95, na.rm = TRUE)) %>%
      data.table()
  } else { ## if non-gim, have to restrict 2 site's estimates to Nov-Dec
    ## get lookup hospital name
    lookup_hospital_name <- find_db_tablename(dbcon, "lookup_hospital")

    ## create query
    lookup_hospital_query <- paste0(
      "select * from ", lookup_hospital_name,
      " where hospital_num in (100, 101, 102, 105, 106)"
    )

    # query lookup hospital
    pulled <- dbGetQuery(dbcon, lookup_hospital_query)

    # filter for sites of interest
    special_sites <- pulled[, hospital_var]

    ## filter out sites of interest
    special_census <- census[get(hospital_var) %in% special_sites & date_time >= "2019-11-01 00:00"] %>%
      group_by(!!rlang::sym(hospital_var)) %>%
      summarise(bed_estimate = quantile(census, probs = 0.95, na.rm = TRUE)) %>%
      data.table()
    regular_census <- census[get(hospital_var) %ni% special_sites] %>%
      group_by(!!rlang::sym(hospital_var)) %>%
      summarise(bed_estimate = quantile(census, probs = 0.95, na.rm = TRUE)) %>%
      data.table()

    ## combine
    daily_census_data <- rbind(regular_census, special_census)
  }

  ## add denominator to surge_table
  filter_cols <- c(hospital_var, "bed_estimate")
  surge_table <- merge(surge_table, daily_census_data[, ..filter_cols], by = setNames(hospital_var, hospital_var))

  ## calculate surge index
  surge_table[, surge_index := surge_index_num / bed_estimate]

  ## select hospital identifier, month_year, numerator
  final_filter <- c(hospital_var, "month_year", "surge_index")
  return(surge_table[, ..final_filter][order(get(hospital_var), month_year)])
}
