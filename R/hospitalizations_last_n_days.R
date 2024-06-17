#' @title Compute number of previous hospitalizations for an encounter in a
#'   given window.
#'
#' @description Computes the number of hospitalizations an encounter has had
#'   over a specified time period, using admission & discharge date-times from
#'   the CIHI Discharge Abstract Database (DAD).
#'
#' @details This function uses DAD fields Admission date-time (Group 04 Fields
#'   01/02), Discharge date-time (Group 05 Fields 01/02) along with
#'   patient_id_hashed to calculate the number of previous hospitalizations an
#'   encounter has had in a specified window.
#'
#' @param cohort (`data.table`, `data.frame`)\cr Cohort table with all relevant
#'   encounters of interest, where each row corresponds to a single encounter.
#'   Must contain the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `patient_id_hashed` (`Varchar`): Unique hash of patient's health card number
#' - `admission_date_time` (`character`): The date and time that the patient was officially registered as an inpatient for the current hospital admission. Must be in YYYY-MM-DD HH:MM.
#' - `discharge_date_time` (`character`): The date and time when the patient was formally discharged from the current hospital admission. Must be in YYYY-MM-DD HH:MM.
#'
#'
#' @param n_days (`integer`)\cr Window for calculating the number of
#'   hospitalizations. n_days is set to 30 days by default.
#'
#' @param admit_dt (`character`)\cr Admission date-time Variable representing
#'   admission date-time. Set to admission_date_time by default.
#'
#' @param discharge_dt Discharge date-time Variable representing discharge
#'   date-time. Set to discharge_date_time by default.
#'
#' @return (`data.table`)\cr data.table containing the same number of rows as
#'   input `cohort` table. This data.table contains the genc_id, along with
#'   `n_hospitalizations`, which represents the number of previous
#'   hospitalizations in the given window. The values in `n_hospitalizations`
#'   will be 0, 1, or 2+.
#'
#' @export
#'
#' @examples
#' # Default n_days of 30:
#' \dontrun{
#' hospitalizations_last_n_days(cohort)
#' }
#'
#' # User-input window of 182 days (6 months)
#' \dontrun{
#' hospitalizations_last_n_days(cohort, n_days = 182)
#' }
#'
#'
hospitalizations_last_n_days <- function(cohort,
                                         n_days = 30,
                                         admit_dt="admission_date_time",
                                         discharge_dt="discharge_date_time") {

  cohort <- coerce_to_datatable(cohort)

  # check cohort for correct variables
  if (any(!c("genc_id", "patient_id_hashed") %in% names(cohort))) {
    stop("Input cohort table is missing at least one of the required variables.
          Refer to function documentation for details.")
  }

  # confirm that both date-time variables are included in the input cohort
  if(any(!c(admit_dt, discharge_dt) %in% names(cohort))){
    stop("Input cohort table is missing at least one date variable.\n  Please confirm that the variables are in the table and are correctly named.")
  }

  # confirm that admission date-time & discharge date-time are in proper format
  if(any(suppressWarnings(is.na(ymd_hm(cohort[, get(admit_dt)]))))){
    stop("One or more admission_date_time values are not in correct format (YYYY-MM-DD HH:MM). Please confirm that they are correctly formatted.")
  }

  if(any(suppressWarnings(is.na(ymd_hm(cohort[, get(discharge_dt)]))))){
    stop("One or more discharge_date_time values are not in correct format (YYYY-MM-DD HH:MM). Please confirm that they are correctly formatted.")
  }


  # select columns of interest from cohort
  dat <- cohort[, .(genc_id, patient_id_hashed, admission_date_time = get(admit_dt), discharge_date_time = get(discharge_dt))]

  # if patient_id_hashed is missing, set to NA
  dat[, patient_id_hashed := ifelse(is.na(patient_id_hashed) | patient_id_hashed =="", NA, patient_id_hashed)]

  # convert admission and discharge date-times to dates
  dat[, ":=" (admission_date_time = ymd_hm(admission_date_time), discharge_date_time = ymd_hm(discharge_date_time))]

  # group by patient_id_hashed, order by admission date-time and discharge date-time
  dat <- dat %>%
    dplyr::group_by(patient_id_hashed) %>%
    dplyr::arrange(desc(admission_date_time), desc(discharge_date_time)) %>%
    dplyr::ungroup() %>%
    data.table()

  # Determine the last 2 discharge date-times (if applicable) for each genc_id, using patient_id_hashed
  dat[, ":=" (prev_disch1 = shift(discharge_date_time, type = "lead", n = 1),
              prev_disch2 = shift(discharge_date_time, type = "lead", n = 2)), by=patient_id_hashed]

  # Find the difference in time between the encounter's admission date-time and previous 2 discharge date-times.
  # then divide the number of days by 365.25 (one year)
  dat[, ":=" (td1 = as.numeric(difftime(admission_date_time, prev_disch1, units="days")/365.25),
              td2 = as.numeric(difftime(admission_date_time, prev_disch2, units="days")/365.25))]

  # Determine the number of hospitalizations within the window. If window/365 is less than difference between the admission date-time
  # and 2 discharges ago: 2+, if window/365 is less than the difference between admission date-time and previous discharge: 1, else: 0.
  dat[, n_hospitalizations := dplyr::case_when(td2 <= n_days/365 ~ "2+",
                                             td1 <= n_days/365 ~ "1", is.na(td1) | td1 > n_days/365 ~ "0")] %>%
    .[is.na(patient_id_hashed), n_hospitalizations := NA]

  # select genc_id and n_hospitalizations
  res <- dat[, c("genc_id", "n_hospitalizations")]


  return(res)
}
