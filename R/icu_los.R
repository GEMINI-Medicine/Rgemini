#' @title
#' Compute Length of Stay in Intensive Care Unit
#'
#' @description
#' Calculates the total duration in Intensive Care Unit (ICU) that an encounter spent
#' during a hospital stay using CIHI Discharge Abstract Database (DAD) fields
#'
#' @details
#' This function calculates the length of stay (LoS) in hours and days that
#' an encounter spent in the ICU during a hospital stay.
#'
#' It uses DAD fields SCU Admit date-time (Group 13, Field 03/04) and
#' SCU Discharge date-time (Group 13, Field 05/06) to derive these numeric fields.
#'
#' Rows with either of SCU Admit/Discharge date-time fields missing,
#' will not be counted in calculation.
#'
#' For encounters with multiple ICU visits, the function returns
#' the sum of the duration of each visit.
#'
#' By definition in DAD (Group 13, Field 02), SCUs include ICUs and Step-Down Units.
#' Step-Down Units are not considered as ICUs.
#'
#' Therefore, this function excludes below CIHI defined Step-Down Units numbers
#' from calculation:
#' \itemize{
#'  \item{90 : }{Step-Down Medical Unit}
#'  \item{93 : }{Combined Medical/Surgical Step-Down Unit}
#'  \item{95 : }{Step-Down Surgical Unit}
#'  \item{99 : }{No SCU}
#' }
#'
#' Please refer to the CIHI DAD abstracting manual for more details.
#'
#' @param cohort (`data.table` or `data.frame`)\cr
#' Table with all relevant encounters of interest, where each row corresponds to
#' a single encounter. Must contain GEMINI Encounter ID (`genc_id`).
#'
#' @param ipscu (`data.table` or `data.frame`)\cr
#' Table equivalent to the `ipscu` table defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Table must contain fields:
#' GEMINI Encounter ID (`genc_id`),
#' SCU admission time (`scu_admit_date_time` in "yyyy-mm-dd hh:mm" format),
#' SCU discharge time (`scu_discharge_date_time` in "yyyy-mm-dd hh:mm" format),
#' and SCU number (`scu_unit_number`).
#'
#' @return (`data.table`)\cr
#' By default, for each encounter in input "cohort" returns the corresponding
#' derived numeric fields "icu_los_hrs_derived" and "icu_los_days_derived".
#'
#' @note:
#' By design, function will not return any NA values.
#' Encounter IDs in the `cohort` table that are not present in the `ipscu` table are assumed to have no visits to ICU.
#' For these encounters, a value of 0 will be assigned to the derived fields.
#' When one tries to left-join the output of this function to another table, make sure the list of encounters aligns in both tables
#'
#' @export
#'
#' @examples
#' # Compute ICU LoS for all encounters in ipadmdad:
#' \dontrun{
#' icu_los (cohort=ipadmdad, ipscu=ipscu)
#' }

icu_los <- function(cohort, ipscu) {

  ###### Check user inputs ######
  ## table provided as data.frame/data.table
  if (!any(class(cohort) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for cohort. Please provide a data.frame or a data.table.")}

  if (!any(class(ipscu) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for ipscu Please provide a data.frame or a data.table.")}

  ## table contains required fields
  if (any(!"genc_id" %in% names(cohort))) {
    stop("Input cohort is missing the required variable 'genc_id'.
          Refer to function documentation for details.")}

  if (any(!c("genc_id", "scu_admit_date_time", "scu_discharge_date_time", "scu_unit_number") %in% names(ipscu))) {
    stop("Input ipscu is missing at least one of the required variables.
          Refer to function documentation for details.")}

  ###### Prepare data ######
  ## coerce tables to data.table as function uses data.table syntax
  cohort <- coerce_to_datatable(cohort)
  ipscu <- coerce_to_datatable(ipscu)

  ## prepare data and filter out step-down units from ipscu
  ipscu <- ipscu %>%
    .[, .(genc_id, scu_admit_date_time, scu_discharge_date_time, scu_unit_number)] %>%
    dplyr::mutate(across(where(is.character), na_if, "")) %>%
    .[!trimws(as.character(scu_unit_number)) %in% c("90", "93", "95", "99")] %>%
    .[, ":="(
      scu_admit_date_time = lubridate::ymd_hm(scu_admit_date_time),
      scu_discharge_date_time = lubridate::ymd_hm(scu_discharge_date_time)
    )] %>%
    .[!is.na(scu_admit_date_time) & !is.na(scu_discharge_date_time)]

  ###### Derive ICU length of stay fields ######
  ipscu <- ipscu %>%
    .[, ":="(
      icu_los_hr = as.numeric(difftime(scu_discharge_date_time, scu_admit_date_time, units = "hours")),
      icu_los_days = as.numeric(difftime(scu_discharge_date_time, scu_admit_date_time, units = "days"))
    )] %>%
    .[, .(
      icu_los_hrs_derived = sum(icu_los_hr),
      icu_los_days_derived = sum(icu_los_days)
    ), by = genc_id]

  ###### Merge results with cohort ######
  res <- cohort[, .(genc_id)] %>%
    dplyr::left_join(ipscu, by = "genc_id") %>%
    dplyr::mutate(across(starts_with("icu"), ~ ifelse(is.na(.), 0, .))) # assign 0 to encounters with no ICU visits

  return(res)
}
