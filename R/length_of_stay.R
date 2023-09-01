#' @title
#' Compute hospital Length of Stay  (LoS)
#'
#' @description
#' `length_of_stay` returns Length of Stay for hospital admissions in unit of
#' hours and days.
#'
#' @details
#' Length of Stay is defined as the duration of hospital in-patient stay,
#' thus, calculated as (discharge date time - admission date time).
#' It is currently a major patient outcome in various research studies
#' and one of the physician performance indicators in MyPracticeReport.
#' Researchers might consider a few modifications to this metric.
#' One example will be adjusting for patient who receives palliative care.
#' If transfer to palliative unit within the hospital is trackable
#' (e.g. through room transfer data), then it might be good idea to re-define
#' discharge point to the palliative unit entrance. This will reduce bias in
#' metric since patient care under palliative order is very different.
#'
#' @section Warning:
#' NA values in returned data.table indicates either missing admission/discharge
#' date and time or its format is incorrect.
#'
#' @section Note:
#' The function takes two optional input arguments defining the admission and
#' discharge date-time variables of interest (by default `admission_date_time`
#' and `discharge_date_time`).
#'
#' @param admdad (`data.table` or `data.frame`)\cr
#' Table equivalent to DRM table "ipadmdad" as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Table must contain three fields: encounter ID (`genc_id`), and two date-time
#' variables corresponding to admission and discharge (typically
#' `admission_date_time` and `discharge_date_time`).
#'
#' @param admvar1 (`character`)\cr
#' a character string equivalent to "admission date time".
#' Must be in "yyyy-mm-dd hh:mm" format.
#' @param disvar1 (`character`)\cr
#' a character string equivalent to "discharge date time".
#' Must be in "yyyy-mm-dd hh:mm" format.
#'
#'
#' @return
#' data.table with the same number of rows as input "admdad", with additional
#' derived numeric fields labelled as "los_hrs_derived" and "los_days_derived".
#'
#' @export
length_of_stay <- function(admdad,
                           admvar1 = "admission_date_time",
                           disvar1 = "discharge_date_time") {
  ## remap variable names in case field names change in the database/users want
  # to use other date-time variables (e.g., entry into palliative care)
  admdad <- coerce_to_datatable(admdad)
  res <- admdad[, .(
    genc_id,
    admvar1 = get(admvar1),
    disvar1 = get(disvar1)
  )]

  res[, ":="(los_hrs_derived = as.numeric(difftime(ymd_hm(disvar1),
    ymd_hm(admvar1),
    units = "hours"
  )),
  los_days_derived = as.numeric(difftime(ymd_hm(disvar1),
    ymd_hm(admvar1),
    units = "days"
  )),
  admvar1 = NULL,
  disvar1 = NULL)][]

  return(res)
}
