#' @title
#' Compute hospital Length of Stay  (LoS)
#'
#' @description
#' `length_of_stay` returns Length of Stay for hospital admissions in unit of
#' hours and days.
#'
#' @details
#' Length of Stay is defined as the duration of hospital in-patient stay,
#' thus, calculated as (`discharge_date_time - admission_date_time`).
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
#' @param ipadmdad (`data.frame` or `data.table`)\cr
#' Table with all relevant encounters of interest from DRM table "ipadmdad" (see
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#' Must contain three fields: encounter ID (`genc_id`), and two date-time
#' variables corresponding to admission and discharge (typically
#' `admission_date_time` and `discharge_date_time`). Date-time variables must be
#'  in "yyyy-mm-dd hh:mm" format.
#'
#' @param adm_dtvar (`character`)\cr
#' Character string defining the column name for admission date-time (typically
#' "admission_date_time").
#'
#' @param dis_dtvar (`character`)\cr
#' Character string defining the column name for discharge date-time (typically
#' "discharge_date_time").
#'
#' @return
#' data.table with the same number of rows as input "ipadmdad", with additional
#' derived numeric fields labelled as "los_hrs_derived" and "los_days_derived".
#'
#' @export
length_of_stay <- function(ipadmdad,
                           adm_dtvar = "admission_date_time",
                           dis_dtvar = "discharge_date_time") {
  ## remap variable names in case field names change in the database/users want
  # to use other date-time variables (e.g., entry into palliative care)
  ipadmdad <- coerce_to_datatable(ipadmdad)
  res <- ipadmdad[, .(
    genc_id,
    adm_dtvar = get(adm_dtvar),
    dis_dtvar = get(dis_dtvar)
  )]

  res[, ":="(los_hrs_derived = as.numeric(difftime(ymd_hm(dis_dtvar),
    ymd_hm(adm_dtvar),
    units = "hours"
  )),
  los_days_derived = as.numeric(difftime(ymd_hm(dis_dtvar),
    ymd_hm(adm_dtvar),
    units = "days"
  )),
  adm_dtvar = NULL,
  dis_dtvar = NULL)][]

  return(res)
}
