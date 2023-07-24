#' @title
#' Compute hospital Length of Stay  (LoS)
#'
#' @description
#' `length_of_stay` returns Length of Stay for hospital admissions in unit of hours and days
#'
#' @details
#' Length of Stay is defined as the duration of hospital in-patient stay,
#' thus, calculated as (discharge date time - admission date time).
#' It is currently a major patient outcome in various research studies
#' and one of the physician performance indicators in MyPracticeReport.
#' Researchers might consider a few modifications to this metric.
#' One example will be adjusting for patient who receives palliative care.
#' If transfer to palliative unit within the hospital is trackable
#' (e.g. through room transfer data),
#' then it might be good idea to re-define discharge point to the palliative unit entrance.
#' This will reduce bias in metric since patient care under palliative order is very different.
#'
#' @section Warning:
#' NA values in returned data.table indicates either missing admission/discharge date and time or
#' its format is incorrect
#'
#' @section Note:
#' Function takes required fields labels as input, this was intended not to hardcode variable names
#' within the function. When field labels change in database, there is no need to go through the lines to change
#' the hardcoded variable names
#'
#' @param admdad (`data.table`)\cr
#' `admdad` table as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' admdad is a data.table object equivalent of DRM table "admdad".
#' Table must contain three fields:
#' admission id, admission date and time and discharge date time.
#'
#' @param idvar1 a character string of label for primary key of "admdad" table, ideally as admission id
#' @param admvar1 a character string of label equivalent to "admission date time".
#' Its value must be in "yyyy-mm-dd hh:mm" format
#' @param disvar1 a character string of label equivalent to "discharge date time".
#' Its value must be in "yyyy-mm-dd hh:mm" format
#'
#' @return
#' data.table with the same number of rows with input "admdad" with addtional
#' derived numeric fields labelled as "los_hrs_derived" and "los_days_derived".
#'
#' @export
length_of_stay <- function(admdad,
                           idvar1 = "genc_id",
                           admvar1 = "admission_date_time",
                           disvar1 = "discharge_date_time") {
  ## remap variable names in case field names change in the database
  admdad <- coerce_to_datatable(admdad)
  res <- admdad[, .(
    idvar1 = get(idvar1),
    admvar1 = get(admvar1),
    disvar1 = get(disvar1)
  )]

  res[, ":="(los_hrs_derived = as.numeric(difftime(lubridate::ymd_hm(disvar1),
    lubridate::ymd_hm(admvar1),
    units = "hours"
  )),
  los_days_derived = as.numeric(difftime(lubridate::ymd_hm(disvar1),
    lubridate::ymd_hm(admvar1),
    units = "days"
  )),
  admvar1 = NULL,
  disvar1 = NULL)][]

  # set back to initial id variable label
  data.table::setnames(res,
    old = "idvar1",
    new = idvar1
  )
  return(res)
}
