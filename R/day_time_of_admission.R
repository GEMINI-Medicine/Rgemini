#' @title
#' Compute day & time of hospital admissions
#'
#' @description
#' `day_time_of_admission` returns whether the patient was admitted during
#' day/night time and weekday/weekend.
#'
#' @details
#' Day time is defined between 08:00 ~ 16:59, and 17:00 ~ 07:59 for Night time.
#' Weekday is Monday ~ Friday, weekend is Sunday and Saturday.
#'
#' This classification might be useful as baseline admission characteristic in
#' Epidemiological studies.
#'
#' @section Warning:
#' NA values in returned data.table indicates either missing admission date and
#' time or its format is incorrect.
#'
#' @section Note:
#' The function takes an optional input specifying the field label for the
#' date-time variable of interest. By default, this is `admission_date_time` but
#' users could specify a different variable (e.g., `discharge_date_time`). This
#' input can also be used in case of differences in variable names between
#' databases.
#'
#' @param ipadmdad (`data.table` or `data.frame`)\cr
#' Table with all relevant encounters of interest from DRM table "ipadmdad" (see
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#' Must contain two fields: `genc_id` and a date-time variable (typically
#' `admission_date_time` in "yyyy-mm-dd hh:mm" format).
#'
#' @param dtvar (`character`)\cr
#' Character string defining the date-time variable of interest
#' (e.g., "admission_date_time").
#'
#' @return
#' data.table with the same number of rows as input "ipadmdad", with additional
#' derived character fields labelled as "day_of_admission_derived" and
#' "time_of_admission_derived". Possible values of these fields are "weekend" or
#' "weekday" for the former, "daytime" or "nighttime" for the latter.
#'
#' @export
day_time_of_admission <- function(ipadmdad,
                                  dtvar = "admission_date_time") {

  ## coerce data frame to data table if necessary
  ipadmdad <- coerce_to_datatable(ipadmdad)

  ## make sure dtvar is in appropriate format (ymd + HM/HMS timestamp)
  ipadmdad[, dtvar := convert_dt(
    get(dtvar), orders = c("ymd HM", "ymd HMS"), dt_varname = dtvar
  )]

  ## select relevant variables
  res <- ipadmdad[, .(genc_id, dtvar)]

  ## daytime = 08:00 to 16:59
  ## nighttime = 17:00 to 07:59
  res[, ":="(day_of_admission_derived =
    ifelse(lubridate::wday(dtvar,label = TRUE) %in% c("Sun", "Sat"),
      "weekend", "weekday"
    ),
  time_of_admission_derived =
    ifelse(lubridate::hour(dtvar) >= 8 & lubridate::hour(dtvar) < 17,
    "daytime", "nighttime"
    ),
  dtvar = NULL)][]

  return(res)
}
