#' @title
#' Get hospital fiscal year
#'
#' @description
#' `hospital_fiscal_year` returns the corresponding hospital fiscal years for a
#' set of dates provided.
#'
#' @details
#' Hospital fiscal year is defined as April ~ March. For example, in case of
#' 2015-02-25, fiscal year will be 2014.
#'
#' @section Warning:
#' NA values in returned vector indicates either missing date or its format is
#' incorrect. All dates must be in `yyyy-mm-dd` format and precede the timestamp
#' (e.g., `yyy-mm-dd hh:mm`).
#'
#' @param date_ymd (`character`)\cr
#' A character vector of dates or date-times.
#' The date value must be in `yyyy-mm-dd` format.
#'
#' @return
#' Numeric vector with the same number of entries as input, containing the
#' derived fiscal year of all input dates (e.g., 2010, 2011, 2012, and so on).
#'
#' @examples
#' # Get fiscal year for `discharge_date_time` variable in `ipadmdad` table
#' \dontrun{
#' ipadmdad$fisc_year <- hospital_fiscal_year(ipadmdad$discharge_date_time)
#' }
#'
#' @importFrom stringr str_sub
#' @export
hospital_fiscal_year <- function(date_ymd) {

  # remove any leading/trailing " "
  date_ymd <- trimws(date_ymd)

  # obtain year from date vector
  yr <- as.numeric(str_sub(date_ymd, 1, 4))

  # based on month in date, assign fiscal year
  # (month < 4 belongs to previous fiscal year)
  ifelse(as.numeric(str_sub(date_ymd, 6, 7)) < 4,
         yr - 1, yr
  )
}
