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
#' NA values in returned vector indicates either missing date or inputs that
#' cannot be converted into dates.
#'
#' @param date_ymd (`character` | `POSIXct POSIXt`)\cr
#' A vector of dates or date-times, typically in `yyyy-mm-dd` or
#' `yyyy-mm-dd hh:mm` format, but other formats are acceptable as long as they
#' can be interpreted by `lubridate::year()` and `lubridate::month()`.
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
#' @export
hospital_fiscal_year <- function(date_ymd) {
  # obtain year from date vector
  yr <- lubridate::year(date_ymd)

  # based on month in date, assign fiscal year
  # (month < 4 belongs to previous fiscal year)
  ifelse(lubridate::month(date_ymd) < 4,
    yr - 1, yr
  )
}
