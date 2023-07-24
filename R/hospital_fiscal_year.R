#' @title
#' Get hospital fiscal year
#'
#' @description
#' `hospital_fiscal_year` returns the corresponding hospital fiscal years for a set of dates provided.
#'
#' @details
#' Hospital fiscal year is defined as April ~ March. For example, in case of 2015-02-25, fiscal year will be 2014.
#'
#' @section Warning:
#' NA values in returned vector indicates either missing date or
#' its format is incorrect, as all dates must be in yyyy-mm-dd format and precede the timestamp.
#'
#' @param date_ymd a character vector of label of dates or date-times.
#' The date value must be in "yyyy-mm-dd" format
#'
#' @return
#' vector with the same number of entries with corresponding fiscal years
#' to input dates.
#' Possible values of this field is "2010", "2011", "2012" and so on.
#'
#' @importFrom stringr str_sub
#' @export
hospital_fiscal_year <- function(date_ymd) {
  # obtain year from date vector
  yr <- as.numeric(str_sub(date_ymd, 1, 4))

  # based on month in date, assign fiscal year (month < 4 belongs to previous fiscal year)
  ifelse(as.numeric(str_sub(date_ymd, 6, 7)) < 4,
    yr - 1, yr
  )
}
