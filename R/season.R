#' Get Season
#'
#' @description
#' Given a date, return the named season.
#'
#' @param date (`Date`)\cr
#'
#' @return (`character`)\cr
#' One of "Spring", "Summer", "Fall", "Winter".
#'
#' @importFrom lubridate month
#'
#' @export
#'
#' @examples
#' season(as.Date(Sys.Date()))
#'
season <- function(date) {
  m <- lubridate::month(date)

  ifelse(m %in% c(3, 4, 5), "Spring",
         ifelse(m %in% c(6, 7, 8), "Summer",
                ifelse(m %in% c(9, 10, 11), "Fall",
                       "Winter")))
}
