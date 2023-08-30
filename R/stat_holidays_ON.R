#' @title
#' Flag statutory holidays in Ontario
#'
#' @description
#' This function merges a given table containing dates of interest
#' with a holiday table to flag which dates are holidays/observed holidays.
#'
#' @param data (`data.frame` or `data.table`)\cr
#' Table containing `date_column` that can be converted to `YYYY-MM-DD` format
#' @param date_column (`character`)\cr
#' Name of column in `data` corresponding to relevant dates; will be merged with
#' holiday table.
#' @param include_observed_holidays (`logical`)\cr
#' If `TRUE`, observed holidays will be included in output.
#'
#' @import timeDate stringi
#' @importFrom stringr str_replace_all
#'
#' @return
#' This function returns the provided input table `data` with the following
#' additional columns:
#' - `date`: `date_column` in `YYY-MM-DD` format
#' - `holiday`: flag indicating whether date corresponds to a holiday or not
#' - `holiday_weekday`: the day of the week when the holiday occurred
#' - `holiday_name`: the name of the holiday occurring on the given date
#'
#' When `include_observed_holidays == TRUE`, 3 additional columns will be
#' returned where `observed_holiday = TRUE` for any dates on which a holiday
#' was observed (whether or not that corresponds to the actual date of the
#' holiday). Additionally, the corresponding weekday and holiday name of
#' observed holidays will be returned as separate columns.
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' db <- DBI::dbConnect(drv,
#'   dbname = "DB_name",
#'   host = "172.XX.XX.XXX",
#'   port = 1234,
#'   user = getPass("Enter user:"),
#'   password = getPass("Enter Password:")
#' )
#'
#' # derive which encounters were discharged on a holiday
#' admdad <- dbGetQuery(db, "SELECT discharge_date_time FROM admdad;")
#' holidays <- stat_holidays_ON(admdad, "discharge_date_time")
#' }
#'
#' @export

stat_holidays_ON <- function(data,
                             date_column,
                             include_observed_holidays = TRUE) {
  ## check inputs
  # make sure provided date column can be converted to YYYY-MM-DD format
  tryCatch(
    {
      data$date <- as.Date(data[[date_column]])
    },
    error = function(e) {
      stop("Invalid user input. Please provide dates in 'YYY-MM-DD' format.")
    }
  )
  
  years <- 2000:2100
  count <- length(years)
  
  ## list relevant holiday names as they are called in timeDate::holiday()
  # note: Family day is not included in package, is calculated separately below
  holiday_names <- c(
    "NewYearsDay", "GoodFriday", "EasterMonday",
    "CAVictoriaDay", "CACanadaDay", "CACivicProvincialHoliday",
    "CALabourDay", "CAThanksgivingDay", "CaRemembranceDay",
    "ChristmasDay", "BoxingDay"
  )
  
  # repeat each holiday entry for each year
  holidays_all <- rep(holiday_names, each = count)
  
  ts <- timeSequence(
    from = paste0(years[1], "-01-01"),
    to = paste0(years[length(years)], "-12-31"),
    by = "month"
  )
  
  ## Derive Family Day
  # Family Day is not included in holiday() function of timeDate package
  # calculate it separately here: is on the 3rd Monday (nday = 1) in February
  family_day <- as.character(timeNthNdayInMonth(ts,
                                                nday = 1, nth = 3,
                                                format = "%Y-%m-%d"
  ))
  family_day <- as.Date(family_day[stri_detect_fixed(family_day, "-02-")])
  family_weekday <- weekdays(family_day)
  
  ## Create data frame with all holidays across all years
  holiday <- data.table(date = c(
    family_day,
    as.Date(holiday(years, holiday_names))
  ))
  holiday$holiday <- TRUE
  holiday$holiday_name <- c(
    rep("Family Day", each = count),
    holidays_all
  )
  holiday$holiday_weekday <- weekdays(holiday$date)
  
  
  # rename holidays
  holiday$holiday_name <- str_replace_all(
    holiday$holiday_name, c(
      "NewYearsDay" = "New Years Day",
      "GoodFriday" = "Good Friday",
      "EasterMonday" = "Easter Monday",
      "CAVictoriaDay" = "Victoria Day",
      "CACanadaDay" = "Canada Day",
      "CACivicProvincialHoliday" = "Civic Holiday",
      "CALabourDay" = "Labour Day",
      "CAThanksgivingDay" = "Thanksgiving Day",
      "CaRemembranceDay" = "Remembrance Day",
      "ChristmasDay" = "Christmas Day",
      "BoxingDay" = "Boxing Day"
    )
  )
  
  # Merge holiday table with data input
  data <- merge(data, holiday, by = "date", all.x = TRUE) %>%
    data.table()
  
  # holiday flag should be either FALSE/TRUE
  data[holiday != TRUE | is.na(holiday), holiday := FALSE]
  
  ### OBSERVED HOLIDAYS ###
  # add as separate columns
  if (include_observed_holidays) {
    # If a holiday falls on a Saturday, the observed holiday is on the following
    # Monday (+ 2 days)
    holiday[holiday_weekday == "Saturday", observed_date := date + 2]
    
    # If Boxing Day falls on a Sunday, the observed holiday is the following
    # Tuesday (+ 2 days)
    holiday[
      holiday_name == "Boxing Day" & holiday_weekday == "Sunday",
      observed_date := date + 2
    ]
    
    # If a holiday falls on a Sunday, the observed holiday is on the following
    # Monday (+ 1 day), except for Boxing Day (is moved to Tuesday, see above)
    # and Christmas Day (doesn't move, because if it falls on a Sunday the
    # the Monday is already Boxing Day holiday -> ensures that the Monday
    # doesn't get assigned 2 different holidays)
    holiday[!holiday_name %in% c("Boxing Day", "Christmas Day") &
              holiday_weekday == "Sunday", observed_date := date + 1]
    
    # For all other holidays, observed date = actual holiday data
    holiday[is.na(observed_date), observed_date := date]
    
    # add observed weekday & flag
    holiday[, observed_holiday_weekday := weekdays(observed_date)]
    holiday[, observed_holiday := TRUE]
    
    setnames(holiday, "holiday_name", "observed_holiday_name")
    
    # Merge holiday table with data input by OBSERVED date
    data <- merge(data, holiday[, .(observed_date, observed_holiday, observed_holiday_name, observed_holiday_weekday)],
                  by.x = "date", by.y = "observed_date", all.x = TRUE
    ) %>%
      data.table()
    
    # observed_holiday flag should be either FALSE/TRUE
    data[observed_holiday != TRUE | is.na(observed_holiday), observed_holiday := FALSE]
  }
  
  
  return(data)
}
