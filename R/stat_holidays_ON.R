#' @title
#' Flag statuatory holidays in Ontario
#'
#' @description
#' This function merges a given data frame with a holiday table to flag which dates are holidays/observed holidays
#'
#' @param data data frame containing date column in format YYYY-MM-DD
#' @param date_column date column name to merge with holiday table
#' @param include_observed_holidays includes observed holidays in holiday table when TRUE
#'
#' @import timeDate stringi
#' @importFrom stringr str_replace_all
#'
#' @return
#' This function returns a given data frame with additional columns as fields
#'    holiday_day: the day of the week when the holiday occurred
#'    holiday_name: the name of the holiday occuring on the given date
#'
#' @export

stat_holidays_ON <- function(data, date_column, include_observed_holidays = T){

  years <- 2000:2100
  count <- length(years)
  holidays <- c("NewYearsDay", "FamilyDay", "GoodFriday", "EasterMonday", "CAVictoriaDay", "CACanadaDay", "CACivicProvincialHoliday","CALabourDay", "CAThanksgivingDay", "CaRemembranceDay", "ChristmasDay", "BoxingDay")


  output <- c()
  for (i in 1:length(holidays)) {
    append <- rep(holidays[i], count)
    output <- c(output, append)
  }

  tS = timeSequence(from = paste0(years[1],"-01-01"), to = paste0(years[length(years)], "-12-31"), by = "month")

  # Family Day is on the 3rd Monday (nday = 1) in February
  family_day <- as.character(timeNthNdayInMonth(tS, nday = 1, nth = 3, format = "%Y-%m-%d"))
  family_day <- family_day[stri_detect_fixed(family_day, '-02-')]
  family_weekday <- weekdays(family_day)

  date <- c(family_day, as.character(holiday(years, holidays[-2])))
  weekday <- c(family_weekday, weekdays(as.character(holiday(years, holidays[-2]))))
  holiday <- data.frame(date, weekday, output)
  holiday$output <- str_replace_all(holiday$output, c("NewYearsDay" = "New Years Day", "FamilyDay" = "Family Day", "GoodFriday" = "Good Friday", "EasterMonday" = "Easter Monday", "CAVictoriaDay" = "Victoria Day", "CACanadaDay" = "Canada Day", "CACivicProvincialHoliday" = "Civic Holiday",
                                    "CALabourDay" = "Labour Day", "CAThanksgivingDay" = "Thanksgiving Day", "CaRemembranceDay" = "Remembrance Day", "ChristmasDay" = "Christmas Day", "BoxingDay" = "Boxing Day"))

  ### OBSERVED HOLIDAYS ###
  observed_holiday <- holiday[holiday$weekday %in% c("Saturday", "Sunday"),]

  # If a holiday falls on a Saturday, the observed holiday is on the following Monday (+ 2 days)
  observed_holiday$observed_date[observed_holiday$weekday == "Saturday"] <- as.character(as.Date(observed_holiday$date[observed_holiday$weekday == "Saturday"]) + 2)

  # If Boxing Day falls on a Sunday, the observed holiday is the following Tuesday (+ 2 days)
  observed_holiday$observed_date[observed_holiday$output == "BoxingDay" & observed_holiday$weekday == "Sunday"] <- as.character(as.Date(observed_holiday$date[observed_holiday$output == "BoxingDay" & observed_holiday$weekday == "Sunday"]) + 2)

  # If a holiday (other than Boxing Day) falls on a Sunday, the observed holiday is on the following Monday (+ 1 day)
  observed_holiday$observed_date[!(observed_holiday$output == "BoxingDay") & observed_holiday$weekday == "Sunday"] <- as.character(as.Date(observed_holiday$date[!(observed_holiday$output == "BoxingDay") & observed_holiday$weekday == "Sunday"]) + 1)

  observed_holiday$observed_weekday[!is.na(observed_holiday$observed_date)] <- weekdays(observed_holiday$observed_date[!is.na(observed_holiday$observed_date)])
  observed_holiday$output <- paste(observed_holiday$output, "(Observed)")
  observed_holiday <- observed_holiday[,c(3:5)]
  colnames(observed_holiday) <- c("output", "date", "weekday")

  if (include_observed_holidays){
    all_holidays <- rbind(holiday, observed_holiday)
    }

  else {
    all_holidays <- holiday
    }

  all_holidays <- all_holidays[order(all_holidays$date),]
  colnames(all_holidays) <- c("holiday_date", "holiday_day", "holiday_name")

  # Merge holiday table with data input

  data <- as.data.frame(data)
  date_column <- date_column

  colnames(data)[colnames(data) == as.character(date_column)] <- "holiday_date"
  data <- left_join(data, all_holidays, by = "holiday_date")
  colnames(data)[colnames(data) == "holiday_date"] <- date_column

  if (nrow(data) == sum(is.na(data$holiday_day))) {
    print("Make sure your data frame has date format: YYYY-MM-YY (no HH:MM:SS)!")
  } else {
    return(data)
  }

}


