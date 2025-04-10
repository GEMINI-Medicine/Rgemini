% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stat_holidays_ON.R
\name{stat_holidays_ON}
\alias{stat_holidays_ON}
\title{Flag statutory holidays in Ontario}
\usage{
stat_holidays_ON(data, date_column, include_observed_holidays = TRUE)
}
\arguments{
\item{data}{(\code{data.frame} or \code{data.table})\cr
Table containing \code{date_column} that can be converted to \code{YYYY-MM-DD} format}

\item{date_column}{(\code{character})\cr
Name of column in \code{data} corresponding to relevant dates; will be merged with
holiday table.}

\item{include_observed_holidays}{(\code{logical})\cr
If \code{TRUE}, observed holidays will be included in output.}
}
\value{
This function returns the provided input table \code{data} with the following
additional columns:
\itemize{
\item \code{date}: \code{date_column} in \code{YYY-MM-DD} format
\item \code{holiday}: flag indicating whether date corresponds to a holiday or not
\item \code{holiday_weekday}: the day of the week when the holiday occurred
\item \code{holiday_name}: the name of the holiday occurring on the given date
}

When \code{include_observed_holidays == TRUE}, 3 additional columns will be
returned where \code{observed_holiday = TRUE} for any dates on which a holiday
was observed (whether or not that corresponds to the actual date of the
holiday). Additionally, the corresponding weekday and holiday name of
observed holidays will be returned as separate columns.
}
\description{
This function merges a given table containing dates of interest
with a holiday table to flag which dates are holidays/observed holidays.
}
\examples{
\dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)

# derive which encounters were discharged on a holiday
ipadm <- dbGetQuery(db, "SELECT discharge_date_time FROM admdad;")
holidays <- stat_holidays_ON(ipadm, "discharge_date_time")
}

}
