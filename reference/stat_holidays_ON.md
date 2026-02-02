# Flag statutory holidays in Ontario

This function merges a given table containing dates of interest with a
holiday table to flag which dates are holidays/observed holidays.

## Usage

``` r
stat_holidays_ON(data, date_column, include_observed_holidays = TRUE)
```

## Arguments

- data:

  (`data.frame` or `data.table`)  
  Table containing `date_column` that can be converted to `YYYY-MM-DD`
  format

- date_column:

  (`character`)  
  Name of column in `data` corresponding to relevant dates; will be
  merged with holiday table.

- include_observed_holidays:

  (`logical`)  
  If `TRUE`, observed holidays will be included in output.

## Value

This function returns the provided input table `data` with the following
additional columns:

- `date`: `date_column` in `YYY-MM-DD` format

- `holiday`: flag indicating whether date corresponds to a holiday or
  not

- `holiday_weekday`: the day of the week when the holiday occurred

- `holiday_name`: the name of the holiday occurring on the given date

When `include_observed_holidays == TRUE`, 3 additional columns will be
returned where `observed_holiday = TRUE` for any dates on which a
holiday was observed (whether or not that corresponds to the actual date
of the holiday). Additionally, the corresponding weekday and holiday
name of observed holidays will be returned as separate columns.

## Examples

``` r
if (FALSE) { # \dontrun{
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
} # }
```
