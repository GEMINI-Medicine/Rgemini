# Convert date-time variables into POSIXct/POSIXt format.

This function converts date-time variables into a user-specified format.
`convert_dt` is called by other `Rgemini` functions to make sure that
any date-time inputs are in the expected order (typically `"ymd HM"`).
It can also be used independently of other `Rgemini` functions to clean
up date-times for analyses. The function additionally returns warning
messages about missing/invalid date-time entries that can be useful for
debugging.

## Usage

``` r
convert_dt(
  dt_var,
  orders = "ymd HM",
  check_ts_zeros = FALSE,
  dt_varname = NULL,
  addtl_msg = NULL,
  ...
)
```

## Arguments

- dt_var:

    
  A vector containing the date-time values to be converted to the
  specified format. The vector can be of any class that can be converted
  to date-times by
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html)
  (usually "character" or "POSIXct"). If users have already
  pre-processed date-time variables into POSIXct (e.g., using
  [`lubridate::ymd_hm()`](https://lubridate.tidyverse.org/reference/ymd_hms.html))
  prior to running the function, `convert_dt()` will simply return the
  original `dt_var`.

- orders:

  (`character`)  
  Order(s) to be used to parse `dt_var`, such as `"ymd HM"` (default),
  `"ymd HMS"`, `"ymd"` etc. Will be passed to
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html),
  which accepts date-time formats without expecting "%" prefix (see
  `orders` input argument in
  [parse_date_time](https://lubridate.tidyverse.org/reference/parse_date_time.html).

  Multiple acceptable formats can be specified by providing a character
  vector, e.g. `format = c("ymd HM", "ymd HMS", "ymd")`.

  If expecting a date format in "ymd" (regardless of whether timestamp
  exists or not), use
  `convert_dt(dt_var, orders = "ymdHMS", truncated = 3)`.

- check_ts_zeros:

  (`logical`)  
  Whether to check for timestamps "00:00", which may represent missing
  times for some variables. This is only user species any `orders`
  containing time information, e.g., `orders = c("ymd HM", "ymd HMS")`.

- dt_varname:

  (`character`)  
  Name of variable referring to user-provided date-time variable. This
  is only used for warning messages to improve clarity. Usually,
  `dt_varname` can be inferred directly based on the provided input, but
  in some cases where inputs are passed between multiple functions, it
  can be helpful to specify the variable name explicitly (for example,
  see
  [`stat_holidays_ON()`](https://gemini-medicine.github.io/Rgemini/reference/stat_holidays_ON.md).

- addtl_msg:

  (`character`)  
  Additional warning message to be shown if any missing/invalid
  date-time entries were found. If `addtl_msg = NULL`, the following
  message will be shown by default: "Please carefully consider how to
  deal with missing/invalid date-time entries and perform any additional
  pre-processing prior to running the function \[function_name\] (e.g.,
  impute missing dates/timestamps etc.)."

- ...:

  Additional parameters passed to
  [`lubridate::parse_date_time()`](https://lubridate.tidyverse.org/reference/parse_date_time.html),
  such as `truncated` , `exact`, etc.

## Value

(`POSIXct` \| `POSIXt`)  
Returns converted `dt_var` parsed according to specified date-time
`orders`.

Will also return the following warning messages (if applicable):

1.  Number (%) of entries in `dt_var` that are missing (NA, "", " ")

2.  Number (%) of entries in `dt_var` that could not be parsed into
    specified format

3.  For formats containing (non-optional) timestamps: Number (%) of
    entries in `dt_var` that only contain date information

Any missing/invalid date-time entries will be returned as `NA` by this
function.

## Examples

``` r
my_date_time <- c("2020-01-01 12:00", "2021-03-06 09:25")
convert_dt(my_date_time, orders = "ymd HM")
#> [1] "2020-01-01 12:00:00 UTC" "2021-03-06 09:25:00 UTC"
```
