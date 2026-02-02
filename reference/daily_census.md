# Compute daily census

Calculates the daily number of patients that were hospitalized at each
site during the specified time period. The daily census is defined as a
cross-sectional count of bed occupancy at a given time of the day (8am
by default). It is calculated as the number of patients who were
admitted before and discharged after that point in time (i.e., the
number of patients who occupied a bed at 8am on each day). Results are
returned as raw counts as well as a capacity ratio
(`= census/capacity`). The capacity ratio indicates whether the number
of hospitalized patients on a particular day was above (\>1) or below
(\<1) the typical occupancy (or max occupancy) during the time period of
interest. By default, capacity is estimated as `median(census)` but
users can specify other measures of typical/max capacity (see
`capacity_func` input). Note that if cohorts are filtered/grouped based
on patient characteristics (e.g., diagnosis, age etc.), this indicator
simply reflects a standardized measure of patient counts, rather than a
measure of true capacity limitations of medical wards.

## Usage

``` r
daily_census(
  cohort,
  time_period = NULL,
  scu_exclude = NULL,
  group_var = NULL,
  capacity_func = "median",
  buffer = 30,
  time_of_day = "08:00:00",
  include_zero = TRUE
)
```

## Arguments

- cohort:

  (`data.frame` or `data.table`) Cohort table with all relevant
  encounters of interest, where each row corresponds to a single
  encounter. Must contain the following columns:

  - `genc_id` (`integer`): GEMINI encounter ID

  - `hospital_num` or `hospital_id` (`integer` or `character`): Hospital
    ID

  - `admission_date_time` (`character` \| `POSIXct POSIXt`): Date-time
    of admission in YYYY-MM-DD HH:MM format

  - `discharge_date_time` (`character` \| `POSIXct POSIXt`): Date-time
    of discharge in YYYY-MM-DD HH:MM format

  If a `group_var` is specified (see below), this should be included in
  the `cohort` table as well.

- time_period:

  (`character`) The start and end points of the study period, e.g.,
  `c('2019-01-01', '2019-12-31')`. Should be specified in valid date
  format (e.g., `'yyyy-mm-dd'`).

- scu_exclude:

  (`data.frame` \| `data.table`) Optional table containing special care
  unit (SCU) encounters. This is only required if patients who were in
  an SCU should be excluded from the census calculation during time
  periods where they were in the SCU. The `scu_exclude` table typically
  refers to the `ipscu` table (see [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/)). However,
  users may want to filter the `ipscu` table by specific SCU unit
  numbers that should be excluded from the census calculation. Entries
  where `scu_unit_number = 99` (`"No SCU"`) are automatically removed by
  this function.

  The `scu_exclude` input table needs to contain the following columns:

  - `genc_id` (`integer`): GEMINI encounter ID

  - `scu_admit_date_time` (`character` \| `POSIXct POSIXt`): Date-time
    of SCU admission in YYYY-MM-DD HH:MM format

  - `scu_discharge_date_time` (`character` \| `POSIXct POSIXt`):
    Date-time SCU of discharge in YYYY-MM-DD HH:MM format

  For all entries in the `scu_exclude` table that have a valid
  `scu_admit_date_time` and `scu_discharge_date_time`, the encounter
  will not be counted towards the census during any time periods where
  they were in the SCU (i.e., time periods where they did not occupy a
  bed in a non-SCU ward). If no input is provided for this argument, no
  exclusion will be applied.

- group_var:

  (`character`) Optional input specifying one (or multiple) grouping
  variables. Census numbers and capacity ratio will be calculated
  separately for each level of a given grouping variable. Note: Users do
  not need to specify hospital as a grouping variable since the function
  automatically groups the output by `hospital_num`. Therefore,
  `group_var` should only be specified if additional grouping (e.g., by
  different medical subservices, physicians etc.) is required. Multiple
  grouping variables can be provided as a character vector (e.g.,
  `group_var = c("medical_subservice","physician")`). If no grouping
  variable is specified (default: `group_var = NA`), the function
  returns the total daily census numbers and capacity ratios per
  hospital.

- capacity_func:

  (`character`) Optional input specifying the function to be used to
  define capacity for the `capacity_ratio` output. Can either be a
  measure of central tendency to obtain typical occupancy (`median`,
  `mean`, or `mode`) or `max` to obtain maximum occupancy. The
  `capacity_ratio` is calculated as `census/capacity_func(census)`.
  Default is median (i.e., `capacity_ratio = census/median(census)`). If
  `"mode"` is selected and `census` has multiple modes, the median of
  all modes is used to calculate the capacity ratio.

  Note that capacity is calculated separately for each site (and
  grouping variable, if any). Other types of measures (e.g., trimmed
  means), or year-over-year capacity ratios could be calculated based on
  the raw census counts in the output (see vignette for more details).

- buffer:

  (`integer`) Buffer period (in days) to be applied at the end of the
  data availability period. Default is 30 days, i.e., census counts (and
  capacity ratio) are set to `NA` for the last 30 days of available data
  for each hospital. This is only relevant if the time period of
  interest is towards the end of the data availability period for a
  given hospital. For example, if
  `time_period = c("2020-01-01","2021-12-31")`, and data are only
  available until Dec 31 2021 for hospital A, census counts will drop
  sharply during the last few days in Dec 2021 due to a "truncation
  bias". Specifically, in the example scenario, there are encounters at
  hospital A who were admitted prior to Dec 31 2021, but have yet to be
  discharged. Due to the fact that encounters only appear in the GEMINI
  database once they have been discharged, the census counts in late
  December will decline abruptly at hospital A because patients who have
  not been discharged yet are not included in the database. To prevent
  this decrease in patient numbers from affecting census counts &
  capacity ratio, outputs will be set to `NA` for the last `30` days at
  hospital A.

  The default buffer period is set to `30` because the vast majority of
  patients is discharged within 30 days. That is, a 30-day buffer period
  ensures that most admitted patients have already been discharged, and
  thus, will be included in our data. However, users may specify a
  different buffer period since length of stays can vary widely between
  cohorts. Therefore, users should explore which buffer period makes
  most sense for the data they work with.

  Note that data availability differs across hospitals. Therefore, the
  actual buffer that is applied is calculated separately for each
  hospital. If a hospital's data availability ends prior to the end of
  the time period of interest, the buffer includes all last X days of
  that hospital's data (e.g., 30 days by default). If a hospital's data
  availability goes past the end of the time period of interest, the
  actual buffer that is applied is based on the difference between the
  specified buffer and the latest available discharge date at that
  hospital. For example, if `buffer = 30` and a given hospital has more
  than 30 extra days of data past the time period of interest, no
  additional buffer is applied.

- time_of_day:

  (`character` \| `numeric`) Optional input specifying the time of the
  day the census/capacity ratio should be calculated at. Default is 8am
  (`time_of_day = "8:00:00"`). Other times can be specified as a
  character (e.g., `"10:30"` for 10.30am) or as numeric input (e.g.,
  `14` for 2pm).

- include_zero:

  (`logical`) Flag indicating whether days with census counts of `0`
  should be treated as real 0s or whether they should be returned as
  `census = NA`. Note that this is only relevant for edge cases where
  this function is applied to small cohorts or if users specify a
  `group_var` with small samples resulting in counts of 0 on some days.
  In those cases, the decision on whether or not to include 0s can
  affect the estimated typical capacity (and thus, the `capacity_ratio`
  output).

  By default (`include_zero = TRUE`), the function will return days
  where no `genc_ids` were hospitalized as `census = 0`. This is the
  desired behavior in cases where counts of 0 patients are conceptually
  meaningful. For example, when users want to analyze how many patients
  with a rare condition are in hospital on a typical day, days where
  `census = 0` represent true counts of 0 patients (assuming the cohort
  provides sufficient coverage of all hospitalized patients). Note: If
  data availability periods differ between hospitals, `census` counts
  will only be set to 0 for dates that fall within the min-max date
  range for that particular site.

  In other scenarios, users may want to disregard days where
  `census = 0` by specifying `include_zero = FALSE`. For example, when
  looking at daily census counts per physician, days where `census = 0`
  likely reflect days where a given physician was not on service.
  Therefore, those days should not be included in the estimate of
  physicians' typical patient volume.

## Value

(`data.table`)  
data.table with the daily counts of hospitalized patients (`census`) at
each hospital. Additionally, the `capacity_ratio` (`census` relative to
the typical occupancy during the time period of interest) will be
returned. Dates within the time period of interest where no data were
available at a given site are not included in the output. For any dates
inside the buffer period, `census` and `capacity_ratio` are returned as
`NA`.

## See also

[`vignette("daily_census", package = "Rgemini")`](https://gemini-medicine.github.io/Rgemini/articles/daily_census.md)

## Examples

``` r
## calculate census of all in-patient admissions (ipadm):
if (FALSE) { # \dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)

ipadm <- dbGetQuery(dbcon, "select * from admdad") %>% data.table()

ipadm_census <- daily_census(ipadm)
} # }
```
