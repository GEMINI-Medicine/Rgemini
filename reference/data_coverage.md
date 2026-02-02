# Check data coverage

This function facilitates data coverage checks that analysts should
perform during data exploration & cohort generation. Specifically, this
function checks for "gaps" in the data timeline (e.g., are there any
hospitals/time periods for which GEMINI did not receive any data at
all?), and also provides more detailed insights into data volume &
coverage by month and hospital (e.g., what percentage of encounters have
an entry in a given table?). All checks performed by this function are
applied at the table level (i.e., missing values in individual columns
are not considered and should be checked separately).

## Usage

``` r
data_coverage(
  dbcon,
  cohort,
  table,
  plot_timeline = TRUE,
  plot_coverage = TRUE,
  hospital_label = NULL,
  hospital_group = NULL,
  custom_dates = NULL,
  as_plotly = FALSE,
  ...
)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database.

- cohort:

  (`data.frame` or `data.table`) Cohort table with all relevant
  encounters of interest, where each row corresponds to a single
  encounter. Must contain the following columns:

  - `genc_id`: GEMINI Encounter ID

  - `hospital_num` \| `hospital_id`: Hospital identifier

  - `discharge_date_time`

- table:

  (`character`) Which table(s) to include. If multiple, specify a
  character vector (e.g., `table = c("lab", "pharmacy", "radiology")`).

  For HPC4Health users: Please specify the full table name as it is
  listed in your datacut (e.g., `"admdad_subset"` instead of only
  `"admdad"`).

- plot_timeline:

  (`logical`) Flag indicating whether to plot an overview of data
  timelines by hospital and table. **Note:** This plot only shows a
  rough overview of min-max dates per table for each hospital. Gaps in
  this plot illustrate time periods where no data were available for
  \>28 consecutive days. Importantly, for time periods without any gaps,
  overall data volume may still be low and certain entries/columns may
  still be missing. Users should further inspect the coverage plots
  (`plot_coverage = TRUE`) and perform additional customized checks
  based on their needs.

- plot_coverage:

  (`logical`) Flag indicating whether to plot data coverage. These plots
  show the percentage of `genc_ids` with an entry in the table(s) of
  interest. Data coverage is plotted by hospital and discharge month
  (because GEMINI data are pulled based on encounters' discharge date).
  Users should carefully inspect these plots for any drops/gaps in
  coverage that may not be visible in the timeline plots (see
  `plot_timeline` above). When plotting the `admdad` table, the function
  will plot the number of encounters by discharge month to show overall
  data volume (note: by definition, 100% of `genc_ids` in GEMINI data
  have an entry in `admdad`).

- hospital_label:

  (`character`) Optional: Name of variable in `cohort` table that
  corresponds to custom label for hospitals (e.g., letters A-E instead
  of hospital_num 101-105). Will be used for plotting purposes.

- hospital_group:

  (`character`) Optional: Name of variable in `cohort` table that
  corresponds to grouping of hospitals (e.g., Teaching vs.
  Non-teaching). Hospitals will be grouped accordingly in all
  plots/output tables.

- custom_dates:

  (`data.frame`\|`data.table`) Optional input allowing users to specify
  a customized timeline (in 'yyyy-mm-dd' format) to be included for a
  given hospital\*table combination. The user-provided input overwrites
  the corresponding row(s) in the `lookup_data_coverage` table. This can
  be used to exclude time periods (e.g., due to data quality issues) and
  generate customized timeline plots. For example, let's say you
  identified a data quality issue in the transfusion table at hospital
  104 for discharge dates \< 2019-01-01. To only include transfusion
  data for encounters discharged after this time period, specify:
  `custom_dates <- data.frame( data = "transfusion", hospital_num = 104, min_date = "2019-01-01", max_date = "2022-06-30" )`

  This will overwrite the `min_date`/`max_date` values for site 104
  transfusion data in the `lookup_data_coverage` table (data for all
  other hospital\*table combinations will remain the same).
  Additionally, the coverage flags by `genc_id` (in returned
  `coverage_flag_enc`) and timeline plot (see `plot_timeline`) will be
  adjusted according to the user-provided dates. The coverage plot (see
  `plot_coverage`) is not affected by the user-specified the
  `custom_dates` input.

- as_plotly:

  (`logical`) Will return any figures as interactive plots using
  `plotly`. Note that this may affect plot aesthetics. The flag will be
  ignored if the `plotly` package is not installed.

- ...:

  Additional inputs that can be passed to control plot aesthetics in
  `plot_timeline` or `plot_coverage` plots, such as:

  - `base_size`: Font size (default = 12)

  - `colors`: Plot color(s) (default = gemini_colors(1))

  - `hospital_group`: Name of variable in cohort specifying color
    grouping of hospitals (e.g., Teaching/Non-teaching); for the
    timeline plot, this is only applied when plotting a single table
    (otherwise, color grouping is applied to different table names by
    default)

  For coverage plots only (inputs are passed to
  [`plot_over_time()`](https://gemini-medicine.github.io/Rgemini/reference/plot_over_time.md)):

  - `time_int`: Time interval used to aggregate data (e.g., by `"month"`
    = default, `"quarter"`, or `"year"`)

  - `ylimits`: Limits of y-axis (e.g., `c(0, 50)`)

  - `scales`: Passed to facet wrap to control if y-scales are `"fixed"`
    (default) or `"free"` (only works if no `ylimits` specified)

## Value

If the plotting flags are set to `FALSE`, this function will return a
single `data.table` object with a flag for each `genc_id` indicating
whether the encounter was discharged during a time period in which data
for a given table (e.g., `"lab"`) were *in principle* available. If the
flag is `FALSE`, the `genc_id` was dicharged during a time period where
GEMINI did not receive any data for the table of interest. If the flag
is `TRUE`, the `genc_id` was discharged during a time period where
GEMINI received *some* data from a given hospital (however, coverage may
still be low, so users are advised to perform additional coverage
checks, e.g., by using the plotting features of this function).

When the plotting flags are set to `TRUE` (default), the function will
return additional data tables (`output[["data"]]`) and plots
(`output[["plots"]]`):

- If `plot_timeline = TRUE`: Returns `timeline_plot` showing an overview
  of data timelines by hospital for each table. Will also return the
  corresponding data table `timeline_data` that lists the min - max
  dates of each table per site (in long format, where tables with
  multiple rows per hospital indicate gaps in the data timeline)

- If `plot_coverage = TRUE`: Returns `coverage_plot` showing the
  percentage of `genc_ids` with an entry in a given table by discharge
  month and hospital. Will also return the plotted data as
  `coverage_data` to facilitate further inspection.

## Details

`data_coverage` provides analysts with a tool to inform their decisions
about which hospitals/time periods to include in their analyses,
depending on the data tables of interest. For example, if a project
relies on lab data (e.g., `mlaps` variable), users should carefully
inspect lab data coverage (see `plot_coverage` below). If lab data
coverage is high (e.g., \\95% of `genc_ids` have an entry in the lab
table), individual `genc_ids` may still not exist in the lab table
(e.g., because lab testing was not indicated) and individual lab columns
might still have missing values. Users should carefully consider how to
handle these cases depending on the context of their analyses.

## Warning!

- Data coverage checks should generally be performed on the whole
  dataset, prior to applying any additional cohort
  inclusions/exclusions.

- If you are an HPC4Health user and your datacut has been pre-filtered
  based on certain inclusion/exclusion criteria (e.g., diagnosis codes),
  please keep in mind that the coverage plots (see `plot_coverage`) may
  be skewed in smaller/pre-filtered samples. Please reach out to the
  GEMINI team if you need additional support.

- Data coverage checks are particularly relevant for clinical data
  tables (e.g., lab, pharmacy, radiology, transfusions, vitals, clinical
  notes etc.).

- This function should be used as a starting point for data coverage
  checks, but users are advised to perform additional checks based on
  their specific needs.

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

cohort <- dbGetQuery(db, "SELECT genc_id FROM admdad;")

## run function with default flags to create all plots
# Note: This might take a while to run...
coverage <- data_coverage(dbcon, cohort, table = c("admdad", "radiology"))

# get flags per encounter based on encounter's discharge date
enc_flag <- coverage[["data"]][1] # coverage[["data"]]$coverage_flag_enc

# get data timeline (min-max dates)
data_timeline <- coverage[["data"]][2] # coverage[["data"]]$timeline_data

# get % encounters with entry in each table by discharge month & hospital
prct_coverage <- coverage[["data"]][3] # coverage[["data"]]$coverage_data


## run function without any plots
# (will only return data.table with encounter-level flag)
coverage <- data_coverage(
  dbcon,
  cohort,
  table = c("admdad", "radiology"),
  plot_timeline = FALSE,
  plot_coverage = FALSE
)
} # }
```
