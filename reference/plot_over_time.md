# Plot variable over time

Function plotting a variable of interest over time (and by hospital, or
other grouping variables).

## Usage

``` r
plot_over_time(
  data,
  plot_var = NULL,
  time_var = "discharge_date_time",
  line_group = "hospital_num",
  color_group = NULL,
  facet_group = "hospital_num",
  time_int = "month",
  func = "mean",
  plot_cat = NULL,
  show_overall = TRUE,
  smooth_method = NULL,
  line_width = 1,
  ylimits = NULL,
  min_n = 0,
  colors = gemini_colors(1),
  base_size = 12,
  return_data = FALSE,
  ...
)
```

## Arguments

- data:

  (`data.frame | data.table`)  
  A table containing the relevant data to be plotted.

- plot_var:

  (`character`)  
  The name of the outcome variable to plot.

- time_var:

  (`character`)  
  The name of the variable specifying time. By default:
  `"discharge_date_time"`.

- line_group:

  (`character`)  
  Grouping variable representing individual lines. By default:
  `"hospital_num"` (unless `"hospital_num"` does not exist, in which
  case the function will check for `"hospital_id"` instead).

- color_group:

  (`character`)  
  Grouping variable used for color coding.

- facet_group:

  (`character`)  
  Grouping variable specifying facet plots.

- time_int:

  (`character`)  
  Time interval used for plotting (i.e., x-axis intervals). Currently,
  the function can automatically calculate any of the following time
  intervals for any date-time variables:

  - "month" (default)

  - "quarter"

  - "year"

  - "fisc_year" for hospital fiscal year starting in April

  - "season"

  For any other custom time intervals (e.g., weeks), users can calculate
  the desired time interval prior to running this function. As long as
  this custom time variable exists in the `data` input, users can then
  specify this variable as the `time_int` input. For example, users
  could derive a weekly time variable called `"week"` in their `data`
  table, and then specify `time_int = "week"`. If `time_var` is equal to
  any of `c("month", "quarter", "year", "fisc_year")` and there is a
  user- provided variable of that same name in the `data` input, the
  function will default to the variable that exists in the user-provided
  input.

- func:

  (`character`)  
  The summary function used to aggregate data by time & hospital. Has to
  be one of the following:

  - "mean" (default)

  - "median"

  - "%" or "prct"/"perc" (for categorical variables), users also need to
    specify `plot_cat` to specify which category to plot the percentage
    of (see below)

  - "n" or "count" to plot the count of rows per hospital/time period

  - "missing" or "na"

- plot_cat:

  Required when `func = "%"` and/or when plotting categorical/factor
  variables. Users need to specify the level of any categorical
  variables to be plotted. For example, to plot the percentage of female
  encounters, `plot_cat = "F"`. Multiple categories can be provided as a
  vector, e.g., to plot all non-male encounters:
  `plot_cat = c("F", "O")`. If no `plot_cat` is specified, the function
  will sort the unique entries of the specified `plot_var` and plot the
  percentage of rows matching the highest value (e.g., for logical
  variables, `% TRUE` will be plotted by default). `plot_cat` can also
  be used for numeric variables, e.g., `plot_cat = 0` to plot encounters
  with Charlson Comorbidity Index = 0.

- show_overall:

  (`logical`)  
  Flag indicating whether to plot thick line representing overall value
  across hospitals.

- smooth_method:

  (`character`)  
  Character specifying which smoothing method to apply (if any). By
  default, this is set to `NULL` (i.e., no smoothing applied). If users
  specify a method (e.g., `smooth_method = "glm"`), individual data
  points will be shown as dots and the smoothed time trend will be shown
  as a line.

- line_width:

  (`numeric`)  
  Width of individual lines. Summary line will be 2 \* line_width.

- ylimits:

  (`numeric`)  
  Numeric vector specifying limits for y-axis e.g. `c(0, 100)`. To
  specify only the lower/upper limit, use `NA` (e.g., `c(NA, 100)` to
  fix upper limit only).

- min_n:

  (`numeric`)  
  Minimum number of data points required for each hospital \* time point
  combination. Data points with cell count \< `min_n` will be
  suppressed.

- colors:

  (`character`)  
  Character vector specifying line color(s).

- base_size:

  (`numeric`)  
  Numeric input to determine the base font size for each subplot in pts
  (default = 12)

- return_data:

  (`logical`)  
  Flag indicating whether to return a list of 2 data.tables with
  aggregated data (\[1\] by hospital and \[2\] overall). If `FALSE`
  (default), will return plot.

- ...:

    
  If a `facet_group` is specified: Additional arguments passed to
  [`lemon::facet_rep_wrap()`](https://rdrr.io/pkg/lemon/man/facet_rep.html)
  (wrapper for
  [`ggplot2::facet_wrap`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)),
  e.g., `scales = "fixed"` (default) vs. `scales = "free"`,
  `nrow`/`ncol` etc.

## Value

By default, returns `ggplot` figure. If `return_data = TRUE`, returns
`data.tables` containing aggregated data.

## See also

[`vignette("plotting_data_exploration", package = "Rgemini")`](https://gemini-medicine.github.io/Rgemini/articles/plotting_data_exploration.md)
