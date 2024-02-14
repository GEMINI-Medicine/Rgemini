fix_string_label <- function(str) {
  str <- tools::toTitleCase(gsub("[_.]", " ", str))
}

#' @title
#' Plot by hospital and time
#'
#' @description
#' Function plotting a variable of interest by hospital and over time.
#'
#' Creates a ggplot of the cohort data across time for a hospital
#' @param cohort (`data.frame | data.table`)\cr
#' A table containing the relevant cohort data to be plotted.
#' @param plot_var (`character`)\cr
#' The name of the variable we wish to plot
#' @param time_var (`character`)\cr
#' The name of the variable specifying time
#' @param hosp_var (`character`)\cr
#' The name of the variable specifying individual hospitals
#' @param hosp_group (`character`)\cr
#' The name of the variable used to group hospitals
#' @param facet_var (`character`)\cr
#' The name of variable specifying facet plots
#' @param time_int (`character`)\cr
#' Time interval to plot by (x-axis intervals). Currently, the function can
#' automatically calculate any of the following time intervals for any date-time
#' variables:
#' - "month" (default)
#' - "quarter"
#' - "year"
#' - "fisc_year" for hospital fiscal year starting in April
#' - "season"
#'
#' For any other custom time intervals (e.g., weeks), users can calculate any
#' custom time interval prior to running this funciton, and then provide this
#' variable as input to both `time_var` and `time_int`. For example, users could
#' derive a weekly time variable called `"week"` in their `cohort` table, and
#' then specify `time_var = "week"` and `time_int = "week"`).
#' If `time_var` is equal to any of `c("month", "quarter", "year", "fisc_year")`,
#' the user-provided variable in `cohort` corresponding to that variable will be
#' used (instead of the version of that variable calculated inside this function).
#'
#'
#' @param func (`character`)\cr
#' The summary function used to aggregate data by time & hospital. Has to be one
#' of the following:
#' - "mean" (default)
#' - "median"
#' - "%" or "perc" (for categorical variables), users also need to specify
#' `plot_cat` to specify which category to plot the percentage of (see below)
#' - "n" or "count" to plot the count of rows per hospital/time period
#' @param plot_cat
#' Required when `func = "%"`. Users need to specify the level of any
#' categorical variables to be plotted. For example, to plot the percentage of
#' female encounters, `plot_cat = "F"`. By default, the function assumes that a
#' user wants to plot % of rows where category = `TRUE` (for logical variables).
#' Can also be used for numeric variables, e.g., `plot_cat = 0` to plot
#' encounters with Charlson Comorbidity Index = 0.
#' @param show_overall (`logical`)\cr
#' Flag indicating whether to plot thick line representing overall value across
#' hospitals.
#' @param line_width (`numeric`)\cr
#' Width of individual lines. Summary line will be 2 * line_width.
#' @param ylabel (`character`)\cr
#' Title on y-axis
#' @param ylimits (`numeric`)\cr
#' Numeric vector specifying limits for y-axis e.g. c(0, 100).
#' @param min_n (`numeric`)\cr
#' Minimum number of data points required for each hospital * time point
#' combination. Data points with cell count < `min_n` will be suppressed.
#' @param colors (`character`)\cr
#' Character vector specifying line color(s).
#'
#' @import ggplot2
#' @importFrom lemon facet_rep_wrap
#'
#' @return
#' ggplot figure
#'
#' @seealso `vignette("plotting_functions", package = "Rgemini")`
#'
#' @export
#'
plot_hosp_time <- function(
    cohort,
    plot_var = NULL,
    time_var,
    hosp_var = "hospital_num",
    hosp_group = NULL,
    facet_var = NULL,
    time_int = "month",
    func = "mean",
    plot_cat = TRUE,
    show_overall = TRUE,
    line_width = 1,
    ylabel = NULL,
    ylimits = NULL,
    min_n = 0,
    colors = plot_colors
) {


  ## Check arguments
  if (missing(plot_var) && !grepl("^n|count", func, ignore.case = TRUE)) stop("Missing the plot variable selection")
  if (missing(time_var)) stop("Missing time variable")

  # if user did not provide custom time_int variable specified by time_var
  # derive time_int (by default year-month)
  if (time_int != time_var){
    if (grepl("month", time_int, ignore.case = TRUE) {
      cohort
    }
  }



  ## Check inputs
  Rgemini:::check_input(
    cohort,
    c("data.table", "data.frame"),
    colnames = c(plot_var, time_var, hosp_var, hosp_group, facet_var)
  )

  if (length(colors) == 1 && length(unique(cohort[[hosp_group]])) > 1){
    colors <- rep(colors, length(unique(cohort[[hosp_group]])))
  }

  ## Use default colors if not enough color values specified for all grouping levels
  if (!is.null(hosp_group) && length(unique(cohort[[hosp_group]])) > length(colors)){
    colors <- NULL
  }

  ## Prepare data
  cohort <- cohort %>% as.data.table()

  if (!is.null(hosp_group)){
    cohort[[hosp_group]] <- as.factor(cohort[[hosp_group]])
  }
  if (!is.null(facet_var)){
    cohort[[facet_var]] <- as.factor(cohort[[facet_var]])
  }

  ## Function aggregating data by specified grouping variables
  aggregate_data <- function(data, func, grouping){
    # aggregate for each individual hospital & by hospital_type
    if (grepl("^n|count", func, ignore.case = TRUE)) {
      res <- data[, .(outcome = .N), by = grouping]

    } else if (grepl("mean", func, ignore.case = TRUE)) {
      res <- data[, .(outcome = mean(as.numeric(get(plot_var)), na.rm = TRUE),
                              n = .N), by = grouping]

    } else if (grepl("median", func, ignore.case = TRUE)) {
      res <- data[, .(outcome = median(as.numeric(get(plot_var)), na.rm = TRUE),
                              n = .N), by = grouping]

    } else if (grepl("%|perc", func, ignore.case = TRUE)) {
      data <- data[!is.na(get(plot_var)),] # remove NA from denominator
      res <- data[, .(outcome = 100 * sum(get(plot_var) == plot_cat)  / sum(.N),
                              n = .N), by = grouping]
    }

    ## exclude observations with low cell count if min_n specified
    if (min_n > 0 & !grepl("^n|count", func, ignore.case = TRUE)) {
      res[n < min_n, outcome := NA]
    }

    return(res)
  }

  ## Aggregate data by all relevant variables
  grouping <- c(time_var, hosp_var, hosp_group, facet_var)
  res <- aggregate_data(cohort, func, grouping)


  ## Aggregate data by time * group (if any, otherwise, will just aggregate across all observations)
  if (!is.null(hosp_var) && !is.null(facet_var) && hosp_var == facet_var){
    grouping <- c(time_var, hosp_group)
  } else {
    grouping <- c(time_var, hosp_group, facet_var)
  }
  # for count variables, "overall" line represents median of all other lines
  if (grepl("^n|count", func, ignore.case = TRUE)){
    res_grouped <- res[, .(outcome = median(outcome, na.rm = TRUE)), by = grouping]
  } else { # for all other functions, "overall" line represents mean/median etc. across all data points (per grouping var)
    res_grouped <- aggregate_data(cohort, func, grouping)
  }

  # Create the plot -- grouped
  overall_label <- ifelse((grepl("^n|count", func, ignore.case = TRUE)), "Median", "Overall")

  fig <- ggplot(
    res_grouped,
    aes(x = get(time_var), y = outcome,
        group = if (is.null(hosp_group)) overall_label else get(hosp_group),
        color = if (is.null(hosp_group)) overall_label else get(hosp_group)))


  if (!is.null(hosp_var)) {
    fig <- fig + geom_line(data = res,
                           aes(x = get(time_var), y = outcome,
                               group = if (is.null(hosp_var)) overall_label else get(as.character(hosp_var)),
                               color = if (is.null(hosp_group)) overall_label else get(hosp_group)),
                           linewidth = line_width,
                           alpha = ifelse(is.null(facet_var) || ((!is.null(facet_var) && !is.null(hosp_var) && (facet_var != hosp_var))), 0.25, 1),
                           show.legend = (!is.null(hosp_group) && (is.null(facet_var) || (!is.null(facet_var) && hosp_group != facet_var) && (!is.null(facet_var) && !is.null(hosp_var) && hosp_var != facet_var)))
    )

    if (!is.null(facet_var)){
      fig <- fig +
        facet_rep_wrap(~get(facet_var), scales = "fixed") +
        theme(panel.spacing.y = unit(0, "lines"))
    }

  }

  if (show_overall == TRUE) {

    fig <- fig + geom_line(
      # average line for the group
      linewidth = ifelse(!is.null(hosp_var) && !is.null(hosp_group) && hosp_var == hosp_group, line_width, 2*line_width),
      show.legend = ((!is.null(hosp_group) || !is.null(hosp_var))) && (is.null(facet_var) || (!is.null(facet_var) && hosp_group != facet_var)),
      alpha = ifelse(!is.null(facet_var) && !is.null(hosp_var) && facet_var == hosp_var, 0.25, 1)
    ) + labs(color = NULL)
  }


  # Common configs for plot
  if (is.null(ylimits)) {
    ylimits <- c(floor(min(res$outcome, na.rm = TRUE)/1.1), ceiling(max(res$outcome, na.rm = TRUE)*1.1))
  } else {
    ylimits <- ylimits
  }


  fig <- fig +
    scale_y_continuous(
      name = ifelse(!is.null(ylabel), ylabel,
                    ifelse(grepl("%|perc", func, ignore.case = TRUE),
                           paste0(fix_string_label(plot_var), ' = ', plot_cat, ' (%)'),
                           ifelse(grepl("^n|count", func, ignore.case = TRUE), 'N',
                                  paste0(fix_string_label(plot_var), ' (', func, ')')))),
      limits = ylimits,
      expand = c(0,0)
    ) +
    xlab(fix_string_label(time_var)) +
    gemini_theme(base_size = ifelse(is.null(facet_var) || (!is.null(facet_var) && length(unique(cohort[[facet_var]])) < 3), 12, ceiling(25/sqrt(length(unique(cohort[[facet_var]]))))),
                 aspect_ratio = NULL) + # 0.8
    theme(axis.text.x=element_text(angle = 60, hjust = 1))

  #scale_x_date(
  #  name = paste0(" \n", fix_string_label(time_var))
  #, breaks = seq(as.Date("2015-04-01"), as.Date("2022-06-01"), by = "1 year"), date_labels = "%Y"
  #) +


  if (!is.null(colors)){
    fig <- fig + scale_color_manual(values = colors)
  }


  if (!is.null(hosp_group) && (is.null(facet_var) || (!is.null(facet_var) && hosp_group != facet_var) || (!is.null(facet_var) && !is.null(hosp_var) && hosp_var != facet_var))) {
    fig <- fig + labs(
      # Legend title
      colour = fix_string_label(hosp_group)
    )
  }

  return(fig)
}

