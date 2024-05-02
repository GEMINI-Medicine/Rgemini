#' @title
#' Plot results over time
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
#' The name of the variable specifying time. Typically "discharge_date_time".
#' @param line_group (`character`)\cr
#' Grouping variable representing individual lines By default:
#' `"hospital_num"` (unless `"hospital_num"` does not exist, in which case the
#' function will check for existence of `"hospital_id"` instead).
#' @param color_group (`character`)\cr
#' Grouping variable used for color coding.
#' @param facet_group (`character`)\cr
#' Grouping variable specifying facet plots.
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
#' For any other custom time intervals (e.g., weeks), users can calculate the
#' desired time interval prior to running this function. As long as this custom
#' time variable exists in the `cohort` input, users can then specify this
#' variable as the `time_int` input. For example, users could derive a weekly
#' time variable called `"week"` in their `cohort` table, and
#' then specify `time_int = "week"`.
#' If `time_var` is equal to any of `c("month", "quarter", "year", "fisc_year")`
#' and there is a user-provided variable of that same name in the `cohort`
#' input, the function will default to the variable that exists in the user-
#' provided input.
#'
#' @param func (`character`)\cr
#' The summary function used to aggregate data by time & hospital. Has to be one
#' of the following:
#' - "mean" (default)
#' - "median"
#' - "%" or "prct"/"perc" (for categorical variables), users also need to specify
#' `plot_cat` to specify which category to plot the percentage of (see below)
#' - "n" or "count" to plot the count of rows per hospital/time period
#' - "missing" or "na"
#' @param plot_cat
#' Required when `func = "%"` and/or when plotting categorical/factor variables.
#' Users need to specify the level of any categorical variables to be plotted.
#' For example, to plot the percentage of female encounters, `plot_cat = "F"`.
#' Multiple categories can be provided as a vector, e.g., to plot all non-male
#' encounters: `plot_cat = c("F", "O")`. If no `plot_cat` is specified, the
#' function will sort the unique entries of the specified `plot_var` and plot
#' the percentage of rows matching the highest value (e.g., for logical
#' variables, `% TRUE` will be plotted by default).
#' `plot_cat` can also be used for numeric variables, e.g., `plot_cat = 0` to
#' plot encounters with Charlson Comorbidity Index = 0.
#' @param show_overall (`logical`)\cr
#' Flag indicating whether to plot thick line representing overall value across
#' hospitals.
#' @param line_width (`numeric`)\cr
#' Width of individual lines. Summary line will be 2 * line_width.
#' @param ylimits (`numeric`)\cr
#' Numeric vector specifying limits for y-axis e.g. `c(0, 100)`. To specify only
#' the lower/upper limit, use `NA` (e.g., `c(NA, 100)` to fix upper limit only).
#' @param min_n (`numeric`)\cr
#' Minimum number of data points required for each hospital * time point
#' combination. Data points with cell count < `min_n` will be suppressed.
#' @param colors (`character`)\cr
#' Character vector specifying line color(s).
#' @param return_data (`logical`)\cr
#' Flag indicating whether to return a list of 2 data.tables with aggregated
#' data ([1] by hospital and [2] overall). If `FALSE` (default), will return plot.
#'
#' @param ... \cr
#' If a `facet_group` is specified: Additional arguments passed to
#' `lemon::facet_rep_wrap()` (wrapper for `ggplot2::facet_wrap`), e.g.,
#' `scales = "fixed"` (default) vs. `scales = "free"`, `nrow`/`ncol` etc.
#'
#' @import ggplot2
#' @importFrom lemon facet_rep_wrap
#'
#' @return
#' By default, returns `ggplot` figure. If `return_data = TRUE`, returns
#' `data.tables` containing aggregated data.
#'
#' @seealso `vignette("plotting_functions", package = "Rgemini")`
#'
#' @export
#'
plot_over_time <- function(
    cohort,
    plot_var = NULL,
    time_var = "discharge_date_time",
    line_group = "hospital_num",
    color_group = NULL,
    facet_group = "hospital_num",
    time_int = "month",
    func = "mean",
    plot_cat = NULL,
    show_overall = TRUE,
    line_width = 1,
    ylimits = NULL,
    min_n = 0,
    colors = gemini_colors(1),
    return_data = FALSE,
    ...) {


  ##### Check inputs #####
  if (missing(plot_var) && !grepl("^n|count", func, ignore.case = TRUE)) stop("Missing the plot variable selection")

  # by default, use hospital_num as hospital identifier, unless it doesn't exist
  # in cohort input (in that case, check if hospital_id exists and use that)
  if (line_group == "hospital_num" && !"hospital_num" %in% colnames(cohort) && "hospital_id" %in% colnames(cohort)) {
    line_group <- "hospital_id"
  }
  if (facet_group == "hospital_num" && !"hospital_num" %in% colnames(cohort) && "hospital_id" %in% colnames(cohort)) {
    facet_group <- "hospital_id"
  }

  if (is.null(line_group) && !is.null(facet_group)) {
    line_group <- facet_group
  }

  Rgemini:::check_input(
    cohort,
    c("data.table", "data.frame"),
    colnames = c(plot_var, time_var, line_group, color_group, facet_group)
  )

  ##### Prepare data #####
  cohort <- cohort %>% as.data.table()

  ## Get time_int
  # if user did not provide custom time_int variable in cohort
  # derive time_int (by default "month")
  if (!time_int %in% colnames(cohort)) {
    time_label <- fix_var_str(paste(strsplit(time_var, "[_]")[[1]][1], time_int))

    # if user already provided date-time variable in POSIX/POSIXct format, keep
    # as is, otherwise, transform into appropriate format
    if (!any(grepl("POSIX", class(cohort[[time_var]])))) {
      cohort[, paste(time_var) := lubridate::parse_date_time(get(time_var), orders = c("ymd HM", "ymd HMS", "ymd"))]
    }

    if (grepl("month", time_int, ignore.case = TRUE)) {
      cohort[, month := lubridate::ym(format(as.Date(get(time_var), format = "%Y-%m-%d"), "%Y-%m"))]
    } else if (grepl("quarter", time_int, ignore.case = TRUE)) {
      cohort[, quarter := paste0(lubridate::year(get(time_var)), "-Q", lubridate::quarter(get(time_var)))]
      cohort[, quarter := factor(quarter, levels = unique(sort(quarter)))]
    } else if (grepl("^year", time_int, ignore.case = TRUE)) {
      cohort[, year := lubridate::year(get(time_var))]
    } else if (grepl("fisc_year", time_int, ignore.case = TRUE)) {
      cohort[, fisc_year := hospital_fiscal_year(get(time_var))]
    } else if (grepl("season", time_int, ignore.case = TRUE)) {
      cohort[, season := season(get(time_var))]
      cohort[, season := factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))]
    }
  } else {
    cohort[, time_int := cohort[[time_int]]]
    time_label <- fix_var_str(time_int)
  }

  ## pre-process func input into standardized version
  if (grepl("^n$|count", func, ignore.case = TRUE)) {
    func <- "count"
    plot_var <- NULL
  } else if (grepl("mean", func, ignore.case = TRUE)) {
    func <- "mean"
  } else if (grepl("median", func, ignore.case = TRUE)) {
    func <- "median"
  } else if (grepl("%|prct|perc", func, ignore.case = TRUE)) {
    func <- "prct"
  } else if (grepl("missing|^na", func, ignore.case = TRUE)) {
    func <- "missing"
  }

  ##### Plot colors #####
  ## If single color is specified, will be used across all group levels
  if (length(colors) == 1 && length(unique(cohort[[color_group]])) > 1) {
    colors <- rep(colors, length(unique(cohort[[color_group]])))
  }
  ## If not enough color values specified for all grouping levels, duplicate values
  if (!is.null(color_group) && length(unique(cohort[[color_group]])) > length(colors)) {
    colors <- rep_len(colors, length(unique(cohort[[color_group]])))
  }

  if (!is.null(color_group)) {
    cohort[[color_group]] <- as.factor(cohort[[color_group]])
  }
  if (!is.null(facet_group)) {
    cohort[[facet_group]] <- as.factor(cohort[[facet_group]])
  }

  ## check if plot_var is character/factor/logical -> plot % by default
  func <- ifelse(
    !func %in% c("count", "missing") &&
      (any(class(cohort[[plot_var]]) %in% c("character", "factor", "logical"))),
    "prct", func
  )


  ## if no plot_cat level specified, sort unique values and plot highest one
  if (func == "prct" && is.null(plot_cat)) {
    plot_cat <- dplyr::last(sort(unique(cohort[[plot_var]])))
  }

  ## Function aggregating data by specified grouping variables
  aggregate_data <- function(data, func, grouping) {

    # aggregate for each individual hospital & by hospital_type
    if (func == "count") {
      res <- data[, .(outcome = .N), by = grouping]
    } else if (func == "mean") {
      res <- data[, .(
        outcome = mean(as.numeric(get(plot_var)), na.rm = TRUE),
        n = .N
      ), by = grouping]
    } else if (func == "median") {
      res <- data[, .(
        outcome = median(as.numeric(get(plot_var)), na.rm = TRUE),
        n = .N
      ), by = grouping]
    } else if (func == "prct") {
      data <- data[!is.na(get(plot_var)), ] # remove NA from denominator

      res <- data[, .(
        outcome = 100 * sum(get(plot_var) %in% plot_cat) / sum(.N),
        n = .N
      ), by = grouping]
    } else if (func == "missing") {

      res <- data[, .(
        outcome = 100 * sum(sum(n_missing(get(plot_var), na_strings = c("", "NA", " "), index = TRUE))) / sum(.N),
        n = .N
      ), by = grouping]
    }

    ## exclude observations with low cell count if min_n specified (will show up as gap on plot)
    # Note: If this means a whole combination of variables are excluded (e.g., all hospital*time combos for gender = "0"),
    # those will not be filled in again below; this is the desired behavior so fully empty
    # combos aren't shown in the plots
    if (min_n > 0 && func != "count") {
      res <- res[n >= min_n, ]
    }

    ## For any date*hosp combos that don't exist, merge and fill with NA so they correctly show up as empty on graph
    # Note: Does not include combos that don't exist at all (e.g., due to cell suppression)
    res <- droplevels(res) # drop levels that don't exist anymore at all
    append <- suppressWarnings(setDT(tidyr::crossing(unique(res[, ..time_int]), distinct(res[, -c(..time_int, "outcome", "n")]))))
    append <- anti_join(append, res, by = grouping)

    ## append missing dates
    res <- rbind(res, append, fill = TRUE)

    ## for count, impute empty time points with 0, i.e., treat missing time points as true zeros
    # (for all other funcs, missing time periods are shown as gap in timeline)
    if (func %in% c("count")) {
      res[is.na(outcome), outcome := 0]
      res[outcome < min_n, outcome := NA] # cell-suppression for counts (also applied to 0s!)
    }

    return(res)
  }

  ## Aggregate data by all relevant variables
  grouping <- unique(c(time_int, line_group, color_group, facet_group))
  res <- aggregate_data(cohort, func, grouping)

  ## show warning if any groupings completely removed (due to cell suppression)
  check_excl <- function(var) {
    if (!is.null(var)) {
      missing <- unique(cohort[[var]])[!unique(cohort[[var]]) %in% unique(res[!is.na(outcome), get(var)])]
      if (length(missing) > 0) {
        warning(paste0("The following levels of input variable '", var, "' were removed from this plot due to cell suppression (all n < ", min_n, "):"), immediate. = TRUE)
        print(as.character(missing))
      }
    }
  }
  lapply(grouping, check_excl)

  ## show warning if specific combos have missing/cell-suppressed data
  # Note: This includes entries that were removed due to cell-suppression (i.e., low counts)
  # if func = "count", empty cells are set to 0, those are not included here (are treated as "true" zeros)
  if (nrow(res[is.na(outcome)]) > 0) {
    warning("Some time points do not have any data or have been removed due to cell suppression.
      This might introduce a bias in the plotted time trends. Please carefully inspect data availability & coverage.
      Missing data found for the following combinations: ", immediate. = TRUE)
    print(res[is.na(outcome), -c("outcome", "n")])# %>% select(line_group, time_int) %>% arrange(get(line_group)))
  }


  ## Get Overall: Aggregate data by time * group (if any, otherwise, will just aggregate across all observations)
  res_overall <- data.table()
  if (show_overall == TRUE) {
    # the only time you'd want to group by facet_group is if facet_group is different from line_group, or line_group & color_group are different
    if ((!is.null(line_group) && !is.null(facet_group) && line_group != facet_group) &&
        (is.null(color_group) || (!is.null(line_group) && !is.null(color_group) && line_group != color_group))) {
      grouping <- unique(c(time_int, color_group, facet_group))
    } else {
      grouping <- unique(c(time_int, color_group))
    }

    if (func == "count") {
      # for count variables, "overall" line represents median of all other lines
      res_overall <- res[, .(outcome = median(outcome, na.rm = TRUE)), by = grouping]
    } else {
      # for all other functions, "overall" line represents mean/median etc.
      # across all data points (per grouping var)
      res_overall <- aggregate_data(cohort, func, grouping)
    }
  }


  if (return_data) {
    ## change column names for outcome variable for clarity
    col_name <- ifelse(func == "count", "n", paste(func, paste0(c(plot_var, plot_cat), collapse = "_"), sep = "_"))
    setnames(res, "outcome", col_name, skip_absent = TRUE)
    setnames(res_overall, "outcome", col_name, skip_absent = TRUE)

    ## Prepare output
    output <- list()
    if (nrow(res) > 0) {
      output$data_aggr <- res
    }
    if (nrow(res_overall) > 0) {
      output$data_aggr_overall <- res_overall
    }
    return(output)

  } else {


    ######### CREATE PLOT #########
    fig <- ggplot()

    # Label for overall summary line
    overall_label <- ifelse(func == "count", "Median", "Overall")


    ## Add individual hospital lines
    if (!is.null(line_group)) {
      fig <- fig + geom_line(
        data = res,
        aes(
          x = get(time_int), y = outcome,
          group = if (is.null(line_group)) overall_label else get(as.character(line_group)),
          color = if (is.null(color_group)) overall_label else get(color_group)
        ),
        linewidth = line_width,
        alpha = ifelse(show_overall && ((is.null(facet_group) ||
                                           ((!is.null(facet_group) && !is.null(line_group) && (facet_group != line_group)) &&
                                              (is.null(color_group) || ((!is.null(color_group) && !is.null(line_group) && (color_group != line_group)))
                                            ))) && 
                                                 length(unique(res[[line_group]])) > 1), 0.2, 1),
        show.legend = (!is.null(color_group) &&
                         (is.null(facet_group) || ((!is.null(facet_group) && color_group != facet_group)) ||
                            ((!is.null(facet_group) && !is.null(line_group) && line_group == facet_group))))
      )

      if (!is.null(facet_group)) {
        fig <- fig +
          facet_rep_wrap(~ get(facet_group), ...) +
          theme(panel.spacing.y = unit(0, "lines"))
      }
    }

    ## Add overall summary lines
    # Note: If only a single site is included, overall line/legend will not be shown
    if (show_overall == TRUE && (is.null(line_group) || length(unique(res[[line_group]])) > 1)) { 
      fig <- fig +
        geom_line(data = res_overall,
                  aes(x = get(time_int), y = outcome,
                      group = if (is.null(color_group)) overall_label else get(color_group),
                      color = if (is.null(color_group)) overall_label else get(color_group)
                  ),
                  linewidth = ifelse(
                    !is.null(line_group) && !is.null(color_group) && line_group == color_group && is.null(facet_group),
                    line_width,
                    2 * line_width
                  ),
                  show.legend = ((!is.null(color_group) || !is.null(line_group))) &&
                    (is.null(facet_group) || (!is.null(facet_group) && color_group != facet_group)),
                  alpha = ifelse(is.null(facet_group) || (
                    (!is.null(facet_group) && !is.null(line_group) && facet_group != line_group) &&
                      (is.null(color_group) || (!is.null(line_group) && !is.null(color_group) && line_group != color_group))), 1, 0.2)
        ) +
        labs(color = NULL)
    }


    ######### Plot Appearance #########
    ## Adjust labels & theme
    fig <- fig +
      labs(x = time_label,
           y = ifelse(func == "prct", paste0(fix_var_str(plot_var), " = ", paste0(plot_cat, collapse = "/"), " (%)"),
                      ifelse(func == "missing", paste0(fix_var_str(plot_var), " = missing (%)"),
                             ifelse(func == "count", "N",
                                    paste0(fix_var_str(plot_var), " (", func, ")"))))) +
      plot_theme(base_size = 12) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))


    ## Adjust y-axis
    # If ylimit is specified, axis range will be fixed to that
    # otherwise, range will be expanded by 5% (unless min value for count/prct/missing outcomes is 0,
    # or max value of prct/missing outcome = 100, in which case scale is not expanded)
    # calculating this manually here so expansion can be capped
    # Note: For facet plots with free y-scales, 15% expansion is applied without any caps
    if ((is.null(ylimits) || sum(is.na(ylimits)) > 0) && (!is.null(facet_group) && !fig$facet$params$free$y)){
      range <- max(res$outcome, na.rm = TRUE) - min(res$outcome, na.rm = TRUE)

      if (is.null(ylimits) || is.na(ylimits[1])){
        ylimits[1] <- min(res$outcome, na.rm = TRUE) - range*0.05
        if (func %in% c("count", "prct", "missing")) {
          ylimits[1] <- max(ylimits[1], 0) # make sure lower limit doesn't go below 0
        }
      }

      if (is.null(ylimits) || is.na(ylimits[2])){
        ylimits[2] <- max(res$outcome, na.rm = TRUE) + range*0.05
        if (func %in% c("prct", "missing")) {
          ylimits[2] <- min(ylimits[2], 100) # make sure upper limit doesn't go above 100
        }
      }
    }

    fig <- fig +
      scale_y_continuous(limits = ylimits,
                         expand = if (!is.null(facet_group) && fig$facet$params$free$y) c(.15, .15) else c(0,0))

    ## Adjust x-axis
    if (grepl("quarter", time_int, ignore.case = TRUE)) {
      fig <- fig +
        scale_x_discrete(breaks = levels(cohort$quarter)[
          seq(1, length(levels(cohort$quarter)),
              by = ifelse(length(levels(cohort$quarter)) <= 8 && is.null(facet_group), 1,
                          ifelse(length(levels(cohort$quarter)) <= 16, 2, 4)))],
          labels = levels(cohort$quarter)[
            seq(1, length(levels(cohort$quarter)),
                by = ifelse(length(levels(cohort$quarter)) <= 8 && is.null(facet_group), 1,
                            ifelse(length(levels(cohort$quarter)) <= 16, 2, 4)))])
    } else if (grepl("month", time_int, ignore.case = TRUE)) {
      fig <- fig + scale_x_date(breaks = seq(
        as.Date(min(cohort$month, na.rm = TRUE)),
        as.Date(max(cohort$month, na.rm = TRUE)),
        by = ifelse((is.null(facet_group) && length(unique(cohort$month)) <= 24) || (!is.null(facet_group) && length(unique(cohort$month)) <= 18), "1 month",
                    ifelse((is.null(facet_group) && length(unique(cohort$month)) <= 60) || (!is.null(facet_group) && length(unique(cohort$month)) <= 45), "6 months", "1 year"))),
        date_labels = ifelse(is.null(facet_group), "%b-%Y", "%m/%y"))
    }

    ## Apply colors
    if (!is.null(colors)) {
      fig <- fig + scale_color_manual(values = colors)
    }


    ## Legend title
    if (!is.null(color_group) &&
        (is.null(facet_group) || (!is.null(facet_group) && color_group != facet_group) ||
         (!is.null(facet_group) && !is.null(line_group) && line_group != facet_group))) {
      fig <- fig +
        labs(colour = fix_var_str(color_group))
    }

    return(fig)
  }
}