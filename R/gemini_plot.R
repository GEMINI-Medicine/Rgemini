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
#' The name of the variable specifying time. Typically "discharge_date_time".
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
#'
#' @param func (`character`)\cr
#' The summary function used to aggregate data by time & hospital. Has to be one
#' of the following:
#' - "mean" (default)
#' - "median"
#' - "%" or "perc" (for categorical variables), users also need to specify
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
#' @param ylabel (`character`)\cr
#' Title on y-axis
#' @param ylimits (`numeric`)\cr
#' Numeric vector specifying limits for y-axis e.g. c(0, 100).
#' @param min_n (`numeric`)\cr
#' Minimum number of data points required for each hospital * time point
#' combination. Data points with cell count < `min_n` will be suppressed.
#' @param colors (`character`)\cr
#' Character vector specifying line color(s).
#' @param return_data (`logical`)\cr
#' Flag indicating whether to return a list of 2 data.tables with aggregated
#' data ([1] by hospital and [2] overall). If `FALSE` (default), will return plot.
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
plot_hosp_time <- function(
    cohort,
    plot_var = NULL,
    time_var = "discharge_date_time",
    hosp_var = "hospital_num",
    hosp_group = NULL,
    facet_var = NULL,
    time_int = "month",
    func = "mean",
    plot_cat = NULL,
    show_overall = TRUE,
    line_width = 1,
    ylabel = NULL,
    ylimits = NULL,
    min_n = 0,
    colors = plot_colors,
    return_data = FALSE
) {


  ##### Check inputs #####
  if (missing(plot_var) && !grepl("^n|count", func, ignore.case = TRUE)) stop("Missing the plot variable selection")

  Rgemini:::check_input(
    cohort,
    c("data.table", "data.frame"),
    colnames = c(plot_var, time_var, hosp_var, hosp_group, facet_var)
  )

  ##### Prepare data #####
  cohort <- cohort %>% as.data.table()

  ## Get time_int
  # if user did not provide custom time_int variable in cohort
  # derive time_int (by default year-month)
  if (!time_int %in% colnames(cohort)) {
    time_label <- fix_string_label(paste(strsplit(time_var, "[_]")[[1]][1], time_int))

    # code below assumes that date-time variable is provided as character
    # if provided as POSIX, turn into character here
    if (grepl("POSIX", class(cohort[[time_var]]))) {
      cohort[, paste(time_int) := as.character(get(time_int))]
    }

    if (grepl("month", time_int, ignore.case = TRUE)) {
      cohort[, paste(time_int) := ym(format(as.Date(lubridate::ymd_hm(get(time_var)), format = "%Y-%m-%d"), "%Y-%m"))]

    } else if (grepl("quarter", time_int, ignore.case = TRUE)) {
      cohort[, paste(time_int) := paste0(lubridate::year(get(time_var)), '-Q', lubridate::quarter(get(time_var)))]
      cohort[, paste(time_int) := factor(time_int, levels = unique(sort(time_int)))]

    } else if (grepl("year", time_int, ignore.case = TRUE)) {
      cohort[, paste(time_int) := lubridate::year(get(time_var))]

    } else if (grepl("fisc_year", time_int, ignore.case = TRUE)) {
      cohort[, paste(time_int) := hospital_fiscal_year(get(time_var))]

    } else if (grepl("season", time_int, ignore.case = TRUE)) {
      cohort[, paste(time_int) := season(get(time_var))]
      cohort[, paste(time_int) := factor(time_int, levels = unique(time_int))]
    }

  } else {
    cohort[, time_int := cohort[[time_int]]]
    time_label <- fix_string_label(time_int)

  }


  ##### Plot colors #####
  ## If single color is specified, will be used across all group levels
  if (length(colors) == 1 && length(unique(cohort[[hosp_group]])) > 1) {
    colors <- rep(colors, length(unique(cohort[[hosp_group]])))
  }
  ## Use default colors if not enough color values specified for all grouping levels
  if (!is.null(hosp_group) && length(unique(cohort[[hosp_group]])) > length(colors)) {
    colors <- NULL
  }


  if (!is.null(hosp_group)) {
    cohort[[hosp_group]] <- as.factor(cohort[[hosp_group]])
  }
  if (!is.null(facet_var)) {
    cohort[[facet_var]] <- as.factor(cohort[[facet_var]])
  }

  ## check if plot_var is character/factor/logical -> plot % by default
  func <- ifelse(!grepl("^n|count", func, ignore.case = TRUE) && (any(class(cohort[[plot_var]]) %in% c("character", "factor", "logical"))), "%", func)

  ## if no plot_cat level specified, sort unique values and plot highest one
  if (grepl("%|perc", func, ignore.case = TRUE) && is.null(plot_cat)) {
    plot_cat <- dplyr::last(sort(unique(cohort[[plot_var]])))
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

      res <- data[, .(outcome = 100 * sum(get(plot_var) %in% plot_cat)  / sum(.N),
                      n = .N), by = grouping]

    } else if (grepl("missing|^na", func, ignore.case = TRUE)) {

      browser()
      n_missing(get(plot_var), na_strings = c("", "NA", " "), index = TRUE)

      res <- data[, .(outcome = 100 * sum(is.na(get(plot_var)))  / sum(.N),
                      n = .N), by = grouping]
    }

    ## exclude observations with low cell count if min_n specified
    if (min_n > 0 & !grepl("^n|count", func, ignore.case = TRUE)) {
      res[n < min_n, outcome := NA]
    }

    return(res)
  }

  ## Aggregate data by all relevant variables
  grouping <- c(time_int, hosp_var, hosp_group, facet_var)
  res <- aggregate_data(cohort, func, grouping)


  ## Aggregate data by time * group (if any, otherwise, will just aggregate across all observations)
  if (!is.null(hosp_var) && !is.null(facet_var) && hosp_var == facet_var) {
    grouping <- c(time_int, hosp_group)
  } else {
    grouping <- c(time_int, hosp_group, facet_var)
  }
  # for count variables, "overall" line represents median of all other lines
  if (grepl("^n|count", func, ignore.case = TRUE)){
    res_grouped <- res[, .(outcome = median(outcome, na.rm = TRUE)), by = grouping]
  } else { # for all other functions, "overall" line represents mean/median etc. across all data points (per grouping var)
    res_grouped <- aggregate_data(cohort, func, grouping)
  }


  if (return_data) {
    return(list(res, res_grouped))
  } else {

    # Create the plot -- grouped
    overall_label <- ifelse((grepl("^n|count", func, ignore.case = TRUE)), "Median", "Overall")

    fig <- ggplot(
      res_grouped,
      aes(x = get(time_int), y = outcome,
          group = if (is.null(hosp_group)) overall_label else get(hosp_group),
          color = if (is.null(hosp_group)) overall_label else get(hosp_group)))


    if (!is.null(hosp_var)) {
      fig <- fig + geom_line(data = res,
                             aes(x = get(time_int), y = outcome,
                                 group = if (is.null(hosp_var)) overall_label else get(as.character(hosp_var)),
                                 color = if (is.null(hosp_group)) overall_label else get(hosp_group)),
                             linewidth = line_width,
                             alpha = ifelse(is.null(facet_var) || ((!is.null(facet_var) && !is.null(hosp_var) && (facet_var != hosp_var))), 0.2, 1),
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
        alpha = ifelse(!is.null(facet_var) && !is.null(hosp_var) && facet_var == hosp_var, 0.2, 1)
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
                             paste0(fix_string_label(plot_var), " = ", paste0(plot_cat, collapse = "/"), " (%)"),
                             ifelse(grepl("^n|count", func, ignore.case = TRUE), "N",
                                    paste0(fix_string_label(plot_var), " (", func, ")")))),
        limits = ylimits,
        expand = c(0,0)
      ) +
      xlab(time_label) +
      gemini_theme(base_size = 12, #ifelse(is.null(facet_var) || (!is.null(facet_var) && length(unique(cohort[[facet_var]])) < 3), 12, ceiling(30/sqrt(length(unique(cohort[[facet_var]]))))),
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
}

