#' @title
#' Plot variable over time
#'
#' @description
#' Function plotting a variable of interest over time (and by hospital, or other grouping variables).
#'
#' @param data (`data.frame | data.table`)\cr
#' A table containing the relevant data to be plotted.
#' @param plot_var (`character`)\cr
#' The name of the outcome variable to plot.
#' @param time_var (`character`)\cr
#' The name of the variable specifying time. By default: `"discharge_date_time"`.
#' @param line_group (`character`)\cr
#' Grouping variable representing individual lines. By default: `"hospital_num"` (unless
#' `"hospital_num"` does not exist, in which case the function will check for `"hospital_id"` instead).
#' @param color_group (`character`)\cr
#' Grouping variable used for color coding.
#' @param facet_group (`character`)\cr
#' Grouping variable specifying facet plots.
#' @param time_int (`character`)\cr
#' Time interval used for plotting (i.e., x-axis intervals). Currently, the function can automatically
#' calculate any of the following time intervals for any date-time variables:
#' - "month" (default)
#' - "quarter"
#' - "year"
#' - "fisc_year" for hospital fiscal year starting in April
#' - "season"
#'
#' For any other custom time intervals (e.g., weeks), users can calculate the desired time interval prior
#' to running this function. As long as this custom time variable exists in the `data` input, users can
#' then specify this variable as the `time_int` input. For example, users could derive a weekly time
#' variable called `"week"` in their `data` table, and then specify `time_int = "week"`.
#' If `time_var` is equal to any of `c("month", "quarter", "year", "fisc_year")` and there is a user-
#' provided variable of that same name in the `data` input, the function will default to the variable
#' that exists in the user-provided input.
#'
#' @param func (`character`)\cr
#' The summary function used to aggregate data by time & hospital. Has to be one of the following:
#' - "mean" (default)
#' - "median"
#' - "%" or "prct"/"perc" (for categorical variables), users also need to specify `plot_cat` to
#' specify which category to plot the percentage of (see below)
#' - "n" or "count" to plot the count of rows per hospital/time period
#' - "missing" or "na"
#' @param plot_cat
#' Required when `func = "%"` and/or when plotting categorical/factor variables. Users need to specify the
#' level of any categorical variables to be plotted. For example, to plot the percentage of female
#' encounters, `plot_cat = "F"`.
#' Multiple categories can be provided as a vector, e.g., to plot all non-male encounters:
#' `plot_cat = c("F", "O")`. If no `plot_cat` is specified, the function will sort the unique entries of
#' the specified `plot_var` and plot the percentage of rows matching the highest value (e.g., for logical
#' variables, `% TRUE` will be plotted by default).
#' `plot_cat` can also be used for numeric variables, e.g., `plot_cat = 0` to plot encounters with Charlson
#' Comorbidity Index = 0.
#' @param show_overall (`logical`)\cr
#' Flag indicating whether to plot thick line representing overall value across hospitals.
#' @param smooth_method (`character`)\cr
#' Character specifying which smoothing method to apply (if any). By default, this is set to `NULL` (i.e.,
#' no smoothing applied). If users specify a method (e.g., `smooth_method = "glm"`), individual data
#' points will be shown as dots and the smoothed time trend will be shown as a line.
#' @param line_width (`numeric`)\cr
#' Width of individual lines. Summary line will be 2 * line_width.
#' @param ylimits (`numeric`)\cr
#' Numeric vector specifying limits for y-axis e.g. `c(0, 100)`. To specify only the lower/upper limit, use
#' `NA` (e.g., `c(NA, 100)` to fix upper limit only).
#' @param min_n (`numeric`)\cr
#' Minimum number of data points required for each hospital * time point combination. Data points with cell
#' count < `min_n` will be suppressed.
#' @param colors (`character`)\cr
#' Character vector specifying line color(s).
#' @param base_size (`numeric`)\cr
#' Numeric input to determine the base font size for each subplot in pts (default = 12)
#' @param return_data (`logical`)\cr
#' Flag indicating whether to return a list of 2 data.tables with aggregated data (\[1\] by hospital and
#' \[2\] overall). If `FALSE` (default), will return plot.
#'
#' @param ... \cr
#' If a `facet_group` is specified: Additional arguments passed to `lemon::facet_rep_wrap()` (wrapper for
#' `ggplot2::facet_wrap`), e.g., `scales = "fixed"` (default) vs. `scales = "free"`, `nrow`/`ncol` etc.
#'
#' @import ggplot2
#' @importFrom lemon facet_rep_wrap
#'
#' @return
#' By default, returns `ggplot` figure. If `return_data = TRUE`, returns
#' `data.tables` containing aggregated data.
#'
#' @seealso `vignette("plotting_data_exploration", package = "Rgemini")`
#'
#' @export
#'
plot_over_time <- function(
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
    ...) {
  ##### Check inputs #####
  if (missing(plot_var) && !grepl("^n|count", func, ignore.case = TRUE)) {
    stop(
      paste0(
        "Please provide a `plot_var` input specifying the variable you want to plot.\n",
        "Otherwise, if you want to plot the number of rows, please specify `func = 'n' (no `plot_Var` input required)."
      )
    )
  }

  # by default, use hospital_num as line_group/facet_group, unless it doesn't
  # exist in data input (in that case, check if hospital_id exists and use that)
  if (line_group == "hospital_num" && !"hospital_num" %in% colnames(data) && "hospital_id" %in% colnames(data)) {
    line_group <- "hospital_id"
  }
  if (facet_group == "hospital_num" && !"hospital_num" %in% colnames(data) && "hospital_id" %in% colnames(data)) {
    facet_group <- "hospital_id"
  }

  if (is.null(line_group) && !is.null(facet_group)) {
    line_group <- facet_group
  }
  
  Rgemini:::check_input(
    data,
    c("data.table", "data.frame"),
    colnames = c(plot_var, time_var, line_group, color_group, facet_group)
  )
  
  if (nrow(data) == 0) {
    stop(paste0("User input `", deparse(substitute(data)), "` has 0 rows. Please carefully check your input table."))
  }

  ## show warning if plot_var is the same as any of the grouping variables
  if (!is.null(plot_var) && plot_var %in% c(time_var, line_group, color_group, facet_group)) {
    warning(paste0("User-specified plot_var '", plot_var, "' is also used as a grouping variable.\n",
                   "Please check your inputs and specify a plot_var that is different from the variables used for grouping."))
  }
  
  ## When specifying a color_group, any line_group variable generally needs to be nested within color_group
  #  (e.g., hospital_num is nested within hospital_type), otherwise, color-coding doesn't make sense
  if (!is.null(line_group) && !is.null(color_group)) {
    # check for 1:1 nesting within color group
    nesting <- ipadmdad[, .(count = uniqueN(get(color_group))), by = get(line_group)]
    if (any(nesting$count > 1)) {
      warning(paste0(
        "line_group variable `", line_group, 
        "` is not fully nested within color_group `", color_group, "`.\n",
        "Note that color grouping cannot be applied within lines, but only across lines.\n", 
        "Please ensure that you specified the correct grouping variables.\n")
      )
    }
  }
  
  ##### Prepare data #####
  data <- copy(data) %>% as.data.table()

  ## Get time_int
  # if user did not provide custom time_int variable in data
  # derive time_int (by default "month")
  if (!time_int %in% colnames(data)) {
    time_label <- fix_var_str(paste(strsplit(time_var, "[_]")[[1]][1], time_int))

    # if user already provided date-time variable in POSIX/POSIXct format, keep
    # as is, otherwise, transform into appropriate format
    if (!any(grepl("POSIX", class(data[[time_var]])))) {
      data[, paste(time_var) := lubridate::parse_date_time(get(time_var), orders = c("ymd HM", "ymd HMS", "ymd"))]
    }

    if (grepl("month", time_int, ignore.case = TRUE)) {
      data[, month := lubridate::ym(format(as.Date(get(time_var), format = "%Y-%m-%d"), "%Y-%m"))]
    } else if (grepl("quarter", time_int, ignore.case = TRUE)) {
      data[, quarter := paste0(lubridate::year(get(time_var)), "-Q", lubridate::quarter(get(time_var)))]
      data[, quarter := factor(quarter, levels = unique(sort(quarter)))]
    } else if (grepl("^year", time_int, ignore.case = TRUE)) {
      data[, year := lubridate::year(get(time_var))]
    } else if (grepl("fisc_year", time_int, ignore.case = TRUE)) {
      data[, fisc_year := hospital_fiscal_year(get(time_var))]
    } else if (grepl("season", time_int, ignore.case = TRUE)) {
      data[, season := season(get(time_var))]
      data[, season := factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))]
    }
  } else {
    data[, time_int := data[[time_int]]]
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
  if (length(colors) == 1 && length(unique(data[[color_group]])) > 1) {
    colors <- rep(colors, length(unique(data[[color_group]])))
  }
  ## If not enough color values specified for all grouping levels, duplicate values
  if (!is.null(color_group) && length(unique(data[[color_group]])) > length(colors)) {
    colors <- rep_len(colors, length(unique(data[[color_group]])))
  }

  ## make sure variable for color grouping is a factor
  if (!is.null(color_group)) {
    data[[color_group]] <- as.factor(data[[color_group]])
  }

  ## check if plot_var is character/factor/logical -> plot % by default
  func <- ifelse(
    !func %in% c("count", "missing") &&
      (any(class(data[[plot_var]]) %in% c("character", "factor", "logical"))),
    "prct", func
  )

  ## if no plot_cat level specified, sort unique values and plot highest one
  if (func == "prct" && is.null(plot_cat)) {
    plot_cat <- dplyr::last(
      sort(unique(data[!n_missing(get(plot_var), na_strings = c("", " "), index = TRUE), get(plot_var)]))
    )
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
      # remove NA from denominator
      data <- data[!n_missing(get(plot_var), na_strings = c("", " "), index = TRUE), ]

      res <- data[, .(
        outcome = 100 * sum(get(plot_var) %in% plot_cat) / sum(.N),
        n = .N
      ), by = grouping]
    } else if (func == "missing") {
      res <- data[, .(
        outcome = 100 * sum(sum(n_missing(get(plot_var), na_strings = c("", " "), index = TRUE))) / sum(.N),
        n = .N
      ), by = grouping]
    }

    ## exclude observations with low cell count if min_n specified (will show up as gap on plot)
    # Note: If this means a whole combination of variables are excluded (e.g., all hospital*time combos for
    # gender = "0"), those will not be filled in again below; this is the desired behavior so fully empty
    # combos aren't shown in the plots
    if (min_n > 0 && func != "count") {
      res <- res[n >= min_n, ]
    }

    ## For any date*hosp combos that don't exist, merge and fill with NA so they correctly show up as empty on graph
    # Note: Does not include combos that don't exist at all (e.g., due to cell suppression)
    res <- droplevels(res) # drop levels that don't exist anymore at all
    append <- suppressWarnings(
      setDT(tidyr::crossing(unique(res[, ..time_int]), distinct(res[, -c(..time_int, "outcome", "n")])))
    )
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
  res <- aggregate_data(data, func, grouping)

  ## show warning if any groupings completely removed (due to cell suppression)
  check_excl <- function(var) {
    if (!is.null(var)) {
      missing <- unique(data[[var]])[!unique(data[[var]]) %in% unique(res[!is.na(outcome), get(var)])]
      if (length(missing) > 0) {
        warning(
          paste0(
            "The following levels of input variable '", var,
            "' were removed from this plot due to cell suppression (all n < ", min_n, "):"
          ),
          immediate. = TRUE
        )
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
    print(res[is.na(outcome), -c("outcome", "n")])
  }


  ## Get Overall: Aggregate data by time * group (if any, otherwise, will just aggregate across all observations)
  res_overall <- data.table()
  if (show_overall == TRUE) {
    # the only time you'd want to group by facet_group is if facet_group is different from line_group,
    # or line_group & color_group are different
    if ((!is.null(line_group) && !is.null(facet_group) && line_group != facet_group) &&
      (is.null(color_group) || (!is.null(line_group) && !is.null(color_group) && line_group != color_group))) {
      grouping_overall <- unique(c(time_int, color_group, facet_group))
    } else {
      grouping_overall <- unique(c(time_int, color_group))
    }

    if (func == "count") {
      # for count variables, "overall" line represents median of all other lines
      res_overall <- res[, .(outcome = median(outcome, na.rm = TRUE)), by = grouping_overall]
    } else {
      # for all other functions, "overall" line represents mean/median etc.
      # across all data points (per grouping var)
      res_overall <- aggregate_data(data, func, grouping_overall)
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
      output$data_aggr <- setorderv(res, grouping)
      # Include both time_var and time_int label in date column for clarity
      colnames(output$data_aggr)[colnames(output$data_aggr) == time_int] <- paste0(
        strsplit(time_var, "[_]")[[1]][1], "_", time_int
      )
    }
    if (nrow(res_overall) > 0) {
      output$data_aggr_overall <- setorderv(res_overall, grouping_overall)
      # Include both time_var and time_int label in date column for clarity
      colnames(output$data_aggr_overall)[colnames(output$data_aggr_overall) == time_int] <- paste0(
        strsplit(time_var, "[_]")[[1]][1], "_", time_int
      )
    }
    return(output)
  } else {
    ######### CREATE PLOT #########
    fig <- ggplot(
      data = res,
      aes(
        x = get(time_int), y = outcome,
        group = if (is.null(line_group)) overall_label else get(as.character(line_group)),
        color = if (is.null(color_group)) overall_label else get(color_group)
      )
    )

    ## Add overall summary line
    # NOTE: Plot this before individual lines to ensure ggplotly interprets legend order correctly
    # Label for overall summary line
    overall_label <- ifelse(func == "count", "Median", "Overall")
    # Note: If only a single site is included, overall line/legend will not be shown
    if (show_overall == TRUE && (is.null(line_group) || length(unique(res[[line_group]])) > 1)) {
      fig <- fig + suppressWarnings( # suppress warnings to ignore `method` when no smoothing is applied
        geom_line(
          data = res_overall,
          aes(
            x = get(time_int), y = outcome,
            group = if (is.null(color_group)) overall_label else get(color_group),
            color = if (is.null(color_group)) overall_label else get(color_group)
          ),
          linewidth = ifelse(
            !is.null(line_group) && !is.null(color_group) && line_group == color_group && is.null(facet_group),
            line_width,
            2 * line_width
          ),
          show.legend = ( # if aggregated overall plots are shown without individual lines, add legend for overall
            (!is.null(smooth_method) && is.null(line_group)) ||
              ((!is.null(color_group) || !is.null(line_group))) &&
                (is.null(facet_group) || (!is.null(facet_group) && color_group != facet_group))
          ),
          alpha = ifelse(is.null(facet_group) || (
            (!is.null(facet_group) && !is.null(line_group) && facet_group != line_group) &&
              (is.null(color_group) || (!is.null(line_group) && !is.null(color_group) && line_group != color_group))
          ), 1, 0.2),
          stat = if (!is.null(smooth_method)) "smooth" else "identity",
          method = smooth_method, # if smoothing, overall line will correspond to smooth line fitted to res_overall
          na.rm = TRUE
        )
      )

      fig <- fig + labs(color = NULL)
      
      
      ## overall line is only shown for neighbouring/connected data points that
      # are not interrupted by NA, otherwise, geom_line can't connect the points
      # e.g., single time point or points surrounded by NA are removed by geom_line
      # -> Identify isolated points (i.e., points surrounded by NA) and plot them
      # with geom_point() instead
      isolated_points <- res %>% 
        group_by(across(all_of(grouping_overall[grouping_overall != time_int]))) %>%
        arrange(get(time_int)) %>% 
        mutate(isolated = ifelse(
          (is.na(lag(outcome)) & is.na(lead(outcome))) |  # points surrounded by NA
            (row_number() == 1 & is.na(lead(outcome))) |  # 1st point and next = NA
            (row_number() == n() & is.na(lag(outcome))),  # last point and previous = NA
          TRUE,
          FALSE
        )) %>%
        ungroup() %>%
        filter(is.na(outcome) == FALSE & isolated == TRUE)
      
      ## add isolated points with geom_point
      fig <- fig + geom_point(
        data = isolated_points, 
        size = line_width, 
        alpha = ifelse(is.null(facet_group) || (
          (!is.null(facet_group) && !is.null(line_group) && facet_group != line_group) &&
            (is.null(color_group) || (!is.null(line_group) && !is.null(color_group) && line_group != color_group))
        ), 1, 0.2),
        show.legend = FALSE
      )
      

      # if smooth trend line is shown, but individual lines are suppressed,
      # let's also show scatter plot for overall curve
      if (!is.null(smooth_method) && is.null(line_group)) {
        fig <- fig +
          geom_point(
            data = res_overall,
            aes(
              x = get(time_int), y = outcome,
              group = if (is.null(color_group)) overall_label else get(color_group),
              color = if (is.null(color_group)) overall_label else get(color_group)
            ),
            size = 2 * line_width,
            alpha = 0.2,
            show.legend = FALSE
          )
      } 
    }


    ## Add individual hospital lines
    if (!is.null(line_group)) {
      ## If applying smooth trend line, show individual data points as dots
      if (!is.null(smooth_method)) {
        fig <- fig +
          geom_point(
            size = line_width, alpha = 0.2,
            show.legend = (!is.null(color_group) &&
              (is.null(facet_group) || ((!is.null(facet_group) && color_group != facet_group)) ||
                ((!is.null(facet_group) && !is.null(line_group) && line_group == facet_group))))
          )
      } else {
        
        ## when no smoothing is applied, individual line will be shown, however,
        # only for uninterrupted time periods that can be connected with geom_line
        # e.g., single time point or points surrounded by NA are removed by geom_line
        # -> Identify isolated points (i.e., points surrounded by NA) and plot them
        # with geom_point() instead
        isolated_points <- res %>% 
          group_by(across(all_of(grouping[grouping != time_int]))) %>%
          arrange(get(time_int)) %>% 
          mutate(isolated = ifelse(
            (is.na(lag(outcome)) & is.na(lead(outcome))) |  # points surrounded by NA
              (row_number() == 1 & is.na(lead(outcome))) |  # 1st point and next = NA
              (row_number() == n() & is.na(lag(outcome))),  # last point and previous = NA
            TRUE,
            FALSE
          )) %>%
          ungroup() %>%
          filter(is.na(outcome) == FALSE & isolated == TRUE)
        
        ## show warning
        if (nrow(isolated_points) > 0) {
          warning(paste("Due to gaps in the data timeline for certain groups,",
                        "there are some data points that can't be connected via geom_line().",
                        "These data points have been plotted with geom_point() instead.",
                        "Please consider removing categories with interrupted data availability."))
          
        }
        
        ## add isolated points with geom_point
        fig <- fig + geom_point(
          data = isolated_points, 
          size = line_width, 
          alpha = ifelse(
            show_overall && ((is.null(facet_group) ||
                                ((!is.null(facet_group) && !is.null(line_group) && (facet_group != line_group)) &&
                                   (is.null(color_group) || (
                                     (!is.null(color_group) && !is.null(line_group) && (color_group != line_group))
                                   )))) &&
                               length(unique(res[[line_group]])) > 1), 0.2, 1
          ),
          show.legend = FALSE)

      }

      fig <- fig + suppressWarnings( # suppress warnings to ignore `method` when no smoothing is applied
        geom_line(
          linewidth = line_width,
          alpha = ifelse(
            show_overall && ((is.null(facet_group) ||
              ((!is.null(facet_group) && !is.null(line_group) && (facet_group != line_group)) &&
                (is.null(color_group) || (
                  (!is.null(color_group) && !is.null(line_group) && (color_group != line_group))
                )))) &&
              length(unique(res[[line_group]])) > 1), 0.2, 1
          ),
          show.legend = (!is.null(color_group) &&
            (is.null(facet_group) || ((!is.null(facet_group) && color_group != facet_group)) ||
              ((!is.null(facet_group) && !is.null(line_group) && line_group == facet_group)))),
          stat = if (!is.null(smooth_method)) "smooth" else "identity",
          method = smooth_method, # if smooth method is specified, fit trend line according to specified method
          na.rm = TRUE
        )
      ) 
      
      

      if (!is.null(facet_group)) {
        ## show warning for facet variables with > 50 levels
        if (length(unique(data[[facet_group]])) > 50) {
          warning(
            paste0(
              "`facet_group` has more than 50 unique values.\n",
              "Please consider applying additional grouping for the facet variable to avoid a large number of subplots.\n"
            ),
            immediate. = TRUE
          )
        }

        fig <- fig +
          facet_rep_wrap(~ get(facet_group), ...) +
          theme(panel.spacing.y = unit(0, "lines"))
      }
    }


    ######### Plot Appearance #########
    ## Adjust labels & theme
    fig <- fig +
      labs(
        x = time_label,
        y = ifelse(func == "prct", paste0(fix_var_str(plot_var), " = ", paste0(plot_cat, collapse = "/"), " (%)"),
          ifelse(func == "missing", paste0(fix_var_str(plot_var), " = missing (%)"),
            ifelse(func == "count", "N",
              paste0(fix_var_str(plot_var), " (", func, ")")
            )
          )
        )
      ) +
      plot_theme(base_size = base_size) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))


    ## Adjust y-axis
    # If ylimits is specified, axis range will be fixed to that
    # otherwise, range will be expanded by 5% (unless min value for count/prct/missing outcomes is 0,
    # or max value of prct/missing outcome = 100, in which case scale is not expanded)
    # calculating this manually here so expansion can be capped
    # Note: For facet plots with free y-scales, 15% expansion is applied without any caps
    if ((is.null(ylimits) || sum(is.na(ylimits)) > 0) &&
      (is.null(facet_group) || (!is.null(facet_group) && !fig$facet$params$free$y))) {
      range <- max(res$outcome, na.rm = TRUE) - min(res$outcome, na.rm = TRUE)

      if (is.null(ylimits) || is.na(ylimits[1])) {
        ylimits[1] <- min(res$outcome, na.rm = TRUE) - range * 0.05
        if (func %in% c("count", "prct", "missing")) {
          ylimits[1] <- max(ylimits[1], 0) # make sure lower limit doesn't go below 0
        }
      }

      if (is.null(ylimits) || is.na(ylimits[2])) {
        ylimits[2] <- max(res$outcome, na.rm = TRUE) + range * 0.05
        if (func %in% c("prct", "missing")) {
          ylimits[2] <- min(ylimits[2], 100) # make sure upper limit doesn't go above 100
        }
      }
    }

    fig <- fig +
      scale_y_continuous(
        limits = ylimits,
        expand = if (!is.null(facet_group) && fig$facet$params$free$y) c(.15, .15) else c(0, 0)
      )

    ## Adjust x-axis
    if (grepl("quarter", time_int, ignore.case = TRUE)) {
      fig <- fig +
        scale_x_discrete(
          breaks = levels(data$quarter)[
            seq(1, length(levels(data$quarter)),
              by = ifelse(length(levels(data$quarter)) <= 8 && is.null(facet_group), 1,
                ifelse(length(levels(data$quarter)) <= 16, 2, 4)
              )
            )
          ],
          labels = levels(data$quarter)[
            seq(1, length(levels(data$quarter)),
              by = ifelse(length(levels(data$quarter)) <= 8 && is.null(facet_group), 1,
                ifelse(length(levels(data$quarter)) <= 16, 2, 4)
              )
            )
          ]
        )
    } else if (grepl("month", time_int, ignore.case = TRUE)) {
      fig <- fig + scale_x_date(
        breaks = seq(
          as.Date(min(data$month, na.rm = TRUE)),
          as.Date(max(data$month, na.rm = TRUE)),
          by = ifelse(
            (is.null(facet_group) && length(unique(data$month)) <= 24) ||
              (!is.null(facet_group) && length(unique(data$month)) <= 18), "1 month",
            ifelse(
              (is.null(facet_group) && length(unique(data$month)) <= 60) ||
                (!is.null(facet_group) && length(unique(data$month)) <= 45), "6 months", "1 year"
            )
          )
        ),
        date_labels = ifelse(is.null(facet_group), "%b-%Y", "%m/%y")
      )
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
