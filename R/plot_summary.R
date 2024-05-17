#' @title
#' Plot descriptive summary statistics of multiple variables
#'
#' @description
#' This function plots distributions (histograms/barplots) and shows basic
#' summary statistics (e.g., median \[Q1, Q3\], % missing etc.) for multiple
#' variables.
#'
#' @section Note:
#' These plots are not meant as publication-ready figures. Instead, the goal of
#' this function is to provide a quick and easy means to visually inspect the
#' data and obtain information about distributional properties of a wide range
#' of variables, requiring just a single line of code.
#'
#' @param data (`data.frame` | `data.table`)\cr
#' Table containing data to be plotted.
#'
#' @param plot_vars (`character` | `list`)\cr
#' Character vector or list of variables to be plotted. If no `plot_vars` input
#' is provided, the function will automatically plot all variables, ignoring any
#' encounter/patient/physician IDs and date-time variables.
#'
#' @param show_stats (`logical`)\cr
#' Flag indicating whether to show descriptive stats above each plot.
#'
#' @param prct (`logical`)\cr
#' Flag indicating whether y-axis labels should show percentage (%). If `FALSE`
#' (default), counts (n) will be shown.
#'
#' @param base_size (`numeric`)\cr
#' Numeric input defining the base font size (in pts) for each subplot.
#' By default, the function will automatically determine an appropriate size
#' depending on the number of subplots (`base_size = 11` if a single subplot).
#'
#' @param color (`character`)\cr
#' Plotting color used for "fill". Default is R's built-in `"lightblue"`.
#'
#' @param ... \cr
#' Additional arguments passed to `ggpubr::ggarrange()` that allow for finer
#' control of subplot arrangement (e.g., `ncol`, `nrow`, `widths`, `heights`,
#' `align` etc.; see `? ggarrange` for more details).
#'
#' @return (`ggplot`)\cr A ggplot figure with subplots showing histograms/
#' barplots for all variables specified in `plot_vars`.
#'
#' @import ggplot2 scales
#' @importFrom ggpubr ggarrange
#' @export
#'
#' @examples
#' # simulate GEMINI data table
#' admdad <- dummy_ipadmdad(
#'   n = 10000,
#'   n_hospitals = 20,
#'   time_period = c(2015, 2022)
#' )
#'
#' plot_summary(
#'   data = admdad,
#'   plot_vars = c("age", "gender", "discharge_disposition")
#' )
#' @seealso `vignette("plotting_data_exploration", package = "Rgemini")`
#'
plot_summary <- function(data,
                         plot_vars = NULL,
                         show_stats = TRUE,
                         prct = FALSE,
                         base_size = NULL,
                         color = "lightblue",
                         ...) {
  
  ########## PREPARE INPUT FOR plot_subplots() below ########
  data <- as.data.table(data)
  ## by default, plot all variables, except ID or date-time variables
  if (is.null(plot_vars)) {
    plot_vars <- colnames(data)[
      !grepl("genc_id|patient_id|epicare|_date|date_|_time|time_|cpso",
             colnames(data),
             ignore.case = TRUE
      ) & 
        !colnames(data) %in% c("admitting_physician", "mrp",
                               "discharging_physician", "adm_code_raw",
                               "dis_code_raw", "mrp_code_raw")
    ]

    ## if data contains both hospital_id & hospital_num, plot hospital_id by default
    if ("hospital_num" %in% colnames(data) && "hospital_id" %in% colnames(data)) {
      plot_vars <- plot_vars[plot_vars != "hospital_num"]
    }

    if (length(plot_vars) < ncol(data)) {
      warning(
        paste(
          "No `plot_vars` input provided.\n",
          "Plotting all variables in `data` input, except encounter/patient/physician IDs and date-time variables.\n",
          'If you would like to plot them, please explicitly specify those variables using `plot_vars = c("variable_name")`.\n'
        ), immediate. = TRUE
      )
    }
    
    ## return error if no relevant (non-ID/date-time) variables found
    if (length(plot_vars) == 0) {
      stop(
        paste("No relevant plotting variables found.\n",
              "Please inspect your `data` input and specify the variables you would like to plot.\n")
      )
    }
    
    ## show warning for large tables
    # if no `plot_vars` input provided and table has >= 10 relevant columns
    if (length(plot_vars) >= 10) {
      warning(
        paste(
          "Plotting", length(plot_vars), "variables.",
          "This might cause memory issues and cluttered outputs.\n",
          "Please consider providing a `plot_vars` input specifying a subset of variables instead.\n"
        ), immediate. = TRUE
      )
    }
  }
  
  ## Always interpret hospital_num as a factor
  if ("hospital_num" %in% plot_vars && "hospital_num" %in% colnames(data)) {
    data[, hospital_num := factor(hospital_num, levels = sort(unique(as.numeric(hospital_num))))]
  }
  
  
  ## if variables are provided as character vector, turn into list
  if (class(plot_vars) == "character") {
    plot_vars <- setNames(lapply(plot_vars, function(x) list()), plot_vars)

    ## add plot_var as list item
    plot_vars <- Map(c, plot_vars, plot_var = names(plot_vars))

    ## add variable label as list item
    # same as plot_vars names if provided as character vector
    plot_vars <- Map(c, plot_vars, varlabel = fix_var_str(names(plot_vars)))
  } else {
    ## add variable label as list item
    plot_vars <- Map(c, plot_vars, varlabel = names(plot_vars))
  }

  ## by default, include no more than 9 plots in single figure
  # if more than 9 variables, plot multiple 3x3 figures (unless user specified
  # nrows/ncols)
  args <- list(...)
  if (length(plot_vars) > 9) {
    nrow <- if (!"nrow" %in% names(args)) 3 else args$nrow
    ncol <- if (!"ncol" %in% names(args)) 3 else args$ncol
    nvars_plot <- nrow * ncol # N subplots per figure (to determine text size)
  } else {
    nvars_plot <- if ("nrow" %in% names(args) && "ncol" %in% names(args)) {
      args$nrow * args$ncol
    } else {
      length(plot_vars) # by default, all vars are in single figure (if < 9)
    }
  }
  
  ## determine font size for subplots (depending on number of variables/figure)
  if (is.null(base_size)) {
    base_size <- 13 - ceiling(1.5 * sqrt(nvars_plot))
  }


  ########## MAIN PLOTTING FUNCTION ########
  ## create individual subplots
  plot_subplots <- function(var, data) {
    ######## CHECK & PREPROCESS DATA ########
    ## make sure specified plot_var exists in data
    if (!var$plot_var %in% colnames(data)) {
      stop(
        paste0(
          "Variable '", var$plot_var, "' does not exist in 'data' input table."
        )
      )
    }
    
    ## if user specified numeric/integer, make sure variable can be transformed
    ## to numeric (count any entries that can't be transformed as NA)
    if (any(var$class %in% c("numeric", "integer"))) {
      ## check if all (non-NA) entries can be transformed to numeric
      n_invalid <- suppressWarnings(
        sum(is.na(as.numeric(
          data[!n_missing(
            data[[var$plot_var]], na_strings = c("", " "), index = TRUE
          ), ][[var$plot_var]]
        )))
      )
      if (n_invalid > 0) {
        warning(paste0(
          n_invalid, " entries in '", var$plot_var,
          "' cannot be coerced to numeric and have been replaced with NA.
        Please carefully check your 'data' input and variable types.\n"
        ))
      }
      
      ## transform entries
      data[[var$plot_var]] <- suppressWarnings(as.numeric(data[[var$plot_var]]))
    }
    
    ## get % missing (count any NA/""/" ")
    # also includes values that couldn't be transformed to numeric above
    missing <- n_missing(data[[var$plot_var]], na_strings = c("", " "))
    
    ## always exclude missing values from all plots/summary statistics
    data <- data[
      !n_missing(data[[var$plot_var]], na_strings = c("", " "), index = TRUE), 
    ]
    
    ## check whether any non-NA values remain
    if (!length(data[[var$plot_var]]) > 0) {
      stop(
        paste(
          "No non-missing values found in variable '", var$plot_var,
          "'. Please carefully check your 'data' input."
        )
      )
    }
    
    
    ######## CREATE PLOTS ########
    ## for numeric variables
    if (any(
      var$class %in% c("numeric", "integer"),
      is.null(var$class) &&
      class(data[[var$plot_var]]) %in% c("numeric", "integer")
    )) {
      ## plot histogram
      sub_fig <- ggplot(
        data, aes(
          x = get(var$plot_var),
          y = if (prct == TRUE) (after_stat(count)) / sum(after_stat(count)) else (after_stat(count))
        )
      ) +
        suppressWarnings( # in case of additional arguments that can't be passed to geom_histogram
          geom_histogram(
            color = "grey20", fill = color,
            binwidth = var$binwidth, bins = var$bins, ...
          )
        )
      
      ## add breaks
      if (!is.null(var$breaks)) {
        sub_fig <- sub_fig +
          scale_x_continuous(breaks = c(var$breaks, max(var$breaks))) +
          # expand limits doesn't work properly here, so specify limits using
          # coord_cartesian to preserve edge data points
          coord_cartesian(
            xlim = c(
              min(var$breaks) - 0.05 * (max(var$breaks) - min(var$breaks)),
              max(var$breaks) + 0.05 * (max(var$breaks) - min(var$breaks))
            )
          )
      }
      
      ## add summary stats
      if (show_stats == TRUE) {
        if (!is.null(var$normal) && var$normal == TRUE) {
          sub_fig <- sub_fig +
            labs(subtitle = paste0(
              "Mean = ", round(mean(data[[var$plot_var]]), 2),
              " (SD = ", round(sd(data[[var$plot_var]]), 2), ")",
              "\nMissing: ", missing,
              "\n "
            ))
        } else {
          sub_fig <- sub_fig +
            labs(subtitle = paste0(
              "Median = ", round(median(data[[var$plot_var]]), 2),
              " [Q1 = ", round(quantile(data[[var$plot_var]], 0.25), 2),
              ", Q3 = ", round(quantile(data[[var$plot_var]], 0.75), 2), "]",
              "\nMissing: ", missing,
              "\n "
            ))
        }
      }
      
      ## for non-numeric variables
    } else if (any( # if variable is not numeric/user didn't specify numeric
      !var$class %in% c("integer", "numeric"), 
      is.null(var$class) && !class(data[[var$plot_var]]) %in% c("integer", "numeric")
    )) {
      
      # show warning about large number of categories
      if (length(unique(as.factor(data[[var$plot_var]]))) > 50) {
        warning(
          paste0(
            "Plotting > 50 categories on x-axis for variable '", var$plot_var, "'.\n",
            "This might take a while and can result in poor readability of the x-tick labels. ",
            "We recommend applying additional grouping before running this function ", 
            "or plotting this variable in a separate figure.\n"
          ), immediate. = TRUE
        )
      }
      
      ## create barplot
      sub_fig <- ggplot(
        data, aes(
          x = as.factor(data[[var$plot_var]]),
          y = if (prct == TRUE) (after_stat(count)) / sum(after_stat(count)) else (after_stat(count))
        )
      ) +
        geom_bar(color = "grey20", fill = color) + 
        scale_x_discrete( # limit x-tick labels to 10 characters
          label = function(x) stringr::str_trunc(x, 10)
        ) 
      
      ## add stats/labels
      if (show_stats == TRUE) {
        sub_fig <- sub_fig +
          geom_text(
            stat = "count", aes(
              label = percent(round(after_stat(count) / sum(after_stat(count)), 3))
            ),
            vjust = -0.4, size = base_size / 5, hjust = 0.5
          ) +
          labs(subtitle = paste0("Missing: ", missing, "\n\n "))
      }
    }

    ## Fix axis labels & apply plot theme
    sub_fig <- sub_fig +
      xlab(var$plot_var) + labs(title = var$varlabel) +
      scale_y_continuous(
        name = if (prct == TRUE) "p" else "count",
        labels = if (prct == TRUE) percent else rescale_none,
        expand = expansion(mult = c(0, 0.1))
      ) +
      plot_theme(base_size = base_size, aspect.ratio = 1) + 
      theme(plot.subtitle = element_text(hjust = 0))


    ## if more than 10 x-tick labels (or tick labels with > 5 characters),
    # add angle for better visibility
    if (length(ggplot_build(sub_fig)$layout$panel_params[[1]]$x$breaks) > 10 ||
        any(nchar(unique(as.character(data[[var$plot_var]])))) > 5) {
      sub_fig <- sub_fig +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }

    return(sub_fig)
  }


  ######## PREPARE OUTPUT ########
  ## create figure for each variable
  sub_figs <- lapply(plot_vars, plot_subplots, data = data)

  ## Combine subplots into final figure(s)
  if (exists("nrow", inherits = FALSE)) { # if nrow/ncol were determined within function 
    fig <- suppressWarnings(ggarrange(plotlist = sub_figs, nrow = nrow, ncol = ncol))
  } else {
    fig <- suppressWarnings(ggarrange(plotlist = sub_figs, ...))
  }
  
  return(fig)
}
