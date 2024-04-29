#' @title
#' Plot descriptive summary statistics of multiple variables
#'
#' @description
#' This function plots distributions (histograms/bar plots) and basic summary
#' statistics (e.g., median [Q1, Q3], % missing etc.) for multiple variables.
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
#' Character vector or list of variables to be plotted.
#'
#' @param show_stats (`logical`)\cr
#' Flag indicating whether to show descriptive stats above each plot.
#'
#' @param prct (`logical`)\cr
#' Flag indicating whether y-axis labels should show percentage (%). If `FALSE`
#' (default), counts (n) will be shown.
#'
#' @param base_size (`numeric`)\cr
#' Numeric input to determine the base size (e.g., font size) for each subplot.
#' By default, the function will automatically determine an appropriate size
#' depending on the number of subplots (base size = 11 if a single subplot).
#'
#' @param color (`character`)\cr
#' Plotting color used for "fill". Default is R's built-in `"lightblue"`.
#'
#' @param ... \cr
#' Additional arguments passed to `ggpubr::ggarrange()` that allow for finer
#' control of subplot arrangement (e.g., `ncol`, `nrow`, `widths`, `heights`).
#'
#' @return (`ggplot`)\cr A ggplot figure containing histograms of all variables
#' specified in `vars`.
#'
#' @import ggplot2 scales
#' @importFrom ggpubr ggarrange
#' @export
#'
#' @examples
#'
#' # simulate GEMINI data table
#' admdad <- dummy_data(n = 10000,
#'                          n_hospitals = 20,
#'                          time_period = c(2015, 2022)
#'                          )
#'
#' plot_summary(data = admdad,
#'              plot_vars = c("age", "gender", "discharge_disposition", "number_of_alc_days")
#'
#'
#'
#'
plot_summary <- function(data,
                         plot_vars = NULL,
                         show_stats = TRUE,
                         prct = FALSE,
                         base_size = NULL,
                         color = "lightblue",
                         ...) {

  # by default, plot all variables, except for patient/hospital identifiers or date-times
  if (is.null(plot_vars)) {
    plot_vars <- colnames(data)[
      !grepl("genc_id|patient_id|hospital_id|hospital_num|_date|_time", colnames(data), ignore.case = TRUE)
    ]
  }


  # if more than 9 variables, don't plot more than 3 columns/rows per figure,
  # unless user specified (by default, no more than 9 plots in single figure)
  args <- list(...)
  if (length(plot_vars) > 9) {
    nrow <- if (!"nrow" %in% names(args)) 3 else args$nrow
    ncol <- if (!"ncol" %in% names(args)) 3 else args$ncol
    nvars_plot <- nrow * ncol # N subplots per figure (to determine text size)
  } else {
    nvars_plot <- if ("nrow" %in% names(args) && "ncol" %in% names(args)) {
      args$nrow*args$ncol
    } else {
      length(plot_vars) # by default, all vars are in single figure
    }
  }

  # determine font size for subplots (depending on number of variables/figure)
  if (is.null(base_size)) {
    base_size <- 13 - ceiling(1.5*sqrt(nvars_plot)) # ceiling(12 / sqrt(nvars_plot))
  }


  ## plotting function
  plot_subplots <- function(var, data, n_vars) {

    ## get % missing (count any NA/""/" ")
    missing <- n_missing(data[[var$plot_var]])

    ## always exclude missing values (not included in plot/summary statistics)
    data <- data[!n_missing(data[[var$plot_var]], index = TRUE),]


    ## for continuous/numeric variables
    if (any(var$class %in% c("numeric", "integer"),
            is.null(var$class) && class(data[[var$plot_var]]) %in% c("numeric", "integer"))) {

      data[[var$plot_var]] <- as.numeric(data[[var$plot_var]])

      sub_fig <- suppressWarnings(ggplot(data, aes(x = get(var$plot_var),
                                                   y = if (prct == TRUE) (..count..)/sum(..count..) else (..count..))) +
                                    geom_histogram(color = "grey20", fill = color, binwidth = var$binwidth, bins = var$bins, ...))

      if (!is.null(var$breaks)) {
        sub_fig <- sub_fig +
          scale_x_continuous(breaks = c(var$breaks, max(var$breaks))) +
          coord_cartesian(xlim = c(min(var$breaks) - 0.05 * (max(var$breaks)-min(var$breaks)), # specify limits using coord_cartesian to preserve edge data points
                                   max(var$breaks) + 0.05 * (max(var$breaks)-min(var$breaks)))) # expand doesn't work here...
      }

      if (show_stats == TRUE) {

        if (!is.null(var$normal) && var$normal == TRUE) {
          sub_fig <- sub_fig +
            labs(subtitle = paste0(
              "Mean = ", round(mean(data[[var$plot_var]]), digits = 2),
              " (SD = ", round(sd(data[[var$plot_var]]), digits = 2), ")",
              "\nMissing: ", missing,
              "\n "))
        } else {
          sub_fig <- sub_fig +
            labs(subtitle = paste0(
              "Median = ", round(median(data[[var$plot_var]]), digits = 2),
              " [Q1 = ", round(quantile(data[[var$plot_var]], 0.25), digits = 2),
              ", Q3 = ", round(quantile(data[[var$plot_var]], 0.75), digits = 2), "]",
              "\nMissing: ", missing,
              "\n "))
        }
      }

      ## for categorical/binary variables
    } else if (any(var$class %in% c("character", "logical"),
                   is.null(var$class) && class(data[[var$plot_var]]) %in% c("character", "logical"))) {

      ## create figure
      sub_fig <- suppressWarnings(ggplot(data, aes(x = as.factor(data[[var$plot_var]]),
                                                   y = if (prct == TRUE) (..count..)/sum(..count..) else (..count..))) +
                                    geom_bar(color = "grey20", fill = color))


      ## add stats/labels
      if (show_stats == TRUE) {
        sub_fig <- sub_fig +
          geom_text(stat = "count", aes(label = percent(round(..count.. / sum(..count..), digits = 3))),
                    vjust = -0.4, size = base_size/5, hjust = 0.5) +
          labs(subtitle = paste0("Missing: ", missing, "\n "))
      }

    }

    sub_fig <- sub_fig +
      xlab(var$plot_var) + labs(title = var$varlabel) +
      scale_y_continuous(name = if (prct == TRUE) "p" else "count",
                         labels = if (prct == TRUE) percent else rescale_none,
                         expand = expansion(mult = c(0, 0.1))) +
      plot_theme(base_size = base_size)


    return(sub_fig)
  }


  ## if variables are provided as character vector, turn into list
  if (class(plot_vars) == "character") {
    plot_vars <- setNames(lapply(plot_vars, function(x) list()), plot_vars)

    ## add plot_var as list item
    plot_vars <- Map(c, plot_vars, plot_var = names(plot_vars))

    ## add varlabel as list item
    # same as plot_vars names if provided as character vector
    plot_vars <- Map(c, plot_vars, varlabel = fix_var_str(names(plot_vars)))

  } else {
    ## add varlabel as list item
    plot_vars <- Map(c, plot_vars, varlabel = names(plot_vars))

  }


  ## create figure for each variable
  sub_figs <- lapply(plot_vars, plot_subplots, data = data, n_vars = length(plot_vars))

  ## Combine subplots
  fig <- suppressWarnings(ggarrange(plotlist = sub_figs, ...))

  return(fig)

}


