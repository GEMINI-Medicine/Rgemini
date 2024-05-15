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
      !grepl("genc_id|patient_id|hospital_id|hospital_num|_date|_time|
             cpso|physician|adm_code_raw|dis_code_raw|mrp_code_raw",
        colnames(data),
        ignore.case = TRUE
      ) & colnales(data) != "mrp"
    ]
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
          data[!n_missing(data[[var$plot_var]], index = TRUE), ][[var$plot_var]]
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
    missing <- n_missing(data[[var$plot_var]])

    ## always exclude missing values from all plots/summary statistics
    data <- data[!n_missing(data[[var$plot_var]], index = TRUE), ]

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
          y = if (prct == TRUE) (..count..) / sum(..count..) else (..count..)
        )
      ) +
        geom_histogram(
          color = "grey20", fill = color,
          binwidth = var$binwidth, bins = var$bins, ...
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

      ## for categorical/binary variables
    } else if (any(
      var$class %in% c("character", "logical"),
      is.null(var$class) && class(data[[var$plot_var]]) %in%
        c("character", "logical")
    )) {
      ## create barplot
      sub_fig <- ggplot(
        data, aes(
          x = as.factor(data[[var$plot_var]]),
          y = if (prct == TRUE) (..count..) / sum(..count..) else (..count..)
        )
      ) +
        geom_bar(color = "grey20", fill = color)


      ## add stats/labels
      if (show_stats == TRUE) {
        sub_fig <- sub_fig +
          geom_text(
            stat = "count", aes(
              label = percent(round(..count.. / sum(..count..), 3))
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


    ## if more than 10 x-tick labels, add angle for better visibility
    if (length(ggplot_build(sub_fig)$layout$panel_params[[1]]$x$breaks) > 10) {
      sub_fig <- sub_fig +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    }

    return(sub_fig)
  }


  ######## PREPARE OUTPUT ########
  ## create figure for each variable
  sub_figs <- lapply(plot_vars, plot_subplots, data = data)

  ## Combine subplots into final figure(s)
  fig <- suppressWarnings(ggarrange(plotlist = sub_figs, ...))

  return(fig)
}