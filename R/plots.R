#' @title
#' Plot histograms of multiple variables
#'
#' @description
#' This function facilitates plotting of multiple histograms for different
#' variables at the same time.
#'
#' @section Note:
#' These plots are not meant as publication-ready figures, but rather as a quick
#' and easy means of illustrating useful distributional information about a
#' wide range of variables with a single line of code.
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
#' @return (`ggplot`)\cr A ggplot figure containing histograms of all variables
#' specified in `vars`.
#'
#' @import ggplot2
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
#' # plot histograms
#' plot_histograms(data = admdad,
#'                 plot_vars = c("age", "gender", "discharge_disposition", "number_of_alc_days")
#'
plot_histograms <- function(data, plot_vars = NULL, show_stats = TRUE) {

  if (is.null(plot_vars)){
    plot_vars <- colnames(data)[!grepl("genc_id|hospital_id|hospital_num|date_time", colnames(data))]
  }


  ## plotting function
  plot_hist <- function(var, n_vars){

    ## for continuous/numeric variables
    if (any(var$class %in% c("numeric", "integer"),
        is.null(var$class) && class(data[[var$plot_var]]) %in% c("numeric", "integer"))) {

      sub_fig <- ggplot(data, aes(x = get(var$plot_var)))  +
        geom_histogram(color = "black", fill = "lightblue", binwidth = var$binwidth) +
        labs(title = var$varlabel)

      if (!is.null(var$breaks)){
        sub_fig <- sub_fig +
          scale_x_continuous(breaks = var$breaks, limits = c(floor(min(var$breaks)-1), ceiling(max(var$breaks)+1)))
      }

      if (show_stats == TRUE) {

        if (!is.null(var$normal) && var$normal == TRUE){
          sub_fig <- sub_fig +
            labs(subtitle = paste0(
              "Mean = ", round(mean(data[[var$plot_var]], na.rm = TRUE), digits = 2),
              " (SD = ", round(sd(data[[var$plot_var]], na.rm = TRUE), digits = 2), ")",
              "\nMissing: ", n_missing(data[[var$plot_var]]),
              "\n "))
        } else {
          sub_fig <- sub_fig +
            labs(subtitle = paste0(
              "Median = ", round(median(data[[var$plot_var]], na.rm = TRUE), digits = 2),
              " [Q1 = ", round(quantile(data[[var$plot_var]], 0.25, na.rm = TRUE), digits = 2),
              ", Q3 = ", round(quantile(data[[var$plot_var]], 0.75, na.rm = TRUE), digits = 2), "]",
              "\nMissing: ", n_missing(data[[var$plot_var]]),
              "\n "))
        }
      }


      ## for categorical/binary variables
    } else if (any(var$class %in% c("character","logical"),
               is.null(var$class) && class(data[[var$plot_var]]) %in% c("character","logical"))) {

      sub_fig <- ggplot(data, aes(x=as.factor(data[[var$plot_var]]))) +
        geom_bar(color = "black", fill = "lightblue") +
        labs(title = var$varlabel)

      if (show_stats == TRUE) {
        sub_fig <- sub_fig +
          geom_text(stat = "count", aes(label = scales::percent(round(..count../sum(..count..), digits = 3))),
                                       vjust = -0.4, size = 10/n_vars, hjust = 0.5) +
          labs(subtitle = paste0("Missing: ", n_missing(data[[var$plot_var]]), "\n "))
      }

    }

    sub_fig <- sub_fig +
      xlab(var$plot_var) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      gemini_theme(base_size = ceiling(14/sqrt(n_vars)), aspect_ratio = NULL)

    return(sub_fig)
  }


  ## if variables are provided as character vector, turn into list
  if (class(plot_vars) == "character"){
    plot_vars <- setNames(lapply(plot_vars, function(x) list()), plot_vars)

    ## add plot_var as list item
    plot_vars <- Map(c, plot_vars, plot_var = names(plot_vars))

    ## add varlabel as list item
    # same as plot_vars names if provided as character vector
    plot_vars <- Map(c, plot_vars, varlabel = fix_string_label(names(plot_vars)))

  } else {
    ## add varlabel as list item
    plot_vars <- Map(c, plot_vars, varlabel = names(plot_vars))

  }


  ## create figure for each variable
  sub_figs <- lapply(plot_vars, plot_hist, n_vars = length(plot_vars))

  fig <- suppressWarnings(ggarrange(plotlist = sub_figs))

  return(fig)

}


