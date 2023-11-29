#' @title
#' Plot histograms of multiple variables
#'
#' @description
#' This function ...
#'
#' @param cohort (`data.frame` | `data.table`)\cr Table containing cohort data.
#' @param vars (`character`)\cr Character vector of variables to be plotted.
#'
#' @return (`ggplot`)\cr A ggplot figure containing histograms of all variables
#' specified in `vars`.

#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
#'
plot_histograms <- function(cohort, vars, binwidth) {

  plot_hist <- function(var){

    if (class(cohort[[var$varname]]) %in% c("numeric", "integer")){
      # for continuous/numeric variables
      sub_fig <- ggplot(cohort, aes(x=get(var$varname)))  +
        geom_histogram(color = "black", fill = "lightblue", binwidth = var$binwidth) +
        scale_x_continuous(breaks = seq(10,110,10)) +
        labs(title = var$varname, subtitle = paste0(
          "Median = ", round(median(cohort[[var$varname]]), digits = 2),
          " [Q1 = ", round(quantile(cohort[[var$varname]], 0.25), digits = 2),
          ", Q3 = ", round(quantile(cohort[[var$varname]], 0.75), digits = 2), "]"))

    } else if (class(cohort[[var$varname]]) %in% c("character","logical")){
      # for categorical/binary variables
      sub_fig <- ggplot(cohort, aes(x=as.factor(cohort[[var$varname]]))) +
        geom_bar(color = "black", fill = "lightblue") +
        geom_text(stat = "count", aes(label = scales::percent(round(..count../sum(..count..), digits = 3))),
                  vjust = -0.2, hjust = 0.5, size = 3) +
        labs(title = var$varname)
    }

    sub_fig <- sub_fig +
      xlab(var$varname) +
      theme_minimal(base_size = 10)

    return(sub_fig)
  }

  # if variables are provided as character vector, turn into list
  if (class(vars) == "character"){
    vars <- setNames(lapply(vars, function(x) list()), vars)
  }
  # add varnames as list item
  vars <- Map(c, vars, varname = names(vars))

  # create figure for each variable
  sub_figs <- lapply(vars, plot_hist)

  fig <- suppressWarnings(ggarrange(plotlist = sub_figs))#, ncol = 3, nrow = 2))

  return(fig)

}


#
# fig_age <- ggplot(data, aes(x=age))  +
#   geom_histogram(color = "black", fill = "lightblue", binwidth = 5) + #, binwidth = 5) +
#   scale_x_continuous(breaks = seq(10,110,10)) +
#   labs(title = "Age", subtitle = paste0(
#     "Median = ", median(data$age),
#     " [Q1 = ", quantile(data$age, 0.25),
#     ", Q3 = ", quantile(data$age, 0.75), "]")) +
#   theme_minimal(base_size = 10)
#
# fig_gender <- ggplot(data, aes(x=as.factor(gender))) +
#   geom_bar(color = "black", fill = "lightblue") +
#   geom_text(stat = "count", aes(label = scales::percent(round(..count../sum(..count..), digits = 3))),
#             vjust = -0.2, hjust = 0.5, size = 3) +
#   labs(title = "Gender") + xlab("gender") +
#   theme_minimal(base_size = 10)
#
# fig_discharge_disp <- ggplot(data, aes(x=as.factor(discharge_disposition))) +
#   geom_bar(color = "black", fill = "lightblue") +
#   geom_text(stat = "count", aes(label = scales::percent(round(..count../sum(..count..), digits = 3))),
#             vjust = -0.2, hjust = 0.5, size = 3) +
#   labs(title = "Discharge disposition") + xlab("discharge_disposition") +
#   theme_minimal(base_size = 10)
#
# # re-derive LOS based on discharge-admission date time as sanity check
# data[, los_days_derived := length_of_stay(data)$los_days_derived]
# fig_los <- ggplot(data, aes(x=los_days_derived)) +
#   geom_histogram(color = "black", fill = "lightblue", binwidth = 2.5) +
#   scale_x_continuous(breaks = seq(0,100,10), limits = c(NA, 105)) +
#   labs(title = "Length of stay (derived)", subtitle = paste0(
#     "Median = ", round(median(data$los_days_derived), digits = 2),
#     " [Q1 = ", round(quantile(data$los_days_derived, 0.25), digits = 2),
#     ", Q3 = ", round(quantile(data$los_days_derived, 0.75), digits = 2), "]")) +
#   theme_minimal(base_size = 10)
#
# fig_alc <- ggplot(data, aes(x=alc_service_transfer_flag)) +
#   geom_bar(color = "black", fill = "lightblue") +
#   geom_text(stat = "count", aes(label = scales::percent(round(..count../sum(..count..), digits = 3))),
#             vjust = -0.2, hjust = 0.5, size = 3) +
#   labs(title = "ALC transfer") + xlab("alc_service_transfer_flag") +
#   theme_minimal(base_size = 10)
#
# fig_alc_days <- ggplot(data, aes(x=number_of_alc_days)) +
#   geom_histogram(color = "black", fill = "lightblue", binwidth = 2.5) +
#   scale_x_continuous(breaks = seq(0,100,10), limits = c(NA, 105)) +
#   labs(title = "ALC days", subtitle = paste0(
#     "Median = ", round(median(data$number_of_alc_days, na.rm = TRUE), digits = 2),
#     " [Q1 = ", round(quantile(data$number_of_alc_days, na.rm = TRUE, 0.25), digits = 2),
#     ", Q3 = ", round(quantile(data$number_of_alc_days, na.rm = TRUE, 0.75), digits = 2), "]")) +
#   theme_minimal(base_size = 10)


#fig <- suppressWarnings(ggarrange(fig_age, fig_gender, fig_discharge_disp, fig_los, fig_alc, fig_alc_days,
#                                  ncol = 3, nrow = 2))
