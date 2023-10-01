# https://josephcrispell.github.io/2021/07/26/creating-R-docstring.html
library(dplyr)
library(tidyselect)
library(ggplot2)
library(stringr)

#' Plotting Hospitals results by time
#'
#' Creates a ggplot of the cohort data across time for a hospital
#' @param cohort The source data
#' @param variable The variable we wish to plot by time
#' @param func The summary function used for groups (default is mean)
#' @param time_var The variable specifying time
#' @param hospital_var The variable specifying individual hospitals
#' @param hospital_group (optional) The variable used to group hospitals
#' @param title (optional) Plot title
#'
#' @return ggplot figure
plot_time_hospital <- function(
    cohort, 
    variable, 
    func=mean, 
    time_var, 
    hospital_var, 
    hospital_group,
    title
    ) {
  
  # Check arguments
  if (missing(variable)) stop("Missing the variable selection")
  if (missing(time_var)) stop("Missing time variable")
  if (missing(hospital_var)) stop("Missing the hospital identification variable")
  if (missing(hospital_group)) stop("Missing hospital group variable")

  # hospital aggregation
  hosp_vars <- c(hospital_var, time_var)
  if (!missing(hospital_group)) hosp_vars <- append(hospital_group, hosp_vars)
  print(hosp_vars)
  aggr_hosp <- cohort %>%
    group_by(across(all_of(hosp_vars))) %>%
    summarize(outcome = func(.data[[variable]], na.rm=TRUE))
  
  # Group aggregation
  if (!missing(hospital_group)) {
    aggr_group <- cohort %>%
      group_by(across(all_of(c(hospital_group, time_var)))) %>%
      summarize(outcome = func(.data[[variable]], na.rm=TRUE))
  }
  
  # Create the plot
  fig <- ggplot(
      aggr_group, 
      aes(x = Year, y = outcome, group = hospital_type, color = hospital_type)
      ) +
    geom_line(
      data = aggr_hosp,
      aes(x = Year, y = outcome, group=hospital_num, color = hospital_type),
      linewidth = 0.5, alpha = .25, show.legend = FALSE
      ) +
    geom_line(
      linewidth = 1.5, 
      show.legend = TRUE
      ) +
    labs(
      # Legend title
      colour = str_to_title(hospital_group)
      ) +
    scale_y_continuous(
      str_to_title(variable), 
      limits = c(0,max(aggr_hosp$outcome)*1.05), 
      expand = c(0,0)
      )
  
  if (!missing(title)) fig <- fig + ggtitle(title)

  return(fig)
}

