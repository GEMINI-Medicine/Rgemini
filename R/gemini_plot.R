# https://josephcrispell.github.io/2021/07/26/creating-R-docstring.html
library(dplyr)
library(tidyselect)
library(ggplot2)
library(stringr)

fix_string_label <- function(str) {
  str <- str_to_title(gsub("[_.]", " ", str))
}

#' Plotting Hospitals results by time
#'
#' Creates a ggplot of the cohort data across time for a hospital
#' @param cohort The source data frame
#' @param plot_variable The name of the variable we wish to plot by time
#' @param time_var The name of the variable specifying time
#' @param hospital_var The name of the variable specifying individual hospitals
#' @param show.individual.hospitals (default true) Show lines with the default hospitals
#' @param hospital_group (optional) The name of the variable used to group hospitals
#' @param title (optional) Plot title
#' @param plot_level (optional) For categorical variables, specify the level % to plot
#' @param func The summary function used for groups (default is mean)
#' @param summary.line.width (default 1.5) width of the summary line
#' @param yaxis.limits (optional) Limits for the y-axis e.g. c(0, 100)
#'
#' @import ggplot2
#'
#' @return ggplot figure
#' @export plot_time_hospital
#' 
plot_time_hospital <- function(
    cohort, 
    plot_variable, 
    time_var, 
    hospital_var, 
    hospital_group,
    title,
    yaxis.limits,
    plot_level,
    func=mean,
    show.individual.hospitals=TRUE,
    summary.line.width=1.5
) {
  
  # Check arguments
  if (missing(plot_variable)) stop("Missing the plot variable selection")
  if (missing(time_var)) stop("Missing time variable")
  if (missing(hospital_var)) stop("Missing the hospital identification variable")
 
  # hospital variables
  hosp_vars <- c(hospital_var, time_var)
  if (!missing(hospital_group)) hosp_vars <- append(hospital_group, hosp_vars)
  if (class(cohort[[plot_variable]]) == 'factor') {
    # create new variable with the factor
    if (missing(plot_level)) stop("Cannot plot factor variable without specifying plot_level")
    cohort$Percentage <- as.numeric(cohort[[plot_variable]] == plot_level)
    plot_variable <- 'Percentage'
  }
  
  # hospital aggregation
  aggr_hosp <- cohort %>%
    group_by(across(all_of(hosp_vars))) %>%
    summarize(outcome = func(.data[[plot_variable]], na.rm=TRUE))
  
  # Group aggregation
  if (!missing(hospital_group)) {
    # create aggregations by the variable named by hospital_group 
    aggr_group <- cohort %>%
      group_by(across(all_of(c(hospital_group, time_var)))) %>%
      summarize(outcome = func(.data[[plot_variable]], na.rm=TRUE))
    
    # Create the plot -- grouped by hospital group
    fig <- ggplot(
      aggr_group, 
      aes(x = get(time_var), y = outcome, group = get(hospital_group), color = get(hospital_group))
    )
    if (show.individual.hospitals) fig <- fig + geom_line(
        data = aggr_hosp,
        aes(x = get(time_var), y = outcome, group=get(hospital_var), color = get(hospital_group)),
        linewidth = 0.5, alpha = .25, show.legend = FALSE
      )    
    
  } else {
    # no aggregation by group so summarize by the average value for all obs.
    aggr_group <- cohort %>%
      group_by(across(all_of(c(time_var)))) %>%
      summarize(outcome = func(.data[[plot_variable]], na.rm=TRUE))
    
    # Create the plot -- no grouping
    fig <- ggplot(
      aggr_group, 
      aes(x = get(time_var), y = outcome)
    )
    if (show.individual.hospitals) fig <- fig + geom_line(
        data = aggr_hosp,
        aes(x = get(time_var), y = outcome, color = get(hospital_var)),
        linewidth = 0.5, alpha = .25, show.legend = FALSE
      )      
  }
  

  # Common configs for plot
  if (missing(yaxis.limits)) {
    ylimits <- c(0,max(aggr_hosp$outcome)*1.05)
  } else {
    ylimits <- yaxis.limits
  }
  
  fig <- fig + geom_line(
      # average line for the group
      linewidth = summary.line.width, 
      show.legend = !missing(hospital_group)  # only show legend if hospital_group present
      ) +
    scale_y_continuous(
      fix_string_label(plot_variable), 
      limits = ylimits, 
      expand = c(0,0)
      ) + 
    xlab(fix_string_label(time_var))
  
  if (!missing(title)) fig <- fig + ggtitle(title)
  if (!missing(hospital_group)) fig <- fig + labs(
      # Legend title
      colour = fix_string_label(hospital_group)
    )

  return(fig)
}

