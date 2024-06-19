#' @title
#' Check data coverage
#'
#' @description
#' This function returns a flag for each `genc_id` indicating whether data for
#' a table of interest (e.g., "lab") were in principle available for that
#' `genc_id` based on a data coverage table in the database.
#'
#' @details
#'
#'
#' @section Warning:
#'
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain the following columns:
#' - `genc_id`: GEMINI Encounter ID
#' - `hospital_num` | `hosital_id`: Hospital identifier
#' - `discharge_date_time`
#'
#' @param table (`character`)
#' Which table(s) to include. If multiple, specify a character vector
#' (e.g., `table = c("lab", "pharmacy", "radiology")`).
#'
#' @import RPostgreSQL ggplot2
#'
#' @return
#' data.table object with the same number of rows as input "cohort", with
#' additional derived column(s) containing a flag indicating whether `genc_id`
#' falls within data-coverage period for the table of interest.
#'
#' @export
#'

data_coverage <- function(dbcon,
                          cohort,
                          table) {

  #########  CHECK INPUTS  #########
  # check input type and column name
  check_input(dbcon, argtype = "DBI")

  # check which variable to use as hospital identifier
  hosp_var <- return_hospital_field(dbcon)
  check_input(cohort, argtype = c("data.table", "data.frame"),
              colnames =  c("genc_id", hosp_var, "discharge_date_time"))

  # get data coverage table
  data_coverage <- dbGetQuery(
    dbcon, "SELECT * from mapping_files.availability_table;"
  ) %>% data.table()

  # only tables that exist in availability_table are valid `table` inputs
  check_input(
    table, argtype = "character", categories = unique(data_coverage$data)
  )


  ## prepare output table
  lookup_coverage <- cohort %>%
    select(all_of(c("genc_id", "discharge_date_time", hosp_var)))

  lookup_coverage[, discharge_date := as.Date(convert_dt(discharge_date_time))]
  lookup_coverage$discharge_date_time <- NULL


  #########  GET FLAG FOR EACH GENC_ID  #########
  # for each table, get min-max available date
  # if genc_id was DISCHARGED between those dates, coverage flag = TRUE
  # note: if timeline is interrupted, we'll still include all data points that
  # lie in-between the very first - very last data point)

  ## get min-max dates
  get_coverage_flag <- function(table) {
    table_coverage <- data_coverage[data == table, .(
      min_date = min(as.Date(min_date)),
      max_date = max(as.Date(max_date))
    ), by = hosp_var]

    ## check if genc_ids discharge date falls within min-max date range
    lookup_coverage[, paste0(table) :=
                    table_coverage[
                      lookup_coverage, on = .(
                        hospital_id, min_date <= discharge_date, max_date >= discharge_date
                      ), .N, by = .EACHI
                    ]$N > 0
    ]
  }

  ## Apply this to all relevant tables
  lapply(table, get_coverage_flag)


  #########  PLOT COVERAGE PERIOD  #########
  ## prepare data for plotting
  n_tables <- length(unique(table))
  plot_data <- melt.data.table(lookup_coverage,
                               id.vars = c("genc_id", hosp_var, "discharge_date"),
                               measure.vars = table,
                               variable.name = "table",
                               value.name = "coverage_flag") %>%
    filter(coverage_flag == TRUE) %>%
    group_by(hospital = get(hosp_var), table) %>%
    summarize(min = min(as.Date(discharge_date)),
              max = max(as.Date(discharge_date))) %>%
    mutate(hospital = factor(hospital),
           table = factor(table),
           y = (-as.numeric(hospital) - (as.numeric(table) - 1) * 0.5/n_tables + (n_tables - 1) * 0.25/n_tables)) %>%
    data.table()

  ## plot overall coverage period
  ggplot(plot_data) +
    geom_rect(aes(xmin = min, xmax = max, ymin = y - 0.25/n_tables, ymax = y + 0.25/n_tables, fill = table)) +
    scale_y_continuous(name = "Hospital", breaks = -unique(as.numeric(plot_data$hospital)), labels = unique(plot_data$hospital), expand = c(0.01, 0.01)) +
    scale_x_date(name = "Discharge Date", date_labels = "%b\n%Y", breaks = "6 months", expand = c(0, 0)) +
    scale_fill_manual(values = gemini_colors()) +
    labs(title = "Data Coverage", fill = "Table") +
    plot_theme(base_size = 12) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.key.height = unit(0.02, "npc"))

  return(lookup_coverage)
}
