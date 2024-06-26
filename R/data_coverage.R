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
#' @param plot_minmax (`logical`)
#' Flag indicating whether to plot minimum - maximum available dates per site
#' per table.
#'
#' @param plot_coverage (`logical`)
#' Flag indicating whether to plot coverage (% `genc_ids` with entry in table).
#'
#' @import RPostgreSQL ggplot2
#' @importFrom purrr map2_df
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
                          table,
                          plot_minmax = TRUE,
                          plot_coverage = TRUE) {

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


  #########  PLOT AVAILABILITY PERIOD  #########
  if (plot_minmax == TRUE) {

    ## prepare data for plotting
    n_tables <- length(unique(table))

    ## transform to long format to plot min-max date per table
    plot_data <- melt.data.table(
      lookup_coverage,
      id.vars = c("genc_id", hosp_var, "discharge_date"),
      measure.vars = table,
      variable.name = "table",
      value.name = "coverage_flag"
    )[coverage_flag == TRUE, ]

    plot_data[, `:=` (hospital = as.factor(get(hosp_var)), table = as.factor(table))]

    plot_data <- plot_data[, .(
      min = min(as.Date(discharge_date)),
      max = max(as.Date(discharge_date))
    ), by = c("hospital", "table")]

    ## offset y based on number of hospitals & tables to be plotted
    plot_data[, y := (
      -as.numeric(hospital) - (as.numeric(table) - 1) * 0.5 / n_tables + (n_tables - 1) * 0.25 / n_tables
    )]

    ## plot overall coverage period
    print(
      ggplot(plot_data) +
        geom_rect(aes(xmin = min, xmax = max, ymin = y - 0.25/n_tables, ymax = y + 0.25/n_tables, fill = table)) +
        scale_y_continuous(name = "Hospital", breaks = -unique(as.numeric(plot_data$hospital)), labels = unique(plot_data$hospital), expand = c(0.01, 0.01)) +
        scale_x_date(name = "Discharge Date", date_labels = "%b\n%Y", breaks = "6 months", expand = c(0, 0)) +
        scale_fill_manual(values = gemini_colors()) +
        labs(
          title = "Data Availability by Hospital & Table",
          caption = paste0("Note: This plot only provides a rough overview of the min-max available dates per site and table. Please carefully inspect data coverage\n",
                           "within each data availability period to gain more detailed insights into data coverage and potential gaps in data availability."),
          fill = "Table") +
        plot_theme(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.key.height = unit(0.02, "npc"),
          plot.caption = element_text(hjust = 0, face = "italic", color = "red"))
    )

  }

  #########  PLOT % GENC_IDs WITH TABLE ENTRY By MONTH  #########
  if (plot_coverage == TRUE) {

    cat("*** Plotting data coverage. This may take a while... ***")

    # write temp table to make query below more efficient
    suppressWarnings(DBI::dbSendQuery(dbcon,"Drop table if exists temp_data;"))
    DBI::dbWriteTable(dbcon, c("pg_temp","temp_data"), cohort[,.(genc_id)], row.names = F, overwrite = T)


    plot_coverage <- function(table, cohort) {

      ## reset coverage flag, just in case
      cohort[, data_entry := FALSE]

      # dt_var <- case_when(
      #   grepl(table %in% c("lab", "lab_subset")) ~ "collection_date_time",
      #   grepl(table %in% c("pharmacy", "pharmacy_subset")) ~ "order_date_time",
      #   grepl(table %in% c("transfusion", "transfusion_subset")) ~ "issue_date_time",
      #   .default = ""
      # )

      ## Query unique genc_ids from table to check if genc_id exists
      cat(paste0("\nQuerying ", table, " table..."))
      data_hosp <- lapply(unique(sort(cohort$hospital_id)), function(hospital, cohort) {

        data_hosp <- DBI::dbGetQuery(
          dbcon, paste("SELECT DISTINCT t.genc_id FROM ", table, " t
                        INNER JOIN temp_data temp ON t.genc_id = temp.genc_id
                        WHERE", paste0("t.hospital_id = '", hospital, "';"))) %>%
          as.data.table()

        cohort[hospital_id == hospital, data_entry := genc_id %in% data_hosp$genc_id]

        return(cohort)

      }, cohort = cohort)

      ## plot % genc_ids with entry in table
      suppressWarnings(invisible(capture.output( # don't show warnings about differences in time points here
        plot_over_time(cohort, plot_var = "data_entry", ylimits = c(-2, 102)) +
          labs(title = paste0("Data coverage - ", table), y = paste0("% genc_ids with entry in ", table, " table")), type = c("output", "message")
      )))
    }

    lapply(table, plot_coverage, cohort = cohort)
  }


  return(lookup_coverage)
}
