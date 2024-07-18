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
#' @param plot_timeline (`logical`)
#' Flag indicating whether to plot minimum - maximum available dates per site
#' per table.
#'
#' @param plot_coverage (`logical`)
#' Flag indicating whether to plot coverage (% `genc_ids` with entry in table).
#'
#' @param ...
#' Additional inputs that can be passed to `plot_over_time`, e.g.:
#' - `time_int = "fisc_year"` (instead of default `"month"`)
#'
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
                          plot_timeline = TRUE,
                          plot_coverage = TRUE,
                          ...) {
  #########  CHECK INPUTS  #########
  # check input type and column name
  check_input(dbcon, argtype = "DBI")

  # check which variable to use as hospital identifier
  hosp_var <- return_hospital_field(dbcon)
  check_input(cohort,
    argtype = c("data.table", "data.frame"),
    colnames = c("genc_id", hosp_var, "discharge_date_time")
  )

  # get data coverage table
  data_coverage_lookup <- dbGetQuery(
    dbcon, "SELECT * from mapping_files.availability_table;"
  ) %>% data.table()

  # only tables that exist in availability_table are valid `table` inputs
  check_input(
    table,
    argtype = "character", categories = unique(data_coverage_lookup$data)
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
    ## check if genc_ids discharge date falls within min-max date range
    # any genc_ids that fall within any gaps will have coverage = FALSE
    lookup_coverage[, paste0(table) :=
      data_coverage_lookup[data == table][
        lookup_coverage,
        on = .(
          hospital_id, min_date <= discharge_date, max_date >= discharge_date
        ), .N, by = .EACHI
      ]$N > 0]
  }

  ## Apply this to all relevant tables
  lapply(table, get_coverage_flag)

  warning(paste0(
    "The returned table contains a flag indicating whether a genc_id was discharged during a time period where `",
    paste(table, collapse = "`, `"), " `data were generally available. This DOES NOT necessarily mean that each ",
    "individual genc_id where the flag is TRUE necessarily has an entry in the `", paste(table, collapse = "`, `"),
    "` table! For example, not all encounters receive an imaging test. However, if `radiology = TRUE`, ",
    "we assume that if a `genc_id` did receive an imaging test, it would likely have an entry in the radiology ",
    "table (if not, we can assume the encounter did not receive an imaging test).\n",
    "Additionally, the flag being TRUE does not mean that all data are available throughout the whole length of ",
    "stay of a given encounter, nor that any particular individual columns are fully available/of high quality. ",
    "Users are advised to perform additional data coverage/quality checks based on their specific needs.\n\n"
  ), immediate. = TRUE)


  #########  PLOT AVAILABILITY PERIOD  #########
  if (plot_timeline == TRUE) {
    ## prepare data for plotting
    n_tables <- length(unique(table))

    plot_data <- copy(data_coverage_lookup[data %in% table, ])

    ## For any table * hosp combos that don't exist, merge and fill with NA so they correctly show up as empty on graph
    # only includes hospitals that exist in cohort
    append <- setDT(tidyr::crossing(
      data = unique(plot_data[, data]), hospital = unique(cohort$hospital_id)
    ) %>% distinct())
    plot_data <- merge(append, plot_data, by.x = c("hospital", "data"), by.y = c(hosp_var, "data"), all.x = TRUE)
    plot_data[, `:=`(hospital = as.factor(hospital), data = as.factor(data))]

    ## offset y based on number of hospitals & tables to be plotted
    plot_data[, y := (
      -as.numeric(hospital) - (as.numeric(data) - 1) * (2 * 0.25) / n_tables + (n_tables - 1) * 0.25 / n_tables
    )]

    ## plot overall coverage period (only plot those within relevant date range in cohort)
    plot_data <- plot_data[
      max_date >= min(as.Date(cohort$discharge_date_time)) & min_date <= max(as.Date(cohort$discharge_date_time))
    ]
    # for edge cases where only single date is available, add an extra 2 days,
    # otherwise, this doesn't show up in plot at all
    # -> probably should remove these entries from coverage table to begin with...
    plot_data[min_date == max_date, max_date := max_date + lubridate::days(2)]
    # cap min/max dates according to min/max discharge dates
    plot_data[min_date < min(as.Date(cohort$discharge_date_time)), min_date := min(as.Date(cohort$discharge_date_time))]
    plot_data[max_date > max(as.Date(cohort$discharge_date_time)), max_date := max(as.Date(cohort$discharge_date_time))]

    print(
      ggplot(plot_data) +
        geom_rect(
          aes(xmin = min_date, xmax = max_date, ymin = y - 0.25 / n_tables, ymax = y + 0.25 / n_tables, fill = data)
        ) +
        scale_y_continuous(
          name = "Hospital", breaks = -unique(as.numeric(plot_data$hospital)), labels = unique(plot_data$hospital),
          expand = c(0.01, 0.01)
        ) +
        scale_x_date(name = "Discharge Date", date_labels = "%b\n%Y", breaks = "6 months", expand = c(0, 0)) +
        scale_fill_manual(values = gemini_colors()) +
        labs(
          title = "Data Timeline by Hospital & Table",
          fill = "Table"
        ) +
        plot_theme(base_size = 12) +
        theme(
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.key.height = unit(0.02, "npc")
        )
    )

    warning(paste(
      "The timeline plot only provides a rough overview of time periods with available data.",
      "Please carefully inspect data coverage by running `data_coverage(..., plot_coverage = TRUE)`",
      "and carefully inspect the % of encounters with available data per month and hospital",
      "to gain more detailed insights into data coverage and potential gaps/drops.\n\n"
    ), immediate. = TRUE)
  }


  #########  PLOT % GENC_IDs WITH TABLE ENTRY BY MONTH  #########
  if (plot_coverage == TRUE) {
    cat("*** Plotting data coverage. This may take a while... ***\n")

    # write temp table to make query below more efficient
    dbExecute(dbcon, "SET client_min_messages TO WARNING;") # suppress temp_table notice
    DBI::dbSendQuery(dbcon, "Drop table if exists temp_data;")
    DBI::dbWriteTable(
      dbcon, c("pg_temp", "temp_data"), cohort[, .(genc_id)],
      row.names = FALSE, overwrite = TRUE
    )
    # Analyze speed up the use of temp table
    DBI::dbSendQuery(dbcon, "Analyze temp_data")

    plot_coverage <- function(table, cohort, ...) {
      ## reset coverage flag, just in case
      cohort[, data_entry := FALSE]

      ## Query unique genc_ids from table to check if genc_id exists
      cat(paste0("Querying ", table, " table...\n"))
      data_hosp <- lapply(unique(sort(cohort$hospital_id)), function(hospital, cohort, ...) {
        data_hosp <- DBI::dbGetQuery(
          dbcon, paste("SELECT DISTINCT t.genc_id FROM ", table, " t
                        INNER JOIN temp_data temp ON t.genc_id = temp.genc_id
                        WHERE", paste0("t.hospital_id = '", hospital, "';"))
        ) %>%
          as.data.table()

        # data_hosp <- DBI::dbGetQuery(
        #   dbcon, paste("SELECT DISTINCT t.genc_id FROM ", table, " t
        #                 INNER JOIN temp_data temp ON t.genc_id = temp.genc_id
        #                 WHERE", paste0("t.hospital_id = '", hospital, "'"),
        #                "AND exists (select 1 from temp_data c where c.genc_id = t.genc_id)", ";")) %>%
        #   as.data.table()


        cohort[hospital_id == hospital, data_entry := genc_id %in% data_hosp$genc_id]

        return(cohort)
      }, cohort = cohort)

      ## plot % genc_ids with entry in table
      capture.output(suppressWarnings(
        # don't show warnings about differences in time points here
        print(plot_over_time(cohort, plot_var = "data_entry", show_overall = FALSE, ...) +
          labs(
            title = paste0("Data coverage - ", table),
            y = paste0("% genc_ids with entry in ", table, " table")
          ) + theme(strip.text.y = element_text(margin = margin(b = 10, t = 10))))
      ))
    }

    lapply(table, plot_coverage, cohort = cohort)

    warning(paste(
      "The coverage plots show the % of encounters that have an entry in the `", paste(table, collapse = "`, `"),
      "` based on DISCHARGE month (because all GEMINI data are pulled based on patient's `discharge_date_time`).",
      "For clinical variables, users are advised to also plot coverage by the respective clinical date-time variables ",
      "(e.g., `collection_date_time` for lab data or `issue_date_time` for `transfusion` data).\n\n"
    ), immediate. = TRUE)
  }


  return(lookup_coverage)
}
