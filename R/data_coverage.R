#' @title
#' Check data coverage
#' Check
#'
#' @description
#' This function facilitates data coverage checks that analysts should perform
#' during data exploration & cohort generation. Data coverage reflects the
#' overall volume of data per table (over time & by hospital). Specifically,
#' this function checks for "gaps" in the data timeline (e.g., are there any
#' hospitals/time periods for which GEMINI did not receive any data at all?),
#' and also provides more detailed insights into coverage by month and hospital
#' (e.g., what percentage of encounters have an entry in a given table?).
#'
#'
#' @details
#' Analysts should use the information provided by this function to inform their
#' decisions about which hospitals/time periods to include in their analyses,
#' depending on the variables of interest. For example, if analyses rely on lab
#' data (e.g., `mlaps`), users should carefully inspect lab data coverage.
#' Importantly, for time periods with high data coverage, individual
#' `genc_ids` may still have missing entries in the lab table (e.g., because
#' testing was not indicated). Users should carefully consider how to handle
#' these missing values, and whether or not they can be imputed.
#'
#'
#' @section Warning!:
#'
#' \itemize{
#'    \item{Data coverage checks should generally be performed on the whole
#' dataset, prior to applying any additional cohort inclusions/exclusions.}
#'    \item{If you are an HPC4Health user and your datacut has been pre-filtered
#'    based on certain inclusion/exclusion criteria (e.g., diagnosis codes),
#'    please keep in mind that the coverage plots (see `plot_coverage`) may be
#'    skewed in smaller/pre-filtered samples. Please review the list of known
#'    data coverage issues printed by this function, and reach out to the GEMINI
#'    team if you need additional support.}
#'    \item{Data coverage checks are particularly relevant for clinical data
#'    tables (e.g., lab, pharmacy, radiology, transfusions, vitals, clinical
#'    notes etc.).}
#'    \item{This function should be used as a starting point for data coverage
#'    checks, but users are advised to perform additional checks based on their
#'    specific needs.}
#' }
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
#' For HPC4Health users: Please specify the full table name as it is listed in
#' your datacut (e.g., `"admdad_subset"` instead of only `"admdad"`).
#'
#' @param plot_timeline (`logical`)
#' Flag indicating whether to plot an overview of data timelines by hospital and
#' table.
#' **Note:** This plot only shows a rough overview of min-max dates per table
#' for each hospital. Gaps in this plot illustrate time periods where no data
#' were available for at least 30 consecutive days. Importantly, for time
#' periods without any gaps, overall data volume may still be low and certain
#' entries/columns may still be missing. Users should further inspect the
#' coverage plots (`plot_coverage = TRUE`) and perform additional customized
#' checks based on their needs.
#'
#' @param plot_coverage (`logical`)
#' Flag indicating whether to plot data coverage. These plots show the
#' percentage of `genc_ids` with an entry in the table(s) of interest. Data
#' coverage is plotted by hospital and discharge month (because GEMINI data are
#' pulled based on encounters' discharge date). Users should carefully inspect
#' these plots for any drops/gaps in coverage that may not be visible in the
#' timeline plots (see `plot_timeline` above). When plotting the `admdad` table,
#' the function will plot the number of encounters by discharge month to show
#' overall data volume (note: by definition, 100% of `genc_ids` in GEMINI data
#' have an entry in `admdad`).
#'
#' @param ...
#' When `plot_coverage = TRUE`: Additional inputs that can be passed to
#' `Rgemini::plot_over_time()` to control plot aesthetics, e.g.:
#' - `colors`
#' - `ylimits`
#' - `base_size`
#'
#' @import RPostgreSQL ggplot2
#'
#' @return
#'
#' If the plotting flags are set to `FALSE`, this function will return a single
#' `data.table` object with a flag for each `genc_id` indicating whether the
#' encounter was discharged during a time period in which data coverage was
#' high for a given table of interest (e.g., `"lab"` table).
#'
#' When the plotting flags are set to `TRUE` (default), the function will
#' return additional tables (as a list) and plots:
#'
#' - If `plot_timeline = TRUE`: Returns a plot showing an overview of data
#' timelines by hospital for each table. Will also return the corresponding
#' table `timeline_data` that lists the min - max dates of the data timeline
#' (in long format, where multiple rows indicate gaps in the data timeline)
#'
#' - If `plot_coverage = TRUE`: Returns a plot showing the percentage of
#' `genc_ids` with an entry in a given table by discharge month and hospital.
#' Will also return the same numbers as a table called `coverage_data`.
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass("Enter user:"),
#'   password = getPass("password")
#' )
#'
#' ## run function with default flags to create all plots
#' coverage <- data_coverage(db, cohort, table = c("lab", "radiology"))
#'
#' # get flag per encounter
#' coverage_flag_enc <- coverage[[1]]
#'
#' # get data timeline (min-max dates)
#' data_timeline <- coverage[[2]]
#'
#' # get % encounters with entry in each table by discharge month & hospital
#' prct_coverage <- coverage[[3]]
#' }
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
  # add _subset suffix if relevant (for HPC users)
  if (any(grepl("_subset", table))) {
    data_coverage_lookup[, data := paste(data, "_subset", sep = "")]
  }

  # only tables that exist in availability_table are valid `table` inputs
  table <- tolower(table)
  check_input(
    table,
    argtype = "character", categories = unique(data_coverage_lookup$data)
  )


  ## prepare output table
  coverage_by_enc <- cohort %>%
    select(all_of(c("genc_id", "discharge_date_time", hosp_var)))

  coverage_by_enc[, discharge_date := as.Date(convert_dt(discharge_date_time))]
  coverage_by_enc$discharge_date_time <- NULL


  #########  GET FLAG FOR EACH GENC_ID  #########
  # for each table, get min-max available date
  # if genc_id was DISCHARGED between those dates, coverage flag = TRUE
  # note: if timeline is interrupted, we'll still include all data points that
  # lie in-between the very first - very last data point)

  ## get min-max dates
  get_coverage_flag <- function(table) {
    ## check if genc_ids discharge date falls within min-max date range
    # any genc_ids that fall within any gaps will have coverage = FALSE
    coverage_by_enc[, paste0(table) :=
                      data_coverage_lookup[data == table][
                        coverage_by_enc,
                        on = .(
                          hospital_id, min_date <= discharge_date, max_date >= discharge_date
                        ), .N, by = .EACHI
                      ]$N > 0]
  }

  ## Apply this to all relevant tables
  lapply(table, get_coverage_flag)

  if (all(grepl("admdad", table))) {
    # in case user runs function with "admdad" as the only table of interest
    cat("Note: By definition, all `genc_ids` that exist in the GEMINI data have an entry in the admdad table.\n\n")
  } else {
    cat(paste0(
      "\n***** Note: *****\n",
      "The returned table by `genc_id` contains a flag indicating whether a given encounter ",
      "was discharged during a time period where `", paste(table[!grepl("admdad", table)], collapse = "`/`"),
      "` data were generally available. This DOES NOT necessarily mean that each ",
      "individual genc_id where the flag is TRUE has an entry in the `", paste(table[!grepl("admdad", table)], collapse = "`/`"),
      "` table! As an example for the `radiology` table: Not all encounters receive an imaging test. However, if `radiology = TRUE`, ",
      "we assume that if a `genc_id` did receive an imaging test, it would likely have an entry in the radiology ",
      "table (if not, we can assume the encounter did not receive an imaging test). ",
      "Additionally, if the data coverage flag is `TRUE`, it does not mean that all data are available throughout the ",
      "whole length of stay of a given encounter, nor that all individual columns are fully available/of high quality. ",
      "Users are advised to perform additional data coverage/quality checks based on their specific needs.\n\n"
    ))

    ## check for N genc_ids with coverage for all entries in "table" input (ignoring admdad)
    cat(paste0(
      "In the user-provided `cohort` table, there are ",
      prettyNum(sum(
        rowSums(coverage_by_enc %>% select(all_of(table[!grepl("admdad", table)]))) == length(table[!grepl("admdad", table)])
      ), big.mark = ","),
      " `genc_ids` that were discharged during time periods with high coverage for ",
      ifelse(length(table[!grepl("admdad", table)]) == 1, paste0("the `", table[!grepl("admdad", table)], "` table"),
             ifelse(length(table[!grepl("admdad", table)]) == 2, paste0("both the `", paste(table[!grepl("admdad", table)], collapse = "` and `"), "` tables"),
                    paste0("all of the ", length(table[!grepl("admdad", table)]), " tables `", paste(table[!grepl("admdad", table)], collapse = "`, and `"))
             )
      ), " (out of a total N = ", prettyNum(lunique(cohort$genc_id), big.mark = ","), " encounters). "
    ))

    ## check for N genc_ids where at least 1 table doesn't have coverage (ignoring admdad)
    cat(paste0(
      prettyNum(sum(
        rowSums(coverage_by_enc %>% select(all_of(table[!grepl("admdad", table)]))) < length(table[!grepl("admdad", table)])
      ), big.mark = ","),
      " `genc_ids` that discharged during time periods where ",
      ifelse(length(table[!grepl("admdad", table)]) == 1, paste0("the `", table[!grepl("admdad", table)], "` table did not have high data coverage."),
             paste0("at least 1 of the tables (`", paste(table[!grepl("admdad", table)], collapse = "` or `"), "`) did not have high data coverage.")
      )
    ))

    cat("\n\n")
  }

  # remove any _subset suffix from column names (for HPC users)
  setnames(coverage_by_enc, names(coverage_by_enc), gsub("_subset", "", names(coverage_by_enc)))

  #########  PLOT AVAILABILITY PERIOD  #########
  if (plot_timeline == TRUE) {
    ## prepare data for plotting
    n_tables <- length(unique(table))

    timeline_data <- copy(data_coverage_lookup[data %in% table, ])

    ## For any table * hosp combos that don't exist, merge and fill with NA so they correctly show up as empty on graph
    # only includes hospitals that exist in cohort
    append <- setDT(tidyr::crossing(
      data = unique(timeline_data[, data]), hospital = unique(cohort$hospital_id)
    ) %>% distinct())
    timeline_data <- merge(append, timeline_data, by.x = c("hospital", "data"), by.y = c(hosp_var, "data"), all.x = TRUE)
    timeline_data[, `:=`(hospital = as.factor(hospital), data = as.factor(data))]

    ## offset y based on number of hospitals & tables to be plotted
    timeline_data[, y := (
      -as.numeric(hospital) - (as.numeric(data) - 1) * (2 * 0.25) / n_tables + (n_tables - 1) * 0.25 / n_tables
    )]

    ## plot overall coverage period (only plot those within relevant date range in cohort)
    timeline_data <- timeline_data[
      max_date >= min(as.Date(cohort$discharge_date_time)) & min_date <= max(as.Date(cohort$discharge_date_time))
    ]
    # for edge cases where only single date is available, add an extra 2 days,
    # otherwise, this doesn't show up in plot at all
    # -> probably should remove these entries from coverage table to begin with...
    timeline_data[min_date == max_date, max_date := max_date + lubridate::days(2)]
    # cap min/max dates according to min/max discharge dates
    timeline_data[min_date < min(as.Date(cohort$discharge_date_time)), min_date := min(as.Date(cohort$discharge_date_time))]
    timeline_data[max_date > max(as.Date(cohort$discharge_date_time)), max_date := max(as.Date(cohort$discharge_date_time))]

    # determine number of months to adjust breaks on x axis accordingly
    n_months <- interval(min(timeline_data$min_date, na.rm = TRUE), max(timeline_data$max_date, na.rm = TRUE)) %/% months(1)

    print(
      ggplot(timeline_data) +
        geom_rect(
          aes(xmin = min_date, xmax = max_date, ymin = y - 0.25 / n_tables, ymax = y + 0.25 / n_tables, fill = data)
        ) +
        scale_y_continuous(
          name = "Hospital", breaks = -unique(as.numeric(timeline_data$hospital)), labels = unique(timeline_data$hospital),
          expand = c(0.01, 0.01)
        ) +
        scale_x_date(
          name = "Discharge Date",
          date_labels = "%b %Y",
          breaks = ifelse(n_months <= 12, "1 month",
                          ifelse(n_months > 12 & n_months <= 48, "3 months",
                                 ifelse(n_months > 24 & n_months <= 96, "6 months",
                                        "1 year"
                                 )
                          )
          ),
          expand = c(0, 0)
        ) +
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

    if (plot_coverage == FALSE) {
      warning(paste(
        "The \"Data Timeline\" plot only provides a rough overview of time periods with available data.",
        "Please carefully inspect data coverage by running `data_coverage(..., plot_coverage = TRUE)`",
        "and check the % of encounters with available data per month and hospital",
        "to gain more detailed insights into data coverage and potential gaps/drops.\n\n"
      ), immediate. = TRUE)
    }
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
    # Analyze speeds up the use of temp table
    DBI::dbSendQuery(dbcon, "Analyze temp_data")

    get_coverage <- function(table, cohort, ...) {
      ## reset coverage flag, just in case
      cohort[, data_entry := FALSE]

      ## Query unique genc_ids from table to check if genc_id exists
      cat(paste0("Querying ", table, " table...\n"))
      data_hosp <- lapply(unique(sort(cohort$hospital_id)), function(hospital, cohort, ...) {
        ## Note: I already tested this and it seems like this query is faster than using EXIST
        data_hosp <- DBI::dbGetQuery(
          dbcon, paste("SELECT DISTINCT t.genc_id FROM ", table, " t
                        INNER JOIN temp_data temp ON t.genc_id = temp.genc_id
                        WHERE", paste0("t.hospital_id = '", hospital, "';"))
        ) %>%
          as.data.table()

        cohort[hospital_id == hospital, data_entry := genc_id %in% data_hosp$genc_id]

        return(cohort)
      }, cohort = cohort)


      ## get coverage data (% encounters with an entry in a given table per month & hospital)
      coverage_data <- quiet(
        plot_over_time(cohort, facet_group = hosp_var, line_group = hosp_var, plot_var = "data_entry", show_overall = FALSE, return_data = TRUE)[[1]]
      )
      coverage_data[, prct_data_entry_TRUE := round(prct_data_entry_TRUE, 2)]

      ## plot coverage
      if (grepl("admdad", table)) { # plot raw encounter counts for admdad
        print(quiet( # don't show any warnings about differences in time points here
          plot_over_time(cohort, facet_group = hosp_var, line_group = hosp_var, func = "n", show_overall = FALSE, min_n = 1, ...) +
            labs(
              title = paste0("Data coverage - ", table),
              y = paste0("N genc_ids in ", table, " table")
            ) +
            scale_y_continuous(expand = expansion(ifelse(lunique(na.omit(coverage_data$prct_data_entry_TRUE)) == 1, 0.01, 0))) +
            theme(strip.text.y = element_text(margin = margin(b = 10, t = 10)))
        ))
        cat("...note: By definition, 100% of `genc_ids` have an entry in the admdad table. Plotting the number of encounters in `admdad` instead...\n")
      } else {
        print(quiet( # don't show any warnings about differences in time points here
          plot_over_time(cohort, facet_group = hosp_var, line_group = hosp_var, plot_var = "data_entry", show_overall = FALSE, ...) +
            labs(
              title = paste0("Data coverage - ", table),
              y = paste0("% genc_ids with entry in ", table, " table")
            ) +
            scale_y_continuous(expand = expansion(ifelse(lunique(na.omit(coverage_data$prct_data_entry_TRUE)) == 1, 0.01, 0))) +
            theme(strip.text.y = element_text(margin = margin(b = 10, t = 10)))
        ))
      }

      # return data coverage table
      setnames(coverage_data, "prct_data_entry_TRUE", paste0("prct_", table, "_entry"))
      setnames(coverage_data, "n", "n_encounters")
      return(coverage_data)
    }

    coverage_data <- lapply(table, get_coverage, cohort = cohort)
    coverage_data <- Reduce(
      function(x, y) merge(x, y, by = c(hosp_var, "discharge_month", "n_encounters"), all.x = TRUE, all.y = TRUE), coverage_data
    )

    cat("\n")
    warning(paste0(
      "The coverage plots show data coverage by *discharge* month (because all GEMINI data are pulled based on patient's `discharge_date_time`).",
      "For clinical variables, users are advised to also plot coverage by the respective clinical date-time variables ",
      "(e.g., `collection_date_time` for lab data or `issue_date_time` for `transfusion` data).\n\n"
    ), immediate. = TRUE)
  }

  ## return relevant tables based on flags
  if (plot_timeline == FALSE && plot_coverage == FALSE) {
    return(coverage_by_enc)
  } else {
    output <- list(coverage_by_enc = coverage_by_enc)
    if (plot_timeline == TRUE) {
      output <- append(output, list(timeline_data = timeline_data[, -("y")]))
    }
    if (plot_coverage == TRUE) {
      output <- append(output, list(coverage_data = coverage_data))
    }
    return(output)
  }
}
