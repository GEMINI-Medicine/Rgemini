#' @title
#' Check data coverage
#'
#' @description
#' This function facilitates data coverage checks that analysts should perform
#' during data exploration & cohort generation. Specifically, this function
#' checks for "gaps" in the data timeline (e.g., are there any hospitals/time
#' periods for which GEMINI did not receive any data at all?), and also provides
#' more detailed insights into data volume & coverage by month and hospital
#' (e.g., what percentage of encounters have an entry in a given table?).
#' All checks performed by this function are applied at the table level (i.e.,
#' missing values in individual columns are not considered and should be checked
#' separately).
#'
#' @details
#' `data_coverage` provides analysts with a tool to inform their decisions
#' about which hospitals/time periods to include in their analyses,
#' depending on the data tables of interest. For example, if a project relies on
#' lab data (e.g., `mlaps` variable), users should carefully inspect lab data
#' coverage (see `plot_coverage` below). If lab data coverage is high (e.g.,
#' \>95% of `genc_ids` have an entry in the lab table), individual `genc_ids`
#' may still not exist in the lab table (e.g., because lab testing was not
#' indicated) and individual lab columns might still have missing values.
#' Users should carefully consider how to handle these cases depending on the
#' context of their analyses.
#'
#' @section Warning!:
#'
#' \itemize{
#'    \item{Data coverage checks should generally be performed on the whole
#' dataset, prior to applying any additional cohort inclusions/exclusions.}
#'    \item{If you are an HPC4Health user and your datacut has been pre-filtered
#'    based on certain inclusion/exclusion criteria (e.g., diagnosis codes),
#'    please keep in mind that the coverage plots (see `plot_coverage`) may be
#'    skewed in smaller/pre-filtered samples. Please reach out to the GEMINI
#'    team if you need additional support.}
#'    \item{Data coverage checks are particularly relevant for clinical data
#'    tables (e.g., lab, pharmacy, radiology, transfusions, vitals, clinical
#'    notes etc.).}
#'    \item{This function should be used as a starting point for data coverage
#'    checks, but users are advised to perform additional checks based on their
#'    specific needs.}
#' }
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain the following columns:
#' - `genc_id`: GEMINI Encounter ID
#' - `hospital_num` | `hospital_id`: Hospital identifier
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
#' were available for >28 consecutive days. Importantly, for time
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
#' @param hospital_label (`character`)
#' Optional: Name of variable in `cohort` table that corresponds to custom
#' label for hospitals (e.g., letters A-E instead of hospital_num 101-105).
#' Will be used for plotting purposes.
#'
#' @param hospital_group (`character`)
#' Optional: Name of variable in `cohort` table that corresponds to grouping
#' of hospitals (e.g., Teaching vs. Non-teaching). Hospitals will be grouped
#' accordingly in all plots/output tables.
#'
#' @param as_plotly (`logical`)
#' Will return any figures as interactive plots using `plotly`. Note that this
#' may affect plot aesthetics.
#' The flag will be ignored if the `plotly` package is not installed.
#'
#' @param custom_dates (`data.frame`|`data.table`)
#' Optional input allowing users to specify a customized timeline (in 'yyyy-mm-dd'
#' format) to be included for a given hospital*table combination. The user-provided
#' input overwrites the corresponding row(s) in the `lookup_data_coverage` table.
#' This can be used to exclude time periods (e.g., due to data quality
#' issues) and generate customized timeline plots.
#' For example, let's say you identified a data quality issue in the
#' transfusion table at hospital 104 for discharge dates < 2019-01-01.
#' To only include transfusion data for encounters discharged after this
#' time period, specify:
#' `custom_dates <- data.frame(
#'      data = "transfusion",
#'      hospital_num = 104,
#'      min_date = "2019-01-01",
#'      max_date = "2022-06-30"
#'  )`
#'
#' This will overwrite the `min_date`/`max_date` values for site 104
#' transfusion data in the `lookup_data_coverage` table (data for all other
#' hospital*table combinations will remain the same). Additionally, the coverage
#' flags by `genc_id` (in returned `coverage_flag_enc`) and timeline plot (see
#' `plot_timeline`) will be adjusted according to the user-provided dates.
#' The coverage plot (see `plot_coverage`) is not affected by the user-specified
#' the `custom_dates` input.
#'
#' @param ...
#' Additional inputs that can be passed to control plot aesthetics in
#' `plot_timeline` or `plot_coverage` plots, such as:
#' - `base_size`: Font size (default = 12)
#' - `colors`: Plot color(s) (default = gemini_colors(1))
#' - `hospital_group`: Name of variable in cohort specifying color grouping of
#' hospitals (e.g., Teaching/Non-teaching); for the timeline plot, this is
#' only applied when plotting a single table (otherwise, color grouping is
#' applied to different table names by default)
#'
#' For coverage plots only (inputs are passed to `plot_over_time()`):
#' - `time_int`: Time interval used to aggregate data (e.g., by `"month"`
#' = default, `"quarter"`, or `"year"`)
#' - `ylimits`: Limits of y-axis (e.g., `c(0, 50)`)
#' - `scales`: Passed to facet wrap to control if y-scales are `"fixed"`
#' (default) or `"free"` (only works if no `ylimits` specified)
#'
#' @import RPostgreSQL ggplot2
#'
#' @return
#' If the plotting flags are set to `FALSE`, this function will return a single
#' `data.table` object with a flag for each `genc_id` indicating whether the
#' encounter was discharged during a time period in which data for a given
#' table (e.g., `"lab"`) were *in principle* available. If the flag is `FALSE`,
#' the `genc_id` was dicharged during a time period where GEMINI did not receive
#' any data for the table of interest. If the flag is `TRUE`, the `genc_id` was
#' discharged during a time period where GEMINI received *some* data from a
#' given hospital (however, coverage may still be low, so users are advised to
#' perform additional coverage checks, e.g., by using the plotting features of
#' this function).
#'
#' When the plotting flags are set to `TRUE` (default), the function will
#' return additional data tables (`output[["data"]]`) and plots
#' (`output[["plots"]]`):
#'
#' - If `plot_timeline = TRUE`: Returns `timeline_plot` showing an overview of
#' data timelines by hospital for each table. Will also return the corresponding
#' data table `timeline_data` that lists the min - max dates of each table per
#' site (in long format, where tables with multiple rows per hospital indicate
#' gaps in the data timeline)
#'
#' - If `plot_coverage = TRUE`: Returns `coverage_plot` showing the percentage
#' of `genc_ids` with an entry in a given table by discharge month and hospital.
#' Will also return the plotted data as `coverage_data` to facilitate further
#' inspection.
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
#' cohort <- dbGetQuery(db, "SELECT genc_id FROM admdad;")
#'
#' ## run function with default flags to create all plots
#' # Note: This might take a while to run...
#' coverage <- data_coverage(dbcon, cohort, table = c("admdad", "radiology"))
#'
#' # get flags per encounter based on encounter's discharge date
#' enc_flag <- coverage[["data"]][1] # coverage[["data"]]$coverage_flag_enc
#'
#' # get data timeline (min-max dates)
#' data_timeline <- coverage[["data"]][2] # coverage[["data"]]$timeline_data
#'
#' # get % encounters with entry in each table by discharge month & hospital
#' prct_coverage <- coverage[["data"]][3] # coverage[["data"]]$coverage_data
#'
#'
#' ## run function without any plots
#' # (will only return data.table with encounter-level flag)
#' coverage <- data_coverage(
#'   dbcon,
#'   cohort,
#'   table = c("admdad", "radiology"),
#'   plot_timeline = FALSE,
#'   plot_coverage = FALSE
#' )
#' }
#'
#' @export
data_coverage <- function(dbcon,
                          cohort,
                          table,
                          plot_timeline = TRUE,
                          plot_coverage = TRUE,
                          hospital_label = NULL,
                          hospital_group = NULL,
                          custom_dates = NULL,
                          as_plotly = FALSE,
                          ...) {
  #########  CHECK INPUTS  #########
  # check input type and column name
  check_input(dbcon, argtype = "DBI")

  # check which variable to use as hospital identifier
  hosp_var <- return_hospital_field(dbcon)
  check_input(cohort,
    argtype = c("data.table", "data.frame"),
    colnames = c(
      "genc_id", hosp_var, "discharge_date_time",
      hospital_label, hospital_group
    )
  )
  # make copy of cohort so we don't overwrite anything
  cohort <- copy(cohort)

  # make sure hospital_group (if any) has 1-1 relationship
  # with hospital ID/num
  if (!is.null(hospital_group)) {
    if (
      # when grouping by hospital & group, should result in same
      # nrows as grouping by hospital ID alone
      nrow(cohort %>%
        dplyr::select(all_of(c(hosp_var, hospital_group))) %>%
        distinct()) != length(unique(cohort[, get(hosp_var)]))
    ) {
      stop(paste(
        "Please make sure the `hospital_group` variable is a hospital",
        "grouping variable that has a 1:1 relationship with the",
        "hospital identifier (e.g., hospital_num)."
      ))
    }
  }

  # check that custom_dates has correct format
  if (!is.null(custom_dates)) {
    check_input(custom_dates,
      argtype = c("data.table", "data.frame"),
      colnames = c(hosp_var, "data", "min_date", "max_date")
    )
    # make sure min/max date are in format that can be converted to ymd
    tryCatch(
      {
        custom_dates[, min_date := as.Date(
          convert_dt(min_date, c("ymdHMS"), truncated = 3)
        )]
        custom_dates[, max_date := as.Date(
          convert_dt(max_date, c("ymdHMS"), truncated = 3)
        )]
      },
      warning = function(w) {
        stop("Invalid format for min/max dates in `custom_dates` input.
        Please make sure dates are specified in 'yyyy-mm-dd' format (e.g., min_date = '2020-12-01').")
      }
    )
  }

  ## get optional, implicit arguments (if any)
  args <- list(...)

  ## get data coverage table (depending on DB version)
  lookup_table_name <- tryCatch(
    {
      # DB versions since drm_cleandb/report_db v3 (H4H template v4)
      find_db_tablename(dbcon, "lookup_data_coverage")
    },
    error = function(e) {
      # for older DB versions
      "mapping_files.availability_table"
    }
  )
  data_coverage_lookup <- tryCatch(
    {
      dbGetQuery(
        dbcon, paste0(
          "SELECT * FROM ", lookup_table_name,
          " WHERE ", hosp_var, " in ('",
          paste(unique(cohort[, get(hosp_var)]), collapse = "', '"),
          "');"
        )
      ) %>% data.table()
    },
    warning = function(e) {
      stop(paste(
        "The version of the database you are working with does not contain",
        "a data coverage table. Please reach out to the GEMINI team for",
        "further support."
      ))
    }
  )


  ## Preprocess lookup table
  data_coverage_lookup[, min_date := as.Date(min_date)]
  data_coverage_lookup[, max_date := as.Date(max_date)]
  # if no additional info column exists, create empty column
  if (!"additional_info" %in% colnames(data_coverage_lookup)) {
    data_coverage_lookup[, additional_info := ""]
  }

  # overwrite entries with user-specified time periods (if any)
  if (!is.null(custom_dates)) {
    custom_dates <- setDT(custom_dates)
    custom_dates <- custom_dates[
      data %in% table & get(hosp_var) %in% unique(cohort[, get(hosp_var)])
    ]

    # save additional_info column to merge back in later
    addtl_info <- data_coverage_lookup[
      data %in% table & get(hosp_var) %in% unique(cohort[[hosp_var]]) &
        !is.na(additional_info) & additional_info != ""
    ] %>%
      dplyr::select(all_of(c(hosp_var, "data", "additional_info"))) %>%
      distinct()

    # remove previous rows
    data_coverage_lookup <- anti_join(
      data_coverage_lookup, custom_dates,
      by = c("data", hosp_var)
    )

    # append customized dates
    data_coverage_lookup <- rbind(
      data_coverage_lookup, custom_dates,
      fill = TRUE
    ) %>% distinct()

    # add addtl info (if any) for overwritten entries
    data_coverage_lookup[, additional_info := addtl_info[
      .SD, additional_info,
      on = c(hosp_var, "data")
    ]]
  }

  # add _subset suffix if relevant (for HPC users)
  if (any(grepl("_subset", table))) {
    data_coverage_lookup[, data := paste(data, "_subset", sep = "")]
  }

  # if hospital_id exists and hospital_label = hospital_num, it means the user
  # may have provided a custom version of hospital_num for labelling, so we'll
  # remove it from data_coverage_lookup, just in case...
  if (all(
    hosp_var == "hospital_id", hospital_label == "hospital_num",
    "hospital_num" %in% colnames(data_coverage_lookup)
  )) {
    data_coverage_lookup <- data_coverage_lookup[, -c("hospital_num")]
  }

  # merge in hospital labels (if any) from cohort table
  if (!is.null(hospital_label)) {
    data_coverage_lookup <- merge(data_coverage_lookup,
      cohort %>% dplyr::select(all_of(c(hosp_var, hospital_label))) %>% distinct(),
      by = hosp_var
    )
  }

  # only tables that exist in availability_table are valid `table` inputs
  table <- tolower(table)
  check_input(
    table,
    argtype = "character", categories = unique(data_coverage_lookup$data)
  )
  data_coverage_lookup <- data_coverage_lookup[data %in% table, ]

  # prepare encounter-level output table
  coverage_flag_enc <- cohort %>%
    dplyr::select(all_of(
      c("genc_id", "discharge_date_time", hosp_var, hospital_label)
    )) %>%
    data.table()

  coverage_flag_enc[, discharge_date := as.Date(convert_dt(discharge_date_time))]
  coverage_flag_enc$discharge_date_time <- NULL

  #########  GET FLAG FOR EACH GENC_ID  #########
  # for each table, get min-max available date
  # if genc_id was DISCHARGED between those dates, coverage flag = TRUE

  # get min-max dates
  get_coverage_flag <- function(table) {
    # check if genc_id's discharge date falls within min-max date range
    # any genc_ids that fall within any gaps will have coverage = FALSE
    coverage_flag_enc[, paste0(table) :=
      data_coverage_lookup[data == table][
        coverage_flag_enc,
        on = c(
          hosp_var,
          "min_date <= discharge_date",
          "max_date >= discharge_date"
        ), .N, by = .EACHI
      ]$N > 0]
  }

  # Apply this to all relevant tables
  lapply(table, get_coverage_flag)

  if (all(grepl("admdad", table))) {
    # in case user runs function with "admdad" as the only table of interest
    cat(paste0(
      "\nNote: By definition, all `genc_ids` that exist in GEMINI ",
      "data have an entry in the admdad table.\n\n"
    ))
  } else {
    # show warning about interpretation of returned flags
    cat(paste0(
      "\n***** Note: *****\n",
      "The returned table by `genc_id` contains a flag indicating whether a ",
      "given encounter was discharged during a time period where *some* `",
      paste(table[!grepl("admdad", table)], collapse = "`/`"),
      "` data were available. This DOES NOT necessarily mean that each ",
      "individual `genc_id` where the flag is TRUE has an entry in the `",
      paste(table[!grepl("admdad", table)], collapse = "`/`"),
      "` table because 1) data coverage may still be low (see `plot_coverage`)",
      " and 2) for some tables, we would not expect all `genc_ids` to have an ",
      "entry even if data coverage is generally high (e.g., not all encounters ",
      "receive an imaging test so not all `genc_ids` have an entry in the ",
      "radiology table even if `radiology = TRUE` in the returned output). ",
      "Additionally, even if the data coverage flag is ",
      "`TRUE`, it does not mean that all data are available throughout the ",
      "whole length of stay of a given encounter, nor that all individual ",
      "columns are fully available/of high quality. Users are advised to ",
      "perform additional data coverage/quality checks based on their ",
      "specific needs.\n\n"
    ))

    # check N of genc_ids with coverage for all entries in "table" input
    # (ignoring admdad)
    n_enc_coverage <- sum(
      rowSums(coverage_flag_enc %>%
        dplyr::select(all_of(table[!grepl("admdad", table)]))) ==
        length(table[!grepl("admdad", table)])
    )
    p_enc_coverage <- round(100 * n_enc_coverage / lunique(cohort$genc_id), 1)
    cat(paste0(
      "In the user-provided `cohort` table, there are ",
      prettyNum(n_enc_coverage, big.mark = ","),
      " `genc_ids` (", p_enc_coverage, "%) that were discharged during time periods with coverage for ",
      ifelse(length(table[!grepl("admdad", table)]) == 1,
        paste0("the `", table[!grepl("admdad", table)], "` table"),
        ifelse(length(table[!grepl("admdad", table)]) == 2,
          paste0("both the `", paste(table[!grepl("admdad", table)],
            collapse = "` and `"
          ), "` tables"),
          paste0(
            "all of the ", length(table[!grepl("admdad", table)]),
            " tables `", paste(table[!grepl("admdad", table)],
              collapse = "`, and `"
            )
          )
        )
      ), ". "
    ))

    cat(paste0(
      "The remaining ",
      prettyNum(sum(rowSums(coverage_flag_enc %>%
        dplyr::select(all_of(table[!grepl("admdad", table)]))) <
        length(table[!grepl("admdad", table)])), big.mark = ","),
      " `genc_ids` were discharged during time periods where ",
      ifelse(length(table[!grepl("admdad", table)]) == 1,
        paste0(
          "the `", table[!grepl("admdad", table)],
          "` table did not have any data coverage."
        ),
        paste0(
          "at least 1 of the tables (`",
          paste(table[!grepl("admdad", table)], collapse = "` or `"),
          "`) did not have any data coverage."
        )
      )
    ))

    cat("\n\n")
  }

  # remove any _subset suffix from column names (for HPC users)
  setnames(
    coverage_flag_enc,
    names(coverage_flag_enc),
    gsub("_subset", "", names(coverage_flag_enc))
  )

  #########  PLOT DATA TIMELINE  #########
  # Plotting the min-max dates and illustrating major gaps
  if (plot_timeline == TRUE) {
    # prepare data for plotting
    n_tables <- length(unique(table))
    timeline_data <- data_coverage_lookup

    # For any table * hosp combos that don't exist, merge and fill with NA so
    # they correctly show up as empty on graph
    append <- setDT(
      tidyr::crossing(
        # only include hospitals that exist in cohort
        data = unique(timeline_data[, data]),
        hospital = unique(cohort[, get(hosp_var)])
      ) %>%
        distinct() %>%
        data.table()
    )
    setnames(append, "hospital", hosp_var)
    timeline_data <- merge(
      append, timeline_data,
      by = c(hosp_var, "data"), all.x = TRUE
    )

    # merge in color grouping variable (if any)
    if (!is.null(hospital_group)) {
      timeline_data <- merge(
        timeline_data, cohort %>%
          dplyr::select(all_of(c(hosp_var, hospital_group))) %>%
          distinct(),
        by = hosp_var,
        all.x = TRUE
      )
    }

    # make sure data & hospital are factors
    timeline_data[, data := factor(data, levels = unique(table))]
    if (!"factor" %in% class(timeline_data[, get(hosp_var)])) {
      timeline_data[, paste(hosp_var) := factor(
        get(hosp_var),
        levels = sort(unique(get(hosp_var)))
      )]
    }

    # offset y based on number of hospitals & tables to be plotted
    if (!is.null(hospital_group) && n_tables > 1) {
      # if grouping hospitals (and if more than 1 table), timeline plot will
      # contain facet_wrap around hospital group
      # therefore, we need to create a new index WITHIN each hospital group
      # to make sure hospitals are shown in subsequent rows within each subplot
      timeline_data[, idx := .GRP, by = c(hospital_group, hosp_var)]
      timeline_data[, idx := match(
        get(hosp_var), unique(get(hosp_var))
      ), by = hospital_group]
    } else {
      # if no hospital grouping, simply arrange hospitals by hosp_var
      # i.e., each row/bar is a single hospital*table combination
      timeline_data[, idx := get(hosp_var)]
    }
    timeline_data[, y := (
      -as.numeric(idx) - (as.numeric(data) - 1) * (2 * 0.25) /
        n_tables + (n_tables - 1) * 0.25 / n_tables
    )]

    # remove rows that are completely outside date range in cohort
    timeline_data[
      (max_date <= min(as.Date(cohort$discharge_date_time)) |
        min_date >= max(as.Date(cohort$discharge_date_time))),
      `:=`(min_date = NA, max_date = NA)
    ]

    # cap min/max dates in remaining rows according to min/max discharge dates
    timeline_data[
      min_date < min(as.Date(cohort$discharge_date_time)),
      min_date := min(as.Date(cohort$discharge_date_time))
    ]
    timeline_data[
      max_date > max(as.Date(cohort$discharge_date_time)),
      max_date := max(as.Date(cohort$discharge_date_time))
    ]

    # determine number of months to adjust breaks on x axis accordingly
    n_months <- interval(
      min(timeline_data$min_date, na.rm = TRUE),
      max(timeline_data$max_date, na.rm = TRUE)
    ) %/% months(1)

    # for edge cases where only 1-2 days are available, add 3 days for
    # plotting purposes, otherwise this doesn't show up in plot at all...
    timeline_data[, max_date_plot := max_date]
    timeline_data[
      difftime(max_date, min_date, units = "days") <= 2,
      max_date_plot := max_date + lubridate::days(3)
    ]

    # create plot
    create_timeline_plot <- function(hosp_group = NULL) {
      if (missing(hosp_group)) {
        timeline_data_subset <- timeline_data
      } else {
        timeline_data_subset <- timeline_data[
          (get(hospital_group)) == hosp_group
        ]
      }
      timeline_data_subset <- timeline_data_subset %>%
        arrange(get(hosp_var)) %>%
        data.table()

      timeline_plot <-
        ggplot(timeline_data_subset, aes(
          xmin = min_date, xmax = max_date_plot, ymin = y - 0.25 / n_tables,
          ymax = y + 0.25 / n_tables,
          fill = if (
            length(table) == 1 & !is.null(hospital_group)
          ) {
            get(hospital_group)
          } else {
            data
          },
          label = get(hosp_var),
          label2 = min_date, label3 = max_date # for ggplotly labels
        )) +
        geom_rect(show.legend = length(table) > 1 | !is.null(hospital_group)) +
        scale_y_continuous(
          name = "Hospital",
          breaks = seq(-1, -length(
            unique(as.numeric(timeline_data_subset[, get(hosp_var)]))
          ), -1),
          labels = if (is.null(hospital_label)) {
            unique(timeline_data_subset[, get(hosp_var)])
          } else {
            unique(timeline_data_subset[, get(hospital_label)])
          },
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
        scale_fill_manual(values = if (
          "colors" %in% names(args)
        ) {
          args$colors
        } else {
          gemini_colors()
        }) +
        labs(
          title = "Data Timeline by Hospital & Table\n",
          fill = if (length(table) == 1 & !is.null(hospital_group)) {
            fix_var_str(hospital_group)
          } else if (length(table) > 1) {
            "Table"
          }
        ) +
        plot_theme(
          base_size = ifelse("base_size" %in% names(args), args$base_size, 12)
        ) +
        theme(
          plot.margin = if (
            length(table) == 1 & !is.null(hospital_group)
          ) {
            margin(t = 20, 5, 5, 5)
          } else {
            (margin(5, 5, 5, 5))
          },
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.key.height = unit(0.02, "npc"),
          plot.title = element_text(vjust = if (
            length(table) == 1 & !is.null(hospital_group)
          ) {
            0.5
          } else {
            1
          })
        )
    }

    # create multiple subplots per hospital_group
    # (only if plotting multiple tables, if not, just color-code by hospital)
    if (!is.null(hospital_group) && n_tables > 1) {
      group_levels <- unique(timeline_data[, get(hospital_group)])
      # get number of hospitals per grouping level to adjust height
      # of subplots accordingly
      n_hosp <- timeline_data %>%
        group_by(get(hosp_var), get(hospital_group)) %>%
        slice(1) %>%
        group_by(get(hospital_group)) %>%
        summarize(N = as.numeric(n())) %>%
        data.table()
      # correct spacing issue: # first plot always appears to be a bit bigger...
      n_hosp[1, N := N / 1.2]

      sub_figs <- lapply(group_levels, create_timeline_plot)
      timeline_plot <- suppressWarnings(ggpubr::ggarrange(
        plotlist = sub_figs[!sapply(sub_figs, is.null)],
        ncol = 1,
        labels = unique(timeline_data[, get(hospital_group)]),
        heights = n_hosp$N
      ))
    } else {
      timeline_plot <- create_timeline_plot()
    }

    # show figure as plotly?
    if (as_plotly == TRUE && system.file(package = "plotly") != "") {
      if (!is.null(hospital_group) && n_tables > 1) {
        stop(paste(
          "Please set `as_plotly` to `FALSE` when providing",
          "a `hospital_group` input and multiple entries for `table`. The",
          "resulting plot contains multiple subplots, which is currently not",
          "supported by `plotly`."
        ))
      }
      print(plotly::ggplotly(timeline_plot))
    } else {
      if (as_plotly == TRUE && system.file(package = "plotly") == "") {
        warning(
          "Package `plotly` not installed. Returning figures as ggplots."
        )
      }
      suppressWarnings(print(timeline_plot))
    }

    # advising users to also plot coverage
    if (plot_coverage == FALSE) {
      warning(paste(
        "The \"Data Timeline\" plot only provides a rough overview of time",
        "periods with *some* available data. Please carefully inspect data",
        "coverage by running `data_coverage(..., plot_coverage = TRUE)` and",
        "check the % of encounters with data coverage per month and hospital",
        "to gain more detailed insights into potential gaps/drops in data",
        "coverage.\n\n"
      ), immediate. = TRUE)
    }
  }

  #########  PLOT % GENC_IDs WITH TABLE ENTRY BY MONTH  #########
  if (plot_coverage == TRUE) {
    cat("*** Plotting data coverage. This may take a while... ***\n")

    # write temp table to make query below more efficient
    dbExecute(dbcon, "SET client_min_messages TO WARNING;") # suppress notice
    DBI::dbSendQuery(dbcon, "Drop table if exists temp_data;")
    DBI::dbWriteTable(
      dbcon, c("pg_temp", "temp_data"), cohort[, .(genc_id)],
      row.names = FALSE, overwrite = TRUE
    )
    # Analyze speeds up the use of temp table
    DBI::dbSendQuery(dbcon, "Analyze temp_data")

    get_coverage <- function(table, cohort, ...) {
      # reset coverage flag, just in case
      cohort[, data_entry := FALSE]

      # Query unique genc_ids from table to check if genc_id exists
      cat(paste0("Querying ", table, " table...\n"))
      data_hosp <- lapply(
        unique(sort(cohort[, get(hosp_var)])), function(h, cohort, ...) {
          table_name <- find_db_tablename(dbcon, table)

          # Note: I already tested this and it seems like this query is faster
          # than using EXIST
          data_hosp <- DBI::dbGetQuery(
            dbcon, paste("SELECT DISTINCT t.genc_id FROM ", table_name, " t
                        INNER JOIN temp_data temp ON t.genc_id = temp.genc_id
                        WHERE", paste0("t.", hosp_var, " = '", h, "';"))
          ) %>%
            as.data.table()

          # filter for encounters from each hospital
          # making sure h here strictly refers to hospital variable provided
          # as input to this function (not a potential column name for a
          # hospital label the user may provide)
          cohort_hosp <- cohort %>%
            filter(get(hosp_var) == !!h)

          # add flag to cohort table by hospital
          cohort[
            genc_id %in% cohort_hosp$genc_id,
            data_entry := genc_id %in% data_hosp$genc_id
          ]
          return(cohort)
        },
        cohort = cohort
      )


      # get coverage data
      # (% encounters with an entry in a given table per month & hospital)
      coverage_data <- quiet( # don't show any warnings from plot_over_time
        plot_over_time(
          cohort,
          facet_group = if (is.null(hospital_label)) hosp_var else hospital_label,
          line_group = hosp_var,
          plot_var = "data_entry",
          plot_cat = TRUE,
          show_overall = FALSE,
          return_data = TRUE
        )[[1]]
      )
      coverage_data[, prct_data_entry_TRUE := round(prct_data_entry_TRUE, 2)]

      # plot coverage
      if (grepl("admdad", table)) { # plot raw encounter counts for admdad
        coverage_plot <- quiet( # don't show any warnings from plot_over_time
          plot_over_time(
            cohort,
            facet_group = if (is.null(hospital_label)) hosp_var else hospital_label,
            line_group = hosp_var,
            func = "n",
            show_overall = FALSE,
            min_n = 1,
            color_group = hospital_group,
            ...
          ) +
            labs(
              title = paste0("Data volume - ", table),
              y = paste0("N genc_ids in ", table, " table")
            ) +
            theme(strip.text.y = element_text(margin = margin(b = 10, t = 10)))
        )
        cat(paste0(
          "- Note: The coverage plot for `admdad` shows the raw count ",
          "(instead of %) of `genc_ids`.\n"
        ))
      } else {
        coverage_plot <- quiet( # don't show any warnings from plot_over_time
          plot_over_time(
            cohort,
            facet_group = if (is.null(hospital_label)) hosp_var else hospital_label,
            line_group = hosp_var,
            plot_var = "data_entry",
            show_overall = FALSE,
            color_group = hospital_group,
            ...
          ) +
            scale_y_continuous(
              limits = if ("ylimits" %in% names(args)) args$ylimits,
              expand = expansion(0.025)
            ) +
            labs(
              title = paste0("Data coverage - ", table),
              y = paste0("% genc_ids in ", table, " table")
            ) +
            theme(strip.text.y = element_text(margin = margin(b = 10, t = 10)))
        )
      }

      # show figure as plotly?
      if (as_plotly == TRUE && system.file(package = "plotly") != "") {
        print(
          plotly::ggplotly(coverage_plot, tooltip = c("get(time_int)", "outcome")) %>%
            plotly::style(showlegend = FALSE)
        )
      } else {
        if (as_plotly == TRUE && system.file(package = "plotly") == "") {
          warning(
            "Package `plotly` not installed. Returning figures as ggplots."
          )
        }
        print(coverage_plot)
      }

      # return data coverage table
      setnames(
        coverage_data, "prct_data_entry_TRUE", paste0("prct_", table, "_entry")
      )
      setnames(coverage_data, "n", "n_encounters")
      return(list(data = coverage_data, plot = coverage_plot))
    }

    coverage <- lapply(table, get_coverage, cohort = cohort, ...)

    # combine all columns (one per table) into single table
    if (length(table) == 1) {
      coverage_data <- coverage[[1]]$data
    } else {
      # extract coverage data from list and merge all tables into 1
      coverage_data <- do.call(function(...) {
        Reduce(function(x, y) {
          merge(
            x, y,
            by = intersect(names(x), names(y))
          )
        }, list(...))
      }, sapply(coverage, function(x) x[["data"]], simplify = FALSE))
    }
    # combine all coverage plots into single list
    coverage_plot <- sapply(coverage, function(x) x[["plot"]], simplify = FALSE)
    names(coverage_plot) <- table

    cat("\n")

    if (!all(grepl("admdad", table))) {
      warning(paste(
        "The coverage plots show data coverage by *discharge* month. For",
        "clinical variables, users are advised to also plot coverage by the",
        "respective clinical date-time variables (e.g., `collection_date_time`",
        "for lab data).\n\n"
      ), immediate. = TRUE)
    }
  }

  # print additional information from lookup table
  # (only for clean DB v3/H4H v4 and newer)
  addtl_info <- data_coverage_lookup[
    data %in% table & get(hosp_var) %in% unique(cohort[[hosp_var]]) &
      !is.na(additional_info) & additional_info != ""
  ] %>%
    dplyr::select(all_of(c(hosp_var, hospital_label, "additional_info"))) %>%
    distinct() %>%
    arrange(get(hosp_var))

  if (nrow(addtl_info) > 0) {
    cat("The following hospital-level information may be helpful when checking data coverage:\n")
    print(addtl_info)
  }

  # return relevant tables/plots based on user-provided input
  if (plot_timeline == FALSE && plot_coverage == FALSE) {
    return(coverage_flag_enc)
  } else {
    output <- list()
    output[["coverage_flag_enc"]] <- coverage_flag_enc
    if (plot_timeline == TRUE) {
      output[["timeline_data"]] <- timeline_data[, -c("y", "max_date_plot", "idx")]
      output[["timeline_plot"]] <- timeline_plot
    }
    if (plot_coverage == TRUE) {
      output[["coverage_data"]] <- coverage_data
      output[["coverage_plot"]] <- coverage_plot
    }
    return(output)
  }
}
