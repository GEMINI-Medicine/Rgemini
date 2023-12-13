#' @title
#' Compute daily census
#'
#' @description
#' Calculates the daily number of patients that were hospitalized at each site during the specified time period.
#' The daily census is defined as a cross-sectional count of bed occupancy at a given time of the day (8am by default).
#' It is calculated as the number of patients who were admitted before and discharged after that point in time
#' (i.e., the number of patients who occupied a bed at 8am on each day).
#' Results are returned as raw counts as well as a capacity ratio (`= census/capacity`). The capacity ratio indicates
#' whether the number of hospitalized patients on a particular day was above (>1) or below (<1) the typical occupancy
#' (or max occupancy) during the time period of interest. By default, capacity is estimated as `median(census)`
#' but users can specify other measures of typical/max capacity (see `capacity_func` input). Note that if cohorts
#' are filtered/grouped based on patient characteristics (e.g., diagnosis, age etc.), this indicator simply reflects
#' a standardized measure of patient counts, rather than a measure of true capacity limitations of medical wards.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row corresponds to a single encounter.
#' Must contain the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `hospital_num` or `hospital_id` (`integer` or `character`): Hospital ID
#' - `admission_date_time` (`character`): Date-time of admission in YYYY-MM-DD HH:MM format
#' - `discharge_date_time` (`character`): Date-time of discharge in YYYY-MM-DD HH:MM format
#'
#' If a `group_var` is specified (see below), this should be included in the `cohort` table as well.
#'
#' @param time_period (`character`)
#' The start and end points of the study period, e.g., `c('2019-01-01', '2019-12-31')`.
#' Should be specified in valid date format (e.g., `'yyyy-mm-dd'`).
#'
#' @param group_var (`character`)
#' Optional input specifying one (or multiple) grouping variables. Census numbers and capacity ratio will be calculated
#' separately for each level of a given grouping variable.
#' Note: Users do not need to specify hospital as a grouping variable since the function automatically groups the output
#' by `hospital_num`. Therefore, `group_var` should only be specified if additional grouping (e.g., by different medical
#' subservices, physicians etc.) is required. Multiple grouping variables can be provided as a character vector
#' (e.g., `group_var = c("medical_subservice","physician")`).
#' If no grouping variable is specified (default: `group_var = NA`), the function returns the total daily census numbers
#' and capacity ratios per hospital.
#'
#' @param scu_exclude (`data.frame` | `data.table`)
#' Optional table containing special care unit (SCU) encounters. This is only required if patients who were in an SCU
#' should be excluded from the census calculation during time periods where they were in the SCU. The `scu_exclude`
#' table typically refers to the `ipscu` table (see
#' [GEMINI database schema](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#' However, users may want to filter the `ipscu` table by specific SCU unit numbers that should be excluded from the
#' census calculation. Entries where `scu_unit_number = 99` (`"No SCU"`) are automatically removed by this function.
#'
#' The `scu_exclude` input table needs to contain the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `scu_admit_date_time` (`character`): Date-time of SCU admission in YYYY-MM-DD HH:MM format
#' - `scu_discharge_date_time` (`character`): Date-time SCU of discharge in YYYY-MM-DD HH:MM format
#'
#' For all entries in the `scu_exclude` table that have a valid `scu_admit_date_time` and
#' `scu_discharge_date_time`, the encounter will not be counted towards the census during any time periods where
#' they were in the SCU (i.e., time periods where they did not occupy a bed in a non-SCU ward). If no input is
#' provided for this argument, no exclusion will be applied.
#'
#' @param capacity_func (`character`)
#' Optional input specifying the function to be used to define capacity for the `capacity_ratio` output.
#' Can either be a measure of central tendency to obtain typical occupancy (`median`, `mean`, or `mode`) or
#' `max` to obtain maximum occupancy. The `capacity_ratio` is calculated as `census/capacity_func(census)`.
#' Default is median (i.e., `capacity_ratio = census/median(census)`). If `"mode"` is selected and `census` has
#' multiple modes, the median of all modes is used to calculate the capacity ratio.
#'
#' Note that capacity is calculated separately for each site (and grouping variable, if any).
#' Other types of measures (e.g., trimmed means), or year-over-year capacity ratios could be calculated based on the
#' raw census counts in the output (see vignette for more details).
#'
#' @param buffer (`integer`)
#' Buffer period (in days) to be applied at the end of the data availability period. Default is 30 days, i.e., census
#' counts (and capacity ratio) are set to `NA` for the last 30 days of available data for each hospital.
#' This is only relevant if the time period of interest is towards the end of the data availability period for a given
#' hospital. For example, if `time_period = c("2020-01-01","2021-12-31")`, and data are only available until Dec 31 2021
#' for hospital A, census counts will drop sharply during the last few days in Dec 2021 due to a "truncation bias".
#' Specifically, in the example scenario, there are encounters at hospital A who were admitted prior to Dec 31 2021,
#' but have yet to be discharged. Due to the fact that encounters only appear in the GEMINI database once they have been
#' discharged, the census counts in late December will decline abruptly at hospital A because patients who have not been
#' discharged yet are not included in the database. To prevent this decrease in patient numbers from affecting census
#' counts & capacity ratio, outputs will be set to `NA` for the last `30` days at hospital A.
#'
#' The default buffer period is set to `30` because the vast majority of patients is discharged within 30 days. That is,
#' a 30-day buffer period ensures that most admitted patients have already been discharged, and thus, will be included
#' in our data. However, users may specify a different buffer period since length of stays can vary widely between
#' cohorts. Therefore, users should explore which buffer period makes most sense for the data they work with.
#'
#' Note that data availability differs across hospitals. Therefore, the actual buffer that is applied is calculated
#' separately for each hospital. If a hospital's data availability ends prior to the end of the time period of interest,
#' the buffer includes all last X days of that hospital's data (e.g., 30 days by default). If a hospital's data
#' availability goes past the end of the time period of interest, the actual buffer that is applied is based on the
#' difference between the specified buffer and the latest available discharge date at that hospital. For example, if
#' `buffer = 30` and a given hospital has more than 30 extra days of data past the time period of interest, no
#' additional buffer is applied.
#'
#' @param time_of_day (`character` | `numeric`)
#' Optional input specifying the time of the day the census/capacity ratio should be calculated at.
#' Default is 8am (`time_of_day = "8:00:00"`). Other times can be specified as a character (e.g., `"10:30"` for 10.30am)
#' or as numeric input (e.g., `14` for 2pm).
#'
#' @return (`data.table`)\cr
#' data.table with the daily counts of hospitalized patients (`census`) at each hospital. Additionally, the
#' `capacity_ratio` (`census` relative to the typical occupancy during the time period of interest) will be returned.
#' Dates within the time period of interest where no data were available at a given site are not included in the output.
#' For any dates inside the buffer period, `census` and `capacity_ratio` are returned as `NA`.
#'
#' @seealso `vignette("daily_census", package = "Rgemini")`
#'
#' @importFrom tidyr crossing
#' @importFrom dplyr anti_join
#' @importFrom lubridate parse_date_time ymd_hm hms
#'
#' @export
#'
#' @examples
#'
#' ## calculate census of all in-patient admissions (ipadm):
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'                         dbname = "db",
#'                         host = "172.XX.XX.XXX",
#'                         port = 1234,
#'                         user = getPass("Enter user:"),
#'                         password = getPass("password"))
#'
#' ipadm <- dbGetQuery(dbcon, "select * from admdad") %>% data.table()
#'
#' ipadm_census <- daily_census(ipadm)
#' }
#'

daily_census <- function(cohort, time_period = NULL, scu_exclude = NULL, group_var = NULL,
                         capacity_func = "median", buffer = 30, time_of_day = "08:00:00") {


  ## If no time_period input provided, use min/max discharge dates
  if (is.null(time_period)) {
    time_period_start <- min(as.Date(cohort$discharge_date_time))
    time_period_end <- max(as.Date(cohort$discharge_date_time))
  } else {
    tryCatch({ # if time period is provided, make sure it can be converted to date
      time_period_start <- as.Date(time_period[1])
      time_period_end <- as.Date(time_period[2])
    },
    error = function(e) {
      stop("Invalid user input for argument 'time_period'.
          Please specify the time period of interest as a character vector containing a start date [1] and an end date [2],
          specified in 'yyyy-mm-dd' format, e.g., time_period = c('2018-01-01','2019-01-01').")
    })
  }

  cat(paste0("\n*** Calculating daily census for input table ", deparse(substitute(cohort)),
             " for time period from ", time_period_start, " to ", time_period_end, " ***\n "))

  ## For internal users: if hospital_num doesn't exist, rename hospital_id to num
  if (!"hospital_num" %in% names(cohort) & "hospital_id" %in% names(cohort)){
    cohort$hospital_num <- cohort$hospital_id
  }

  #######  Check user inputs  #######
  ## check cohort input
  check_input(cohort, c("data.table", "data.frame"),
              colnames = c("genc_id", "hospital_num", "admission_date_time", "discharge_date_time"),
              coltypes = c("", "", "character", "character"),
              unique = TRUE) # make sure there are no duplicate entries in input table

  ## if grouping variable is specified, does it exist in cohort?
  if (!is.null(group_var)) {
    check_input(cohort, c("data.table", "data.frame"),
                colnames = group_var)

    # if hospital_num specified as grouping var, show warning
    if (!is.null(group_var) & any(group_var == "hospital_num")) {
      warning(paste0("Ignoring grouping variable 'hospital_num'.
    Daily census is automatically calculated separately for each hospital.
    Only specify a grouping variable if additional grouping by variables other than hospital is required."),
              immediate. = TRUE)

      # remove hospital_num from grouping variables
      group_var[group_var == "hospital_num"] <- NA
    }
  }

  ## scu_exclude provided as data.frame/data.table?
  if (!is.null(scu_exclude)){
    check_input(scu_exclude, c("data.table", "data.frame"),
                colnames = c("genc_id", "scu_admit_date_time", "scu_discharge_date_time"),
                coltypes = c("", "character", "character"))
  }

  ## Valid input for capacity_func?
  check_input(capacity_func, "character", categories = c("median", "mode", "mean","max"))

  ## Buffer needs to be non-negative integer
  check_input(buffer, "integer", interval = c(0, Inf))


  ### Additional sanity checks
  ## Check if dates specified in time period are reasonable
  if (time_period_start > time_period_end) {
    stop("Invalid user input for argument 'time_period'.
         Please make sure the start date specified in 'time_period[1]' is earlier than the end date 'time_period[2]'.")
  }
  # time_period_start needs to be >= earliest discharge date
  if (time_period_start < min(as.Date(cohort$discharge_date_time))) {
    stop(paste0("Invalid user input for argument 'time_period'.
        The start of the time period you specified (", time_period[1], ") is earlier than the earliest discharge date in the cohort (",
                min(as.Date(cohort$discharge_date_time)), ").\nPlease adjust the time period input accordingly."))
  }
  # time_period_end needs to be <= latest discharge date
  if (time_period_end > max(as.Date(cohort$discharge_date_time))) {
    stop(paste0("Invalid user input for argument 'time_period'.
        The end of the time period you specified (", time_period[2], ") is later than the latest discharge date in the cohort (",
                max(as.Date(cohort$discharge_date_time)), ").
        Please adjust the time period input accordingly."))
  }



  ## Check if valid time_of_day input
  tryCatch(
    { # try to convert input to hms format
      time_of_day <- format(parse_date_time(time_of_day, orders = c("HMS", "HM", "H")), format = "%H:%M:%S")
    },
    warning = function(e) {
      stop("Invalid user input for argument 'time_of_day'. Input cannot be converted to HMS format.
            Please refer to the function documentation for more details.")
    }
  )


  #######  Prepare data  #######
  cohort <- coerce_to_datatable(cohort)
  cohort[, hospital_num := as.factor(hospital_num)]

  ## make sure dates are in correct format
  cohort[, admission_date_time := ymd_hm(admission_date_time)]
  cohort[, discharge_date_time := ymd_hm(discharge_date_time)]

  ## Filter cohort by relevant time period
  cohort <- cohort[discharge_date_time >= time_period_start &
                     discharge_date_time <= time_period_end + hms(time_of_day), ]

  ## make these key variables to be used as 'interval' in foverlaps function below
  setkey(cohort, "admission_date_time", "discharge_date_time")

  ##  Quality check: any encounters with admission date-time > discharge date-time?
  # Note: These need to be excluded to avoid issues in foverlaps function below
  if (nrow(cohort[admission_date_time > discharge_date_time, ]) > 0) {
    warning(paste0(nrow(cohort[admission_date_time > discharge_date_time, ]),
                   " encounters with admission_date_time > discharge_date_time.
    This likely reflects a data quality issue and should only affect a small percentage of the overall cohort.
    Any encounters with admission_date_time > discharge_date_time have been excluded from the census calculation. "),
            immediate. = TRUE)
    cohort <- cohort[cohort$admission_date_time <= cohort$discharge_date_time, ]
  }



  #####  Prepare SCU data  #####
  if (!is.null(scu_exclude)) {

    cat("Applying exclusion of SCU encounters.\n")
    cat("SCU entries where 'scu_unit_number = 99' are removed from scu_exclude.\n")

    scu_exclude <- coerce_to_datatable(scu_exclude)
    scu_exclude[scu_exclude == ""] <- NA
    scu_exclude <- scu_exclude[genc_id %in% cohort$genc_id, ] # only keep genc_ids that are relevant for cohort

    ## Exclude SCU unit 99 ("no SCU")
    scu_exclude <- scu_exclude[scu_unit_number != 99, .(genc_id,scu_admit_date_time,scu_discharge_date_time)]

    ## make sure dates are in correct format
    # date formats that are missing HM information cannot be removed from census -> are set to NA
    scu_exclude[, scu_admit_date_time := ymd_hm(scu_admit_date_time, quiet = TRUE)]
    scu_exclude[, scu_discharge_date_time := ymd_hm(scu_discharge_date_time, quiet = TRUE)]

    ## check for missing/invalid SCU times
    check_scu <- scu_exclude[is.na(scu_admit_date_time) | is.na(scu_discharge_date_time) |
                             scu_admit_date_time > scu_discharge_date_time, ]
    if (nrow(check_scu) > 0) {
      warning(
        paste0("Identified "), nrow(check_scu),
        " SCU entries with invalid or missing admission or discharge date-time in scu_exclude. These entries
            cannot be excluded from the census calculation and have therefore been removed."
      )
    }

    ## remove all NAs
    scu_exclude <- scu_exclude[!is.na(scu_admit_date_time) | !is.na(scu_discharge_date_time), ]

    ## remove all times outside relevant time period of interest
    scu_exclude <- scu_exclude[scu_discharge_date_time >= time_period_start &
                             scu_admit_date_time <= time_period_end + hms(time_of_day), ]

    ## remove all where SCU discharge < admission
    scu_exclude <- scu_exclude[scu_discharge_date_time >= scu_admit_date_time, ]

    ## make SCU admit/discharge time key variables for foverlaps below
    setkey(scu_exclude, "scu_admit_date_time", "scu_discharge_date_time")
  }


  #######  Get census  #######
  ## function is applied to individual hospitals (see below)
  get_census <- function(data, time_period_start, time_period_end, group_var, scu_exclude) {

    if (nrow(data) > 0){ # skip if table is empty (can happen for some time period x hospital_num combos)

      ## Find min & max available date for each site
      min_date <- as.Date(min(data$discharge_date_time))
      max_date <- as.Date(max(data$discharge_date_time))

      ## create time series with relevant dates to search for (according to site's data avilability)
      # beginning of time period: either earliest discharge date of site or time_period_start, whichever is later
      # end of time period: either latest discharge date of site or time_period_end, whichever is earlier
      ts <- seq(max(min_date, time_period_start),
                min(max_date, time_period_end),
                by = "day") + hms(time_of_day) # add time of day (8am by default)


      ## foverlaps (below) checks for overlaps between intervals
      # (i.e., overlap between patient's stay and time interval of interest)
      # here "interval" is just a single point in time (by default: 8am each day),
      # but it needs to be duplicated in 2 columns because foverlaps expects 2 intervals...
      time_int <- data.table(date_time = ts, date_time_end = ts)
      setkey(time_int, "date_time", "date_time_end")


      ## use fast binary-search based overlap join as efficient way to check for overlap between intervals
      census <- foverlaps(time_int, data,
                          by.x = c("date_time", "date_time_end"),
                          by.y = c("admission_date_time", "discharge_date_time"),
                          nomatch = 0L,
                          type = "within", # intervals overlap if admission <= date_time AND discharge >= date_time_end (=date_time)
                          mult = "all"
      )


      ## exclude encounters who were in SCU at point in time where census was taken
      if (!is.null(scu_exclude)) {
        ## calculate SCU census
        scu_census <- foverlaps(time_int, scu_exclude[genc_id %in% census$genc_id, ],
                                by.x = c("date_time", "date_time_end"),
                                by.y = c("scu_admit_date_time", "scu_discharge_date_time"),
                                nomatch = 0L,
                                type = "within", mult = "all"
        )

        ## remove all SCU entries from census counts
        # (i.e., we don't want to count encounters that were in SCU at time census was taken)
        census <- anti_join(census, scu_census, by = c("genc_id", "date_time", "date_time_end"))
      }


      ## get census counts per date_time (+ group, if any)
      if (!is.null(group_var)) {
        census <- census[, .(census = .N), by = c("hospital_num", "date_time", group_var)]
      } else {
        census <- census[, .(census = .N), by = .(hospital_num, date_time)]
      }


      ## Fill in missing time points (if any) with 0 - different from buffer (see below)
      # This only applies to time points that are within a site's [min-max] available dates
      # in current census table, days with 0 counts don't exist (missing rows), so we'll need to fill them in and set to 0
      # (because data were in principle available, but no encounters were identified)
      # Note: this is only relevant for small cohorts/subgroups, in which case some entries may have 0 counts on some days
      # get all columns specifying grouping variables (hospital_num + group_vars)
      group_cols <- colnames(census[, -c("census", "date_time")])
      # create all combos of dates with columns specifying grouping variables
      append <- setDT(crossing(date_time = time_int$date_time, distinct(census[, ..group_cols])))
      append[, census := 0]

      ## find date-group combos that don't exist in census table
      append <- anti_join(append, census, by = colnames(append[, -c("census")]))

      ## append missing dates
      census <- rbind(census, append)[order(date_time)]


      ## Apply buffer at end of data availability period
      if (difftime(max_date, time_period_end, units = "days") <= 0) {
        # if data availability ends before/at time_period_end, apply buffer as specified
        buffer_period <- buffer
      } else if (difftime(max_date, time_period_end, units = "days") > 0) {
        # if hospital has data available past time_period_end, buffer_period = buffer minus any additional days available
        # (or 0 if hospital has more extra days available than specified as buffer period)
        buffer_period <- max(0, as.integer(buffer - difftime(max_date, time_period_end)))
      }
      # apply buffer by group (if any)
      if (!is.null(group_var)) {
        census[, `census` := replace(census, rowid(hospital_num) > (.N - buffer_period), NA), by = group_var]
      } else {
        census[, `census` := replace(census, rowid(hospital_num) > (.N - buffer_period), NA)]
      }


      ## Get capacity ratio = census/capacity where capacity is either typical or max occupancy
      # no built-in mode function, so need to define it here
      if (capacity_func == "mode") {
        central_func <- function(vals,na.rm = TRUE) {
          if (na.rm){
            vals <- vals[!is.na(vals)]
          }
          unique_vals <- unique(vals)
          tab <- tabulate(match(vals, unique_vals))
          m <- unique_vals[tab == max(tab)]

          # if multiple modes, get median
          if (length(m) > 1) {
            m <- median(m)
          }
          return(m)
        }
      } else {
        central_func <- match.fun(capacity_func)
      }

      # Note 1: 0 counts are included in capacity estimate
      # Note 2: if grouping variable is specified, capacity ratio is calculated separately for each group
      if (all(is.na(census$census))){ # there are edge cases where all entries are NA due to buffer
        census[, capacity_ratio := NA] # in that case, set capacity ratio to NA as well
      } else {
        census[, capacity_ratio := census / central_func(census,na.rm=T), by = group_cols]
      }

      return(census)
    }
  }



  #####  calculate census separately for each hospital  #####
  # note: split data by hospital before running foverlaps to avoid working with massive tables
  cohort_hospitals <- split(cohort, cohort$hospital_num)
  census <- lapply(cohort_hospitals, get_census,
                   time_period_start = time_period_start,
                   time_period_end = time_period_end,
                   group_var = group_var,
                   scu_exclude = scu_exclude
  )

  ##  Combine all
  census_all <- do.call(rbind, census)



  return(census_all)
}
