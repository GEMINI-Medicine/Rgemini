#' @title
#' Compute entry to Intensive Care Unit
#'
#' @description
#' Determines whether an encounter has entered Intensive Care Unit (ICU) during
#' hospital stay using CIHI Discharge Abstract Database (DAD) fields.
#'
#' @details
#' This function uses DAD fields Admission date-time (Group 04 Fields 01/02),
#' and SCU Admit date-time (Group 13, Field 03/04) to derive boolean fields
#' indicating ICU entries at any time during hospital stay,
#' and within specified time window since hospital admission.
#'
#' By definition in DAD (Group 13, Field 02), SCUs include ICUs and Step-Down Units.
#' Step-Down Units are not considered as ICUs.
#'
#' Therefore, this function excludes below CIHI defined Step-Down Units numbers
#' from calculation:
#' \itemize{
#'  \item{90 : }{Step-Down Medical Unit}
#'  \item{93 : }{Combined Medical/Surgical Step-Down Unit}
#'  \item{95 : }{Step-Down Surgical Unit}
#'  \item{99 : }{No SCU}
#' }
#'
#' Please refer to the CIHI DAD abstracting manual for more details.
#'
#' @param cohort (`data.table`, `data.frame`)\cr
#' Cohort table with all relevant encounters of interest, where each row corresponds to
#' a single encounter. Must contain the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `admission_date_time` (`character`): Date-time of admission in YYYY-MM-DD HH:MM format
#'
#' @param ipscu (`data.table`, `data.frame`)\cr
#' Table equivalent to the `ipscu` table defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Table must contain fields:
#' GEMINI Encounter ID (`genc_id`), SCU admission time (`scu_admit_date_time`), and SCU number (`scu_unit_number`).
#'
#' @param as_outcome (`logical`)\cr
#' Whether ICU admission is as a clinical outcome or not. Default to FALSE.
#' When set to TRUE, records with direct ICU admission before admitted to inpatient care (IP admission)
#' (i.e. `scu_admit_date_time` <= `admission_date_time`) are excluded.
#'
#' @param exclude_cutoff (`integer`)\cr
#' The number of hours to add to the IP admission time to establish a cutoff time for excluding records from being identified as ICU admissions.
#' This parameter is only relevant when `as_outcome=TRUE`.
#' For example, when `exclude_cutoff = 12`, records with ICU entry time `scu_admit_date_time` <= `admission_date_time + 12 hours`
#' are excluded from being identified as ICU admissions.
#' Default is exclude_cutoff = 0, where any ICU entries before IP admission time are excluded.
#'
#' @param entry_since_cutoff (`integer`, `vector`)\cr
#' Time window of ICU entry since IP admission (or since x hours post IP admission when user specifies `exclude_cutoff`), in hours.
#' This parameter, together with `exclude_cutoff` specifies the time interval during which ICU admissions are identified.
#' By default, `exclude_cutoff = 0` and `entry_since_cutoff = c(24, 48, 72)`, function calculates ICU entry within 24, 48 and 72 hours since IP admission.
#' Users can specify different values for the two parameters to customize the time interval of interest.
#' For example, when `exclude_cutoff = 24` and `entry_since_cutoff = 48`, function determines whether a patient
#' was admitted to ICU between 24 to 72 (24+48) hours post IP admission (i.e. time interval (24, 72] hours)).
#'
#' @return (`data.table`)\cr
#' By default, for each encounter in input "cohort" returns the corresponding derived boolean (TRUE/FALSE) fields
#' "icu_entry_derived", "icu_entry_in_24hr_derived", "icu_entry_in_48hr_derived" and "icu_entry_in_72hr_derived".
#' If user specified time window x hour is used, field "icu_entry_in_xhr_derived" is computed in addition to "icu_entry_derived".
#'
#' @note:
#' By design, this function will not return any NA values for `icu_entry`, unless the flag is time sensitive
#' (i.e., if `as_outcome` is `TRUE`, or when deriving ICU admission within a certain time window/after a cut-off).
#' For time sensitive flags, any genc_ids with an ICU entry but *no* valid ICU admission date-time will be returned as NA.
#' If a genc_id has *at least one* valid ICU admission date-time, all valid entries will be included in the calculation.
#' Note that a lot of invalid ICU admission date-times contain date information only. Users may choose to impute
#' missing timestamps prior to running this function.
#'
#' When one tries to left-join the output of this function to another table,
#' make sure the list of encounters aligns in both tables.
#' As there are 2 dependent parameters (exclude_cutoff, entry_since_cutoff)
#' determining the time interval during which ICU admissions will be identified,
#' please read function documentation carefully when specifying a non-default value to these parameters
#' to ensure the identification time interval is as intented. See examples below for common use cases.
#'
#' @export
#'
#' @examples
#' ## ICU admission within the first 24 hours since IP admission (i.e. you are interested in knowing % of encounters admitted to ICU):
#' \dontrun{
#'  icu_entry (cohort, ipscu, as_outcome=FALSE, entry_since_cutoff=24)
#' }
#'
#' ## ICU admission within the first 24 hours since IP admission, as a clinical outcome
#' ## excluding records with ICU entries prior to IP admission:
#' \dontrun{
#'  icu_entry (cohort, ipscu, as_outcome=TRUE, entry_since_cutoff=24)
#' }
#'
#' ## ICU admission within the first 72 hours since IP admission, as a clinical outcome
#' ## excluding records with ICU entries prior to the first 24 hours of IP admission (i.e. you are interested in knowing
#' ## patients who were admitted to ICU between the interval of (24, 72] hours since IP admission):
#' \dontrun{
#'  icu_entry (cohort, ipscu, as_outcome=TRUE, exclude_cutoff=24, entry_since_cutoff=48) # Note: entry_since_cutoff=48 (instead of 72) because 24+48=72
#' }
#'
#'

icu_entry <- function(cohort, ipscu, as_outcome = FALSE, exclude_cutoff = 0, entry_since_cutoff = c(24, 48, 72)) {
  ###### Check user inputs ######
  ## table provided as data.frame/data.table
  if (!any(class(cohort) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for cohort. Please provide a data.frame or a data.table.")
  }

  if (!any(class(ipscu) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for ipscu. Please provide a data.frame or a data.table.")
  }

  ## table contains required fields
  if (any(!c("genc_id", "admission_date_time") %in% names(cohort))) {
    stop("Input cohort table is missing at least one of the required variables.
          Refer to function documentation for details.")
  }

  if (any(!c("genc_id", "scu_admit_date_time", "scu_unit_number") %in% names(ipscu))) {
    stop("Input ipscu table is missing at least one of the required variables.
          Refer to function documentation for details.")
  }

  ###### Prepare data ######

  ## coerce tables to data.table as function uses data.table syntax
  cohort <- coerce_to_datatable(cohort)
  ipscu <- coerce_to_datatable(ipscu)

  ## subset and format fields used for calculation
  res <- cohort[, .(
    genc_id,
    admission_date_time = convert_dt(admission_date_time, addtl_msg = "")
  )]
  ipscu <- ipscu[, .(
    genc_id,
    scu_admit_date_time, # convert to dt below, once we've filtered relevant rows
    scu_unit_number
  )]

  ## filter out step-down units as they are not considered as icus, and merge in admission date time
  ipscu <-
    ipscu %>%
    dplyr::mutate(across(where(is.character), na_if, "")) %>%
    .[!trimws(as.character(scu_unit_number)) %in% c("90", "93", "95", "99")] %>%
    .[, .(genc_id, scu_admit_date_time)] %>%
    dplyr::left_join(res, by = "genc_id") %>% data.table()

  ###### show warning for those with invalid entry time  ######
  ## convert scu_admit_date_time into correct format / show warning for missing values
  ipscu <- ipscu[, scu_admit_date_time := convert_dt(scu_admit_date_time, addtl_msg = "")]

  # identify genc_ids with ICU entry where ALL entries have a missing/invalid date-time
  all_missing <- ipscu[, .(all_na = all(is.na(scu_admit_date_time))), by = genc_id][all_na == TRUE]
  if (nrow(all_missing) > 0) {
    warning(
      paste(
        "Identified a total of", nrow(all_missing),
        "genc_ids in the ICU table that have 0 entries with valid `scu_admit_date_time`.\n",
        "These genc_ids will be returned as `NA` for any ICU flags that are time sensitive",
        "(e.g., ICU entry within a certain time window/after an exclusion cut-off).",
        "For all other genc_ids, any entries with valid ICU admission date-times will be included in determining ICU entry.\n",
        "Please carefully check the `ipscu` table and perform any additional",
        "pre-processing for date-time variables if necessary (e.g., impute missing timestamps).\n"
      ),
      immediate. = TRUE
    )
  }

  ##### Define cutoff time (i.e time point since which icu entry will be considered. Records prior to this cutoff are removed).
  ipscu[, exclude_time_cutoff := admission_date_time + lubridate::hours(exclude_cutoff)] #Default to admission_date_time (ipatient admission time).

  ###### icu as an outcome or not ######
  if (as_outcome == TRUE) {

    cutoff_msg <- ifelse(exclude_cutoff == 0, "inpatient admission time", paste(exclude_cutoff, "hours post-ipadmission"))
    message(paste0(
      "Based on user input, deriving ICU entry as a clinical outcome. \n",
      nrow(ipscu[scu_admit_date_time <= exclude_time_cutoff, ]),
      " records have icu-entry time before or equal to ", cutoff_msg, ". They are removed from deriving ICU entry as an outcome.\n"
    ))

    # only keep those with scu_admit_date_time after the cut-off
    ipscu <- ipscu %>% dplyr::filter(scu_admit_date_time > exclude_time_cutoff)
  }


  ##### Define cutoff time (i.e time point since which icu entry will be considered. Records prior to this cutoff are removed).
  ipscu[, exclude_time_cutoff :=  admission_date_time + lubridate::hours(exclude_cutoff)] #Default to admission_date_time (ipatient admission time).

  ###### icu as an outcome or not ######
  if (as_outcome == TRUE) {

    cutoff_msg <- ifelse(exclude_cutoff == 0, "inpatient admission time", paste(exclude_cutoff, "hours post-ipadmission"))
    message(paste0(
      "Based on user input, deriving ICU entry as a clinical outcome. \n",
      nrow(ipscu[scu_admit_date_time <= exclude_time_cutoff, ]),
      " records have icu-entry time before or equal to ", cutoff_msg, ". They are removed from deriving ICU entry as an outcome.\n"
    ))

    ipscu <- ipscu %>%  dplyr::filter(scu_admit_date_time > exclude_time_cutoff)
  }

  ###### Derive ICU entry fields ######

  ## derive ICU entry at any time point (regardless of whether valid date-time)
  res[, icu_entry_derived := ifelse(genc_id %in% ipscu$genc_id, TRUE, FALSE)]

  ## if as_outcome is set to TRUE, ICU entry is time sensitive
  # -> need to set icu_entry to NA for genc_ids with any missing/invalid ICU date-time
  if (as_outcome == TRUE) {
    res[genc_id %in% all_missing$genc_id, icu_entry_derived := NA]
  }

  ## derive ICU entry within a specified time window since the cutoff time.
  #  for genc_ids with any missing/invalid scu_admit_date_time, return NA
  lapply(entry_since_cutoff, function(x) {
    res[!genc_id %in% all_missing$genc_id,
        paste0("icu_entry_in_", x, "hr_derived") :=
          ifelse(genc_id %in% ipscu[scu_admit_date_time <= (exclude_time_cutoff + lubridate::hours(x)), genc_id],
                 TRUE,
                 FALSE)]
  })

  ###### Clean up output ######

  ## remove irrelevant field from output
  res[, admission_date_time := NULL][]

  return(res)
}
