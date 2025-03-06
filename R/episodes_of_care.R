#' @title
#' Identify episodes of care
#'
#' @description
#' The `episodes_of_care()` function groups encounters that are linked via transfers into a single episode of
#' care. The derived episodes of care are used in the `readmission()` function to avoid counting transfers as
#' readmissions. Therefore, the criteria for transfers used in this function are based on
#' [CIHI guidelines for readmission calculations](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital).
#' However, the `episodes_of_care` function can also be used independently to compute episodes
#' of care outside of readmission.
#'
#' Transfer information is obtained from the GEMINI database table `lookup_transfer` (see
#' [GEMINI database schema](https://geminimedicine.ca/the-gemini-database/)).
#' By default, the function queries all encounters in the `admdad` table to ensure that any encounters that may be
#' linked via transfers are accurately grouped into the same episode of care.
#'
#' Note: Users do not typically need to calculate episodes of care themselves, but rather should use the derived
#' `epicares` variable in the GEMINI database, if available. This is due to the fact that the derived variable
#' is calculated based on all available GEMINI encounters, and therefore, provides the most accurate identification
#' of episodes of care. By contrast, datacuts or cohorts that have been pre-filtered may miss certain encounters,
#' resulting in a loss of information. However, in certain situations, users may still find this function useful 1)
#' to understand how the derived variable is calculated and 2) in case users need to re-calculate episodes of care
#' or readmission rates for a restricted cohort (see below).
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param restricted_cohort (`data.table` | `data.frame`)\cr
#' User specified cohort that is a restricted subset of all encounters in DRM table "ipadmdad" (see
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/)).
#' Must contain `genc_id` as the identifier. Default is `Null`, which loads the entire "ipadmdad"
#' table in the user-provided database (recommended approach).
#'
#' @details
#' An episode of care refers to all contiguous inpatient hospitalizations admitted to any medical or intensive care
#' service within GEMINI. Episodes involving inter-facility transfers are linked regardless of diagnosis. An acute
#' care transfer is assumed to have occurred if either of the following criteria are met (see
#' [CIHI guidelines](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital):
#' \itemize{
#'   \item{An admission occurs within 7 hours after discharge, regardless of whether the transfer is
#' coded by hospitals. OR}
#'   \item{An admission occurs within 7-12 hours after discharge, and at least one hospital has coded the transfer.}
#' }
#'
#' Acute transfers that are coded by hospitals (`AT_in_coded` and `AT_out_coded`) are based on GEMINI-derived
#' mappings of institution types that can be found in the `lookup_transfer` table. The mapped institution types are
#' derived from the raw institution codes in `admdad` (DAD fields `institution_from` and `institution_to`):
#' `AT_in_coded`/`AT_out_coded` is `TRUE`, when the mapped institution from/to type is "AT". All remaining
#' entries are set to `FALSE`.
#'
#' Acute transfers that actually occurred (`AT_in_occurred` and `AT_out_occurred`) are defined as follows:
#' `AT_in_occurred`/`AT_out_occurred` is `TRUE` when admission is within 7 hrs of discharge regardless of transfer
#' coding, or, admission is within 7-12hrs of discharge and at least one hospital coded the transfer.
#' `AT_in_occurred`/`AT_out_occurred` is `NA` when the transfer was coded but admission time since
#' previous discharge is unknown. This is because it cannot be determined if the transfer actually took place or not.
#' `AT_in_occurred`/`AT_out_occurred` is `FALSE`, for all remaining entries.
#'
#' Each episode of care (`epicare`) is defined by linked transfers identified based on `AT_in_occurred`/
#' `AT_out_occurred`. A unique numeric ID is assigned to each unique episode of care. That is, contiguous encounters
#' linked by transfers will have the same `epicare` number.
#'
#' @return (`data.frame`)\cr
#' This function returns a `data.table` excluding records with invalid `patient_id_hashed`, `age < 18`, or
#' `admit_category` of cadaveric donors in line with CIHI guidelines.
#' Nine columns are returned: `genc_id`, `patient_id_hashed`, `time_to_next_admission` (`numeric`),
#' `time_since_last_admission` (`numeric`), `AT_in_coded` (`TRUE`/`FALSE`), `AT_out_coded` (`TRUE`/`FALSE`),
#' `AT_in_occurred` (`TRUE`/`FALSE`/`NA`), `AT_out_occurred` (`TRUE`/`FALSE`/`NA`), `epicare` (`numeric`)
#'
#' @note
#' We recommend that users run this function on the entire GEMINI data set (the default setting) to ensure no
#' transfers to/from are omitted.
#' Please review the function documentation & vignette carefully. A comprehensive understanding of the following
#' concepts is needed for proper use of the function: transfer coded vs. transfer occurred, episode of care definition.
#'
#' @seealso `vignette("epicare_and_readmission", package = "Rgemini")`
#'
#' @references
#' [CIHI readmission guidelines](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital)
#'
#'
#' @export
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
#' epicare_table <- episodes_of_care(dbcon)
#' }
#'
episodes_of_care <- function(dbcon, restricted_cohort = NULL) {
  ## check user inputs
  check_input(dbcon, "DBI")
  if (!is.null(restricted_cohort)) {
    check_input(restricted_cohort, c("data.table", "data.frame"),
      colnames = "genc_id"
    )
  }

  ############ Load lookup_transfer ############
  lookup_transfer_name <- find_db_tablename(dbcon, "lookup_transfer", verbose = FALSE)
  lookup_transfer <- DBI::dbGetQuery(dbcon, paste0("select * from ", lookup_transfer_name, ";")) %>% as.data.table()

  ############ Load whole admdad table (default) ############
  ## find relevant table name corresponding to admdad
  admdad_name <- find_db_tablename(dbcon, "admdad", verbose = FALSE)
  if (!is.null(restricted_cohort)) {
    restricted_cohort <- coerce_to_datatable(restricted_cohort)
    if (!"genc_id" %in% names(restricted_cohort)) {
      stop("genc_id is required in user specified cohort")
    } else {
      warning(
        "Note: Based on the user input, epicares will be computed solely based on the user specified cohort,
        instead of the default method. The default method computes epicares based on all available data in the
        GEMINI database and is recommended.\n ",
        immediate. = TRUE
      )

      ## write a temp table to improve querying efficiency
      DBI::dbSendQuery(dbcon, "Drop table if exists temp_data;")
      DBI::dbWriteTable(
        dbcon, c("pg_temp", "temp_data"), restricted_cohort[, .(genc_id)],
        row.names = FALSE, overwrite = TRUE
      )

      # Analyze speed up the use of temp table
      DBI::dbSendQuery(dbcon, "Analyze temp_data")

      admdad <- DBI::dbGetQuery(dbcon, paste0(
        "select genc_id, patient_id_hashed, age, admit_category, admission_date_time,
                                            discharge_date_time
                                            from ", admdad_name,
        " a where exists (select 1 from temp_data t where t.genc_id=a.genc_id); "
      )) %>% as.data.table()
    }
  } else {
    admdad <- DBI::dbGetQuery(dbcon, paste0(
      "select genc_id, patient_id_hashed, age, admit_category, admission_date_time,
                                            discharge_date_time
      from ", admdad_name
    )) %>% as.data.table()
  }

  ############ Prepare data for epicare computation ############
  data <- merge(admdad, lookup_transfer, by = "genc_id", all.x = TRUE)

  ############  Exclude invalid records   ############
  data <- data[!is.na(patient_id_hashed) & patient_id_hashed != ""] # remove invalid patient_id

  data <- data[!admit_category %in% c("R", "RI"), ] # cadaveric donors should not be in DB
  data <- data[age >= 18, ] # age < 18 should not be in DB

  ############  Convert date-times into appropriate format   ############
  data[, discharge_date_time := convert_dt(discharge_date_time)]
  data[, admission_date_time := convert_dt(admission_date_time)]

  ############  Identify hospital coded Acute Transfer (AT)   ############
  data <- data[order(patient_id_hashed, discharge_date_time, admission_date_time)]

  ### Set default to false
  data$AT_in_coded <- FALSE
  data$AT_out_coded <- FALSE

  ## identify acute-care transfers ("AT") based on mapped AT transfers
  #  according to lookup_transfer (GEMINI-mapped insitution types)
  #  relevant column names depend on DB version of lookup_transfer table
  if ("acute_transfer_in" %in% colnames(lookup_transfer)) {
    # For DB versions <= report/drm DB v2 [H4H_template v3]:
    # filter lookup_transfer for acute_transfer_in/out = "AT"
    data[acute_transfer_in == "AT", AT_in_coded := TRUE]
    data[acute_transfer_out == "AT", AT_out_coded := TRUE]
  } else {
    # For DB versions > report/drm DB v2 [H4H_template v3]:
    # filter lookup_transfer for institution_from/to_type_mns = "AT"
    data[institution_from_type_mns == "AT", AT_in_coded := TRUE]
    data[institution_to_type_mns == "AT", AT_out_coded := TRUE]
  }

  ############  Compute `time_to_next_admission`, `time_since_last_admission`  ############
  ## Defined as time difference between admission date-time of (n+1)th encounter minus
  ## discharge date-time of (n)th encounter
  data[, time_to_next_admission := as.numeric(difftime(
    data.table::shift(admission_date_time, type = "lead"), # (n+1)th encounter
    discharge_date_time, # (n)th encounter
    units = "hours"
  ))]

  ## For last encounter of each patient: time_to_next_admission = Inf since no next encounter by definition
  data[!duplicated(patient_id_hashed, fromLast = TRUE), time_to_next_admission := Inf]


  ## Defined as time difference between admission date-time of nth encounter - discharge date-time of (n-1)th encounter
  data[, time_since_last_admission := as.numeric(difftime(admission_date_time, # nth encounter
    data.table::shift(discharge_date_time, type = "lag"), # (n-1)th discharge
    units = "hours"
  ))]

  ## For first encounter of each patient: time_since_last_admission = Inf since no previous encounter by definition
  data[!duplicated(patient_id_hashed), time_since_last_admission := Inf]

  ############  Identify `AT_out_occurred` and `AT_out_occurred`  ############
  ## An acute transfer out is assumed to have actually occurred (`AT_out_occurred`=T) if EITHER criteria is met:
  ##  1) time_to_next_admission <7, regardless of AT_out coding; OR
  ##  2) time_to_next_admission >= 7 AND time_to_next_admission <=12 AND with hospital coding of AT_out=T.
  ## Note: when time_to_next_admission=Inf AND hospital coding AT_out=T, it is unknown whether or not the transfer has
  ## actually occurred, therefore `AT_out_occurred=NA`.For anything else remaining `AT_out_occurred=False`,
  ## Same logic for acute transfer in (`AT_in_occurred`)

  ### Set default to false
  data$AT_in_occurred <- FALSE
  data$AT_out_occurred <- FALSE

  ### <7h, regardless of hospital coding
  data[time_since_last_admission < 7, AT_in_occurred := TRUE]
  data[time_to_next_admission < 7, AT_out_occurred := TRUE]

  ### 7-12h, with hospital coding
  data[time_since_last_admission >= 7 & time_since_last_admission <= 12 & AT_in_coded == TRUE, AT_in_occurred := TRUE]
  data[time_to_next_admission >= 7 & time_to_next_admission <= 12 & AT_out_coded == TRUE, AT_out_occurred := TRUE]

  ### Time of transfer is unknown (Inf)
  data[time_since_last_admission == Inf & AT_in_coded == TRUE, AT_in_occurred := NA]
  data[time_to_next_admission == Inf & AT_out_coded == TRUE, AT_out_occurred := NA]

  ############  Derive epicare = episode of care based on occurred transfers   ############

  ### 1. Compute lagged AT_out_occurred within patient_id_hashed
  data[, AT_out_occurred_lag := shift(AT_out_occurred), by = patient_id_hashed]

  ### 2. Identify linked transfers
  ## same_as_prev=T when nth encounter is an occurred transfer from the (n-1)th encounter
  data$same_as_prev <- FALSE # set default to F
  data[AT_in_occurred == TRUE | AT_out_occurred_lag == TRUE, same_as_prev := TRUE]

  ## Reset the first encounter of each patient_id with 'same_as_prev' to False
  ## This is to account for special scenarios where the first encounter has AT_in_occurred==T (thus same_as_prev==T),
  ## but we do want to count this encounter as the beginning of a unique epicare
  data[, same_as_prev := ifelse(rowid(patient_id_hashed) == 1, FALSE, same_as_prev)]

  ## Set up 'new_epi' to signal occurrence of a unique epicare
  data$new_epi <- 0 # set to 0 by default, i.e., encounter is *not* a new epicare
  data[same_as_prev == FALSE, new_epi := 1]

  ### 3. Assign unique key to each epicare
  ## First assign unique raw epicare ids to encounters based on patient_id
  setDT(data)[, epicare_original := .GRP, by = patient_id_hashed]
  # Then update epicare based on new_epi to distinguish new episode of care of a patient_id
  data[, epicare := epicare_original + cumsum(new_epi)]
  data[, epicare := .GRP, by = epicare] # reset count to be continuous & start from 1

  ############  Output data  ############
  data <- data[, c(
    "genc_id", "patient_id_hashed", "time_to_next_admission", "time_since_last_admission",
    "AT_in_coded", "AT_out_coded", "AT_in_occurred", "AT_out_occurred", "epicare"
  )]

  return(data)
}
