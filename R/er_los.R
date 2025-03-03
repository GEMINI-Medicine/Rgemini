#' @title
#' Compute Length of Stay in Emergency Room
#'
#' @description
#' Calculate the total duration in Emergency Room (ER) that an encounter spent
#' through a hospitalization using CIHI National Ambulatory Care Reporting System
#' (NACRS) fields
#'
#' @details
#' This function calculates the length of stay (LoS) in hours and days that an
#' encounter spent in the ER during a hospital stay.
#'
#' It uses NACRS fields Triage Date and Time (Data Element Number 24/25) and
#' Date and Time Patient Left Emergency Department (Data Element Number 116/117).
#'
#' @param cohort (`data.table` or `data.frame`)\cr
#' Table with all relevant encounters of interest, where each row corresponds to
#' a single encounter. Must contain GEMINI Encounter ID (`genc_id`).
#'
#' @param er (`data.table` or `data.frame`)\cr
#' Table equivalent to the `er` table defined in the
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#' Table must contain fields:
#' GEMINI Encounter ID (`genc_id`),
#' ER triage date-time (`triage_date_time` in "yyyy-mm-dd hh:mm" format),
#' Left ER date-time (`left_er_date_time` in "yyyy-mm-dd hh:mm" format).
#'
#' @return (`data.table`)\cr
#' By default, for each encounter in input `cohort` returns the corresponding
#' derived numeric fields `er_los_hrs_derived` and `er_los_days_derived`.
#'
#' @note
#' Encounter IDs in the `cohort` table that are not present in the `er` table
#' are assumed to have no ER visit and are returned with `er_los = 0`. Please
#' check if any of these entries might be due to data availability issues and
#' consider removing them from your analyses.
#' Encounter IDs in the `er` table that have missing/invalid `triage_date_time`
#' or `left_er_date_time` will be returned with `er_los = NA`.
#'
#' @export
#'
#' @examples
#' # Compute ER LoS for all encounters in ipadmdad;
#' \dontrun{
#' er_los(cohort = ipadmdad, er = er)
#' }
#'
er_los <- function(cohort, er) {
  ###### Check user inputs ######
  check_input(
    arginput = cohort,
    argtype = c("data.table", "data.frame"),
    colnames = "genc_id"
  )

  check_input(
    arginput = er,
    argtype = c("data.table", "data.frame"),
    colnames = c("genc_id", "triage_date_time", "left_er_date_time")
  )

  ###### Prepare data ######
  ## coerce tables to data.table as function uses data.table syntax
  cohort <- coerce_to_datatable(cohort)
  er <- coerce_to_datatable(er)

  ##### Prepare er data #####
  er[, `:=`(
    triage_date_time = convert_dt(
      triage_date_time,
      addtl_msg = ""
    ),
    left_er_date_time = convert_dt(
      left_er_date_time,
      addtl_msg = ""
    )
  )]

  # convert_dt will already show warnings about missing/invalid date-times but
  # adding a general warning here for how those are dealt with within er_los
  n_invalid_dt <- sum(is.na(er$triage_date_time) | is.na(er$left_er_date_time))
  if (n_invalid_dt > 0) {
    warning(
      paste(
        "Identified a total of", n_invalid_dt,
        "entries with missing/invalid `triage_date_time` or `left_er_date_time`.",
        "These entries will be returned as `er_los = NA`.",
        "Please carefully check the `er` table and perform any additional",
        "pre-processing for date-time variables if necessary.\n"
      ),
      immediate. = TRUE
    )
  }

  ### Derive er length of stay ###
  er[, `:=`(
    er_los_hrs_derived = as.numeric(difftime(left_er_date_time,
      triage_date_time,
      units = "hours"
    )),
    er_los_days_derived = as.numeric(difftime(left_er_date_time,
      triage_date_time,
      units = "days"
    ))
  )]

  ### Merge result with cohort ###
  res <- merge(cohort[, .(genc_id)],
    er[, .(genc_id, er_los_hrs_derived, er_los_days_derived)],
    by = "genc_id",
    all.x = TRUE, all.y = FALSE
  )

  ## Prepare final output
  # for genc_ids that don't exist in er table, impute ER LOS with 0
  # (assume no time was spent in ER)
  # keep all other NA (for genc_ids that exist in er table but have
  # invalid/missing er date-times)
  res[!genc_id %in% er$genc_id, `:=`(
    er_los_hrs_derived = 0,
    er_los_days_derived = 0
  )]

  return(res)
}
