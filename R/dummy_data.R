#' @title
#' Simulate ICD-10 Diagnosis Codes
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated in Rgemini version 3.0.0.
#' @export
sample_icd <- function(n = 1, source = "comorbidity", dbcon = NULL, pattern = NULL) {
  lifecycle::deprecate_stop(
    "3.0.0", "sample_icd()", "gemSim::sample_icd()"
  )
}

#' @title
#' Generate Simulated Diagnosis Data Table
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated in Rgemini version 3.0.0.
#' @export
dummy_diag <- function(nid = 5, nrow = 50, ipdiagnosis = TRUE, diagnosis_type = NULL, ...) {
 lifecycle::deprecate_stop(
    "3.0.0", "dummy_diag()", "gemSim::dummy_diag()"
  )
}


#' @title
#' Simulate ipadmdad data
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated in Rgemini version 3.0.0.
#' @export
dummy_ipadmdad <- function(n = 1000,
                           n_hospitals = 10,
                           time_period = c(2015, 2023)) {
   lifecycle::deprecate_stop(
    "3.0.0", "dummy_ipadmdad()", "gemSim::dummy_admdad()"
  )
}


#' @title
#' Generated simulated lab data
#'
#' @description
#' Designed to mimic the most important elements of the GEMINI lab table as defined in the
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#'
#' @param id (`numeric`)\cr
#' A single identifier that is repeated to match the length of `value`.
#'
#' @param omop (`character`)\cr
#' Codes corresponding to OMOP concept identifiers.
#'
#' @param value (`numeric`)\cr
#' Simulated result values for each lab test measurement.
#'
#' @param unit (`character`)\cr
#' Units corresponding to the particular lab test as defined by `omop`. It is repeated to match the length of `value`.
#'
#' @param mintime (`character`)\cr
#' In the format yyyy-mm-dd hh:mm. Earliest recorded test performed time.
#'
#' @return (`data.table`)\cr
#' With the columns, `id`, `omop`, `value`, `unit`, and `collection_date_time` as described above.
#'
#' @export
#'
#' @examples
#' lab <- dummy_lab(1, 3024641, c(7, 8, 15, 30), "mmol/L", "2023-01-02 08:00")
#'
dummy_lab <- function(id, omop, value, unit, mintime) {
  res <- data.table(
    genc_id = rep(id, length(value)),
    test_type_mapped_omop = omop,
    result_value = value,
    result_unit = rep(unit, length(value)),
    collection_date_time = format(as.POSIXct(mintime, tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = length(value), replace = TRUE), "%Y-%m-%d %H:%M")
  )
  return(res)
}


#' @title
#' Generated simulated administrative data
#'
#' @description
#' Designed to partially mimic the `admdad` table as defined in the
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#'
#' @param id (`numeric`)\cr
#' A single identifier that is repeated to match the length of `value`.
#'
#' @param admtime (`character`)\cr
#' In the format yyyy-mm-dd hh:mm. Corresponds to the admission time of the encounter.
#'
#' @return (`data.table`)\cr
#' With the columns `id` and `admission_date_time` as described above.
#'
#' @export
#'
#' @examples
#' admdad <- dummy_admdad(1, "2023-01-02 00:00")
#'
dummy_admdad <- function(id, admtime) {
  res <- data.table(
    genc_id = id,
    admission_date_time = format(as.POSIXct(admtime, tz = "UTC"), "%Y-%m-%d %H:%M")
  )
  return(res)
}
