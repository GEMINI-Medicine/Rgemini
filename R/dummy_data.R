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
#' `r lifecycle::badge("deprecated")`
#' Deprecated in Rgemini version 3.0.0.
#'
dummy_lab <- function(id, omop, value, unit, mintime) {
   lifecycle::deprecate_stop(
    "3.0.0", "dummy_lab()"
  )
}


#' @title
#' Generated simulated administrative data
#' @description
#' `r lifecycle::badge("deprecated")`
#' Deprecated in Rgemini version 3.0.0.
dummy_admdad <- function(id, admtime) {
  lifecycle::deprecate_stop(
    "3.0.0", "dummy_admdad()"
  )
}
