#' @title
#' Compute CIHI Hospital Frailty Risk Score
#'
#' @description
#' `frailty_score` returns the number of frailty conditions for hospital admissions based on the CIHI hospital frailty risk score (HFRS).
#' By setting `component_wise` to `TRUE`, function alternatively returns identified frailty-related diagnosis codes and their corresponding frailty conditions.
#'
#' @details
#' The CIHI HFRS is a contextual measure of frailty for patients aged 65 years and older.
#' It categorizes a list of ICD-10-CA diagnosis codes into 36 distinct frailty-related conditions.
#' The level of frailty is determined as the cumulative number of distinct frailty conditions (an equal-weight algorithm) present in an individual.
#' ALL diagnoses are included to the calculation (i.e. all emergency department (ER) and in-patient (IP) diagnoses, all diagnosis types).
#' When none of the frailty conditions is present in an individual, a score of zero is assigned.
#'
#' The function closely adheres to the CIHI HFRS with the following adaptations:
#'
#' - No look-back period: Score is computed at encounter level. The 2-year look-back in the CIHI HFRS is not implemented.
#'  This adaptation systematically underestimates frailty but ensures comparable scores across time and hospitals considering variations in data availability
#' - Score format: Integer scores are returned representing the sum of the number of frailty conditions.
#'  These scores can be easily converted to the different formats (i.e. continuous fractions, 8 risk groups, binary) defined by CIHI HFRS.
#'  For example, dividing the returned score by 36 (maximum number of conditions possible) gives the continuous CIHI HFRS.
#'  Users interested in further categorizing the scores should refer to [Amuah et al, 2023](https://doi.org/10.1503/cmaj.220926).
#'
#' @param cohort (`data.table`, `data.frame`)\cr
#' Cohort table with encounters of interest and their corresponding age.
#' Each row corresponds to a single encounter.
#' Must contain GEMINI encounter ID (`genc_id`), and age of the encounter (`age`).
#'
#' @param ipdiag (`data.table`, `data.frame`)\cr
#' `ipdiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#' This table must contain the `genc_id` and `diagnosis_code` fields in long format.
#' The diagnosis codes must be free from any punctuation or special characters.
#'
#' @param erdiag (`data.table`, `data.frame`)\cr
#' `erdiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#' This table must contain the `genc_id` and `er_diagnosis_code` fields in long format.
#' The diagnosis codes must be free from punctuation or special characters.
#'
#' @param component_wise (`logical`)\cr
#' Default is FALSE. When TRUE, the function does not aggregate the score and instead outputs a table in long format that
#' shows the frailty conditions contributed to the score for each encounter.
#' Encounters with no frailty conditions found (frailty_score_derived=0) are excluded.
#'
#' @return (`data.frame`)\cr
#' If `component_wise == FALSE`:
#'     `genc_id` (`numeric`),\cr
#'     `frailty_score_derived` (`numeric`) total number of frailty conditions for the encounter.
#'
#' If `component_wise == TRUE`:
#'     `genc_id` (`numeric`),\cr
#'     `diagnosis_code` (`character`),\cr ICD-10-CA diagnosis codes associated with the encounter
#'     `frailty_categories` (`character`)\cr frailty category of the ICD-10-CA code mapped to
#'
#' @section Warnings:
#' A warning message is returned when the user-provided cohort includes encounters not qualified for assessing frailty.
#' Encounters below the age cutoff for frailty assessment are excluded from the results.
#' Encounters with no diagnosis data are excluded from the results.
#' The size of exclusion is detailed in the warning message.
#' `frailty_score_derived` for these encounters are NAs, when joining the result table to the original cohort.
#'
#' A warning message is returned if `erdiag` is explicitly set to `NULL`.
#' It's possible to exclude diagnoses at the emergency department from the consideration by explicitly specifying `erdiag=NULL`.
#' However, excluding any types of diagnoses is not recommended.
#' The CIHI frailty score was developed and validated based on all diagnoses present in medical records (both NACRS and DAD).
#' Excluding diagnoses in NACRS was found to underestimate frailty levels (Amuah et al, 2023).
#'
#' @section Notes:
#' The original development paper of CIHI HFRS maped 595 ICD-10-CA diagnosis codes to frailty conditions.
#' Three codes (Z96.62, U07.1 and U07.2) were added to this mapping in the CIHI methodology notes.
#' This function uses the mapping of 598 ICD-10-CA diagnosis codes in the CIHI methodology notes to identify frailty conditions.
#'
#' The previous `frailty_score()` function calculated the UK HFRS (Gilbert, 2018), and it is now deprecated.
#' Using a similar approach as the UK HFRS, the CIHI HFRS was developed and validated based on Canadian cohorts, making it particularly suited for GEMINI data.
#' The UK version remains available in `Rgemini` version 0.3.1 and earlier but will not receive future maintenance.
#' Users interested in the UK version should refer to the original publications for important differences in diagnostic coding practices and age threshold.
#'
#' @references
#' We recommend referencing both original articles (UK HFRS and CIHI HFRS) when using this function:
#'
#' - UK HFRS: Gilbert T, et al. Lancet, 2018. http://dx.doi.org/10.1016/S0140-6736(18)30668-8
#' - CIHI HFRS: Amuah JE, et al. CMAJ, 2023. https://doi.org/10.1503/cmaj.220926
#' - [CIHI methodology notes](https://www.cihi.ca/sites/default/files/document/cihi-hospital-frailty-risk-measure-meth-notes-en.pdf)
#
#' @examples
#' \dontrun{
#' cohort_dum <- data.table(
#'   genc_id = c(1, 2, 3), age = c(64, 65, 80)
#' )
#' ipdiag_dum <- dummy_diag(
#'   nid = 3, nrow = 10,
#'   ipdiagnosis = TRUE,
#'   pattern = "C20$|R460$" # frailty conditions
#' )
#' erdiag_dum <- dummy_diag(
#'   nid = 3, nrow = 5,
#'   ipdiagnosis = FALSE,
#'   pattern = "M121$" # not a frailty condition
#' )
#' # calculate frailty score
#' frailty_score(cohort_dum, ipdiag_dum, erdiag_dum, component_wise = FALSE)
#' }
#'
#' @export
frailty_score <- function(cohort, ipdiag, erdiag, component_wise = FALSE) {
  # load CIHI HFRS-ICD mapping from package data folder
  frailty_map <- Rgemini::mapping_cihi_frailty %>%
    mutate(diagnosis_code = gsub("\\.", "", icd10ca)) %>%
    data.table()

  # input checks
  check_input(cohort, c("data.table", "data.frame"),
    colnames = c("genc_id", "age"),
    coltypes = c("", "numeric|integer")
  )

  check_input(ipdiag, c("data.table", "data.frame"),
    colnames = c("genc_id", "diagnosis_code"),
    coltypes = c("", "character")
  )

  if (!is.null(erdiag)) {
    check_input(erdiag, c("data.table", "data.frame"),
      colnames = c("genc_id", "er_diagnosis_code"),
      coltypes = c("", "character")
    )
  }

  cohort <- coerce_to_datatable(cohort)
  ipdiag <- coerce_to_datatable(ipdiag)
  if (!is.null(erdiag)) {
    erdiag <- coerce_to_datatable(erdiag)
  }
  elig_enc <- unique(cohort[age >= 65, ]$genc_id)


  # clean and merge all diagnosis codes; return a warning if EXPLICITLY set to NULL by user
  if (is.null(erdiag)) {
    warning(
      "\nBased on user input, `erdiag` is set to NULL. This is NOT recommended. The CIHI frailty score was developed and validated based on diagnosis codes from both NACRS and DAD. Excluding erdiagnosis can underestimate the level of frailty (see references in documentation for details).\n"
    )
  } else {
    erdiag <- erdiag[genc_id %in% elig_enc, .(genc_id, er_diagnosis_code)] %>% rename(diagnosis_code = er_diagnosis_code)
  }

  ipdiag <- ipdiag[genc_id %in% elig_enc, .(genc_id, diagnosis_code)]

  alldiag <- rbind(ipdiag, erdiag)

  # Calculate score
  # Previously used fuzzyjoin, but now removed that dependency due to downtime on CRAN, and to improve speed of function
  # Case-insensitive prefix match (same as ignore_case = TRUE)
  alldiag[, diagnosis_code := toupper(diagnosis_code)]
  frailty_map[, `:=`(
    prefix = toupper(diagnosis_code),
    n = nchar(diagnosis_code)
  )]

  frailty <- rbindlist(lapply(unique(frailty_map$n), \(k) {
    mapk <- frailty_map[n == k, .(prefix, diagnosis_code.y = diagnosis_code, frailty_categories)]
    setkey(mapk, prefix)

    alldiag[, .(
      genc_id,
      diagnosis_code.x = diagnosis_code,
      prefix = substr(diagnosis_code, 1, k)
    )][
      mapk,
      on = "prefix", nomatch = 0L
    ][, .(genc_id, diagnosis_code.x, diagnosis_code.y, frailty_categories)]
  }), use.names = TRUE)

  ## Derive score (# of unique frailty categories) to encounters w/ diagnoses mapped
  res_score <- frailty[, .(frailty_score_derived = length(unique(frailty_categories))), genc_id]

  ## Assign score=0 to encounters w/o diagnosis mapped to frailty conditions
  res_0 <- alldiag[!(genc_id %in% res_score$genc_id), .(genc_id)] %>%
    .[, frailty_score_derived := 0] %>%
    distinct()

  res <- rbind(res_score, res_0) # combine

  ## component_wise result (available only for encounters w/ diagnoses mapped)
  res_component <- frailty[, .(genc_id, diagnosis_code.x, frailty_categories)] %>% rename(diagnosis_code = diagnosis_code.x)

  # Warnings on output
  if (length(elig_enc) < length(unique(cohort$genc_id))) { # exclusion of encounters w/ age<65
    warning(paste0(
      "\n",
      length(unique(cohort[age < 65, ]$genc_id)), " of ", length(unique(cohort$genc_id)),
      " (", round(100 * length(unique(cohort[age < 65, ]$genc_id)) / length(unique(cohort$genc_id)), 1), "%) ",
      "encounters in the user-provided cohort are under the age of 65 and do not qualify for CIHI frailty score. These encounters have been excluded from the results.\n"
    ))
  }

  if (length(elig_enc) > length(res$genc_id)) { # exclusion of encounters w/o diagnosis data
    warning(paste0(
      "\n",
      (length(elig_enc) - length(res$genc_id)), " of ", length(elig_enc),
      " (", round(100 * (length(elig_enc) - length(res$genc_id)) / length(elig_enc), 1), "%) ",
      "age-qualified encounters in the user-provided cohort do not have diagnosis data and have been excluded from the results.\n"
    ))
  }

  # Outputs
  if (component_wise) {
    return(res_component)
  }

  return(res)
}
