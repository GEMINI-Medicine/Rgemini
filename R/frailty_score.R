#' @title
#' Compute frailty score using ICD-10-CA
#'
#' @description
#' `frailty_score` returns frailty risk score for hospital admissions calculated based on ICD-10-CA diagnoses codes.
#'
#' @details
#' This function takes a list of ICD-10-CA and generates a numeric field corresponding to frailty risk
#' score for each admission.
#' This function can take any `data.table` with ICD-10-CA codes.
#' Historically, scores have been calculated based on partition of ICD-10-CA codes.
#'
#' Researchers may want to focus on frailty risk scores based on in-patient (IP) diagnoses because
#' emergency department (ED) diagnoses have lower sensitivity/specificity for certain conditions and
#' are less reliably filled compared to IP diagnoses. In general, there will be overlap between IP and
#' ED diagnoses codes. ED diagnoses codes to compute frailty risk score can be used to evaluate frailty
#' risk upon admission to IP services.
#'
#' This function does not differentiate between diagnosis types.
#'
#' @note
#' The original frailty risk score was developed based on ICD-10 codes instead of ICD-10-CA.
#' However, ICD-10-CA version codes are identical to the ICD-10 counterpart up to 4 character levels
#' and the frailty risk score only uses the first three characters of ICD-10.
#'
#' Please refer to the references in this page for more details.
#'
#' @section Warning:
#' This function returns a `data.table` with `genc_id` and three numeric fields.
#' The function will return NA values if `genc_id` in the `cohort` table is not found
#' in the `ipdiagnosis` or `eddiagnosis` tables.
#'
#' `frailty_score_derived` is NA if either `ip_frailty_score_derived` or `er_frailty_score_derived` are NA.
#' When one tries to left-join the output of this function with another table (another list of admissions in the left),
#' make sure list of admissions (or patient ids) aligns in both tables.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#' @param ipdiagnosis (`data.table`)
#' `ipdiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' This table must contain the `genc_id` and `diagnosis_code` (as ICD-10-CA) fields in long format table only.
#' The diagnosis codes must be free from any punctuation or special characters.
#' @param eddiagnosis (`data.table`)
#' `eddiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' This table must contain the `genc_id` and `er_diagnosis_code` (as ICD-10-CA) fields in long format table only.
#' The diagnosis codes must be free from any punctuation or special characters.
#'
#' @return `data.table`
#' This function returns a table with all encounters identified by the `cohort` table input and
#' additional derived numeric fields for `ip_frailty_score_derived`, `er_frailty_score_derived`
#' and `frailty_score_derived`
#' \itemize{
#'  \item{ip_frailty_score_derived : }{Frailty risk score calculated only using IP diagnoses codes from `ipdiagnosis` table}
#'  \item{er_frailty_score_derived : }{Frailty risk score calculated only using ED diagnoses codes from `eddiagnosis` table}
#'  \item{frailty_score_derived : }{Frailty risk score calculated using both IP & ED diagnoses codes}
#' }
#' If frailty risk score is < 5, the patient is low-risk
#' If frailty risk score is between 5 and 15, the patient is intermediate risk
#' If frailty risk score is > 15.0, the patient is high risk
#'
#' @references
#' Gilbert T, Neuburger J, Kraindler J, et al. Development and validation of a
#' Hospital Frailty Risk Score focusing on older people in acute care settings using
#' electronic hospital records: an observational study. Lancet 2018; published online
#' April 26. http://dx.doi.org/10.1016/S0140-6736(18)30668-8.
#'
#' @export
frailty_score <- function(cohort,
                          ipdiagnosis,
                          eddiagnosis) {

  ## Ensure user inputs are in data.table format before proceeding
  cohort <- coerce_to_datatable(cohort)
  ipdiagnosis <- coerce_to_datatable(ipdiagnosis)
  eddiagnosis <- coerce_to_datatable(eddiagnosis)

  ## remap variable names in case field names change in the database
  res <- cohort[, .(idvar1 = genc_id)]

  ipdiag <- ipdiagnosis[, .(id = genc_id,
                            diagcode = diagnosis_code)]

  eddiag <- eddiagnosis[, .(id = genc_id,
                            diagcode = er_diagnosis_code)]

  ## Frailty score mapping for first 3 character of ICD-10. This table can be found in the reference provided.
  frailty_score <-
    structure(list(diagcode = c("F00", "G81", "G30", "I69", "R29", "N39",
                           "F05", "W19", "S00", "R31", "B96", "R41", "R26", "I67", "R56",
                           "R40", "T83", "S06", "S42", "E87", "M25", "E86", "R54", "Z50",
                           "F03", "W18", "Z75", "F01", "S80", "L03", "H54", "E53", "Z60",
                           "G20", "R55", "S22", "K59", "N17", "L89", "Z22", "B95", "L97",
                           "R44", "K26", "I95", "N19", "A41", "Z87", "J96", "X59", "M19",
                           "G40", "M81", "S72", "S32", "E16", "R94", "N18", "R33", "R69",
                           "N28", "R32", "G31", "Y95", "S09", "R45", "G45", "Z74", "M79",
                           "W06", "S01", "A04", "A09", "J18", "J69", "R47", "E55", "Z93",
                           "R02", "R63", "H91", "W10", "W01", "E05", "M41", "R13", "Z99",
                           "U80", "M80", "K92", "I63", "N20", "F10", "Y84", "R00", "J22",
                           "Z73", "R79", "Z91", "S51", "F32", "M48", "E83", "M15", "D64",
                           "L08", "R11", "K52", "R50"),

                   score = c("7.1", "4.4", "4.0", "3.7",
                             "3.6", "3.2", "3.2", "3.2", "3.2", "3.0", "2.9", "2.7", "2.6",
                             "2.6", "2.6", "2.5", "2.4", "2.4", "2.3", "2.3", "2.3", "2.3",
                             "2.2", "2.1", "2.1", "2.1", "2.0", "2.0", "2.0", "2.0", "1.9",
                             "1.9", "1.8", "1.8", "1.8", "1.8", "1.8", "1.8", "1.7", "1.7",
                             "1.7", "1.6", "1.6", "1.6", "1.6", "1.6", "1.6", "1.5", "1.5",
                             "1.5", "1.5", "1.5", "1.4", "1.4", "1.4", "1.4", "1.4", "1.4",
                             "1.3", "1.3", "1.3", "1.2", "1.2", "1.2", "1.2", "1.2", "1.2",
                             "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.1", "1.0", "1.0",
                             "1.0", "1.0", "1.0", "0.9", "0.9", "0.9", "0.9", "0.9", "0.9",
                             "0.8", "0.8", "0.8", "0.8", "0.8", "0.8", "0.7", "0.7", "0.7",
                             "0.7", "0.7", "0.6", "0.6", "0.5", "0.5", "0.5", "0.5", "0.4",
                             "0.4", "0.4", "0.4", "0.3", "0.3", "0.1")))

  frailty_score <- data.frame(frailty_score)

  ### Assign scores by merging first 3 characters
  ipdiag[, diagcode := stringr::str_sub(diagcode, 1, 3)]
  eddiag[, diagcode := stringr::str_sub(diagcode, 1, 3)]
  alldiag <- rbind(ipdiag, eddiag)

  ipdiag <- merge(ipdiag, frailty_score, by = "diagcode", all.x = TRUE) %>%
    .[, .(diagcode, id, score)] %>%
    unique %>%
    .[, .(stat = sum(as.numeric(score), na.rm = TRUE)), id]

  eddiag <- merge(eddiag, frailty_score, by = "diagcode", all.x = TRUE) %>%
    .[, .(diagcode, id, score)] %>%
    unique %>%
    .[, .(stat = sum(as.numeric(score), na.rm = TRUE)), id]

  alldiag <- merge(alldiag, frailty_score, by = "diagcode", all.x = TRUE) %>%
    .[, .(diagcode, id, score)] %>%
    unique %>%
    .[, .(stat = sum(as.numeric(score), na.rm = TRUE)), id]


  ### merge with admission list and NA rules
  res <- merge(res, ipdiag[, .(id, ip_frailty_score_derived = stat)], by.x = "idvar1", by.y = "id", all.x = TRUE)
  res <- merge(res, eddiag[, .(id, er_frailty_score_derived = stat)], by.x = "idvar1", by.y = "id", all.x = TRUE)
  res <- merge(res, alldiag[, .(id, frailty_score_derived = stat)], by.x = "idvar1", by.y = "id", all.x = TRUE)

  res[(is.na(ip_frailty_score_derived) | is.na(er_frailty_score_derived)), frailty_score_derived := NA][]

  data.table::setnames(res,
                       old = c("idvar1"),
                       new = c("genc_id"))
  return(res)
}
