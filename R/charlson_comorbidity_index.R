#' @title
#' Determine Diagnoses at Admission
#'
#' @description
#' See CIHI's [HSMR Methodology](https://www.cihi.ca/sites/default/files/document/hospital-standardized-mortality-ratio-meth-notes-en.pdf).
#'
#' Specifically page 19 for inclusion/exclusion criteria.
#'
#' @param ipdiag (`data.table`)\cr
#' `ipdiagnosis` table as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' @param erdiag (`data.table`)\cr
#' `erdiagnosis` table as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Typically, ER diagnoses should be included when deriving Charlson Comorbidity
#' in order to increase sensitivity. However, in certain scenarios, users may
#' choose to only include IP diagnoses by specifying `erdiag = NULL`. This may
#' be useful when comparing cohorts with different rates of ER admissions.
#'
#' @return (`data.table`)\cr
#' All encounters by `genc_id` with `diagnosis_type` and `diagnosis_code` for all diagnoses at admission.
#'
#' @export
#'
diagnoses_at_admission <- function(ipdiag, erdiag) {

  ipdiag <- coerce_to_datatable(ipdiag)

  if (!is.null(erdiag)){
    erdiag <- coerce_to_datatable(erdiag)

    ### Synchronize varnames between ipdiag and erdiag
    erdiag[, ":="(diagnosis_code = er_diagnosis_code)]
    erdiag[, ":="(diagnosis_type = er_diagnosis_type)]
  }

  ### Process inpatient diagnoses at admission

  # From CIHI methodology notes (page 19):
  #   "Diagnosis types 1, W, X and Y are used to calculate the Charlson score."

  ip_type_1wxy <- ipdiag[
    diagnosis_type %in% c("1", "W", "X", "Y"),
    c("genc_id", "diagnosis_code", "diagnosis_type")
  ] %>%
    .[, genc_diag := paste(genc_id, diagnosis_code)] # create unique identifier for encounter + diagnosis combo

  # From CIHI methodology notes (page 19):
  #   "Type 3 codes for the following conditions are also included where applicable:"

  diabetes_w_chronic_complications <- c(
    "E102", "E103", "E104", "E105", "E107", "E112", "E113", "E114", "E115", "E117", "E132", "E133", "E134",
    "E135", "E137", "E142", "E143", "E144", "E145", "E147"
  )

  metastatic_solid_tumor <- c("C77", "C78", "C79", "C80")

  any_malignancy_incl_lymph_leuk <- c(
    "C00", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12", "C13", "C14",
    "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23", "C24", "C25", "C26", "C30", "C31", "C32",
    "C33", "C34", "C37", "C38", "C39", "C40", "C41", "C43", "C45", "C46", "C47", "C48", "C49", "C50", "C51",
    "C52", "C53", "C54", "C55", "C56", "C57", "C58", "C60", "C61", "C62", "C63", "C64", "C65", "C66", "C67",
    "C68", "C69", "C70", "C71", "C72", "C73", "C74", "C75", "C76", "C81", "C82", "C83", "C84", "C85", "C88",
    "C90", "C91", "C92", "C93", "C94", "C95", "C96", "C97"
  )

  cancers <- c(metastatic_solid_tumor, any_malignancy_incl_lymph_leuk)

  ip_type_3 <- ipdiag[
    (diagnosis_type == 3) &
      (substr(diagnosis_code, 0, 4) %in% diabetes_w_chronic_complications | substr(diagnosis_code, 0, 3) %in% cancers),
    c("genc_id", "diagnosis_code", "diagnosis_type")
  ] %>%
    .[, genc_diag := paste(genc_id, diagnosis_code)]

  ipcharl <- rbind(ip_type_1wxy, ip_type_3)

  # From CIHI methodology notes (page 19):
  #   "For cases without a type 6 diagnosis code:
  #     – If a patient had a qualifying Charlson diagnosis code as type 1, W, X, Y or 3 (for selected cases),
  #       and this same code also appeared as the MRDx or type 2, then this type 1, W, X, Y or 3 code was not
  #       included in the Charlson calculation."

  type6 <- ipdiag[diagnosis_type == 6, genc_id]
  no_type6 <- ipdiag[!genc_id %in% type6, genc_id]

  type2_or_mrdx <- ipdiag[diagnosis_type == 2 | diagnosis_type == "M", c("genc_id", "diagnosis_code")] %>%
    .[, genc_diag := paste(genc_id, diagnosis_code)]

  ipcharl <- ipcharl[, exclude := ifelse(genc_id %in% no_type6 & genc_diag %in% type2_or_mrdx$genc_diag, 1, 0)] %>%
    .[exclude == 0] %>%
    .[, exclude := NULL]

  # - From CIHI methodology notes (page 19):
  #   "For cases with a type 6 diagnosis code:
  #     – The original type 6 code is not included in the Charlson calculation.
  #     – The original MRDx is included in the Charlson calculation if this diagnosis code is not also a type 2 code

  type2 <- ipdiag[genc_id %in% type6 & diagnosis_type == 2, c("genc_id", "diagnosis_code")] %>%
    .[, genc_diag := paste(genc_id, diagnosis_code)]

  mrdx <- ipdiag[genc_id %in% type6 & diagnosis_type == "M", c("genc_id", "diagnosis_code", "diagnosis_type")] %>%
    .[, genc_diag := paste(genc_id, diagnosis_code)]

  type6_w_mrdx_not_type2 <- mrdx[!genc_diag %in% type2$genc_diag]

  # combine these with existing qualifying diagnoses
  ipcharl <- rbind(ipcharl, type6_w_mrdx_not_type2)

  # - From CIHI methodology notes (page 19):
  #   "If a patient had a qualifying Charlson diagnosis code as type 1, W, X, Y or 3 (for selected cases),
  #    and this same code also appeared as type 6 or type 2, then this type 1, W, X, Y or 3 code was not included
  #    in the Charlson calculation.

  type2_or_type6 <- ipdiag[
    diagnosis_type == 2 | diagnosis_type == 6, c("genc_id", "diagnosis_code", "diagnosis_type")
    ] %>%
    .[, genc_diag := paste(genc_id, diagnosis_code)]

  ipcharl <- ipcharl[, exclude := ifelse(genc_diag %in% type2_or_type6$genc_diag, 1, 0)] %>%
    .[exclude == 0] %>%
    .[, exclude := NULL]

  ### Combine er-diagnoses and inpatient-diagnoses as diagnoses at admission
  if (!is.null(erdiag)){
    res <- rbind(ipcharl[, -c("genc_diag")], erdiag[, c("genc_id", "diagnosis_code", "diagnosis_type")])
  } else {
    res <- ipcharl[, -c("genc_diag")]
  }

  return(res)
}


#' @title
#' Compute Charlson comorbidity index (CCI) score
#'
#' @description
#' Based on the methodology from [Quan et al. 2011](https://pubmed.ncbi.nlm.nih.gov/21330339/)
#' using ICD-10 codes as the mapping algorithm,
#' implemented in the R [comorbidity](https://ellessenne.github.io/comorbidity/index.html) package
#' by [Gasparini, 2018](https://doi.org/10.21105/joss.00648).
#'
#' Can compute either CCI score at admission based on
#' [HSMR Methodology](https://www.cihi.ca/sites/default/files/document/hospital-standardized-mortality-ratio-meth-notes-en.pdf)
#' or for the entire patient encounter. The default is to compute comorbidity at admission.
#'
#' @inheritParams diagnoses_at_admission
#'
#' @param raw_comorbidities (`logical`)\cr
#' Whether to output a `data.table` of raw comorbidities as opposed to pre-computed charlson scores.
#'
#' @param at_admission (`logical`)\cr
#' Whether to calculate the charlson score for all comorbidities or for only pre-admit comorbidities.
#'
#' @return (`data.table`)\cr
#' By default, for each encounter, outputs the Charlson comorbidity index. If `raw_comorbidities` is `TRUE`,
#' outputs a wide `data.table` with a column for each comorbidity for each encounter.
#'
#' @note
#' A hierarchy of comorbidities is used when calculating the Charlson score, but not when outputting `raw_comorbidites`.
#' This affects comorbidities present in a patient with different degrees of severity. See documentation for
#' [comorbidity::comorbidity()] for details.
#'
#' @importFrom comorbidity comorbidity score
#' @export
#'
charlson_comorbidity_index <- function(ipdiag, erdiag, at_admission = TRUE, raw_comorbidities = FALSE) {

  ## users can set erdiag to NULL to exclude ER diagnoses; but warning will be shown
  if (is.null(erdiag)) {
    cat("\n*** Based on the input you provided, only in-patient diagnoses (ipdiag) will be included in the Charlson Comorbidity Index.
    If you want to include ER diagnoses, please provide the correspondig table as an input to `erdiag`. ***\n")
  }

  ## check that ipdiag/erdiag contains genc_id & diagnosis_code
  check_input(ipdiag, c("data.table", "data.frame"),
              colnames = c("genc_id", "diagnosis_code"),
              coltypes = c("", "character"))
  if (!is.null(erdiag)){
    check_input(erdiag, c("data.table", "data.frame"),
                colnames = c("genc_id", "er_diagnosis_code"),
                coltypes = c("", "character"))
  }


  diagnoses <- if (at_admission) {
    diagnoses_at_admission(ipdiag, erdiag)
  } else {
    if (!is.null(erdiag)){
      rbind(
        erdiag[, .(genc_id, "diagnosis_code" = er_diagnosis_code, "diagnosis_type" = er_diagnosis_type)],
        ipdiag[, .(genc_id, diagnosis_code, diagnosis_type)]
      ) } else {
        diagnoses <- ipdiag[, .(genc_id, diagnosis_code, diagnosis_type)]
      }
  }

  charlson <- comorbidity(
    x = diagnoses,
    id = "genc_id",
    code = "diagnosis_code",
    map = "charlson_icd10_quan",
    assign0 = FALSE
  )

  if (raw_comorbidities) {
    if (at_admission) {
      attr(charlson, "variable.labels") <- paste(attr(charlson, "variable.labels"), "(at admission)")
    }

    return(charlson)
  }

  scores <- score(charlson, weights = "quan", assign0 = TRUE)

  res <- cbind(charlson$genc_id, scores) %>% as.data.table()

  if (at_admission) {
    names(res) <- c("genc_id", "admit_charlson_derived")
  } else {
    names(res) <- c("genc_id", "all_charlson_derived")
  }

  return(res)
}
