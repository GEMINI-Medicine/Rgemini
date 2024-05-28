#' @title Identify CCSR categories for ICD-10-CA diagnosis codes
#'
#' @description
#' CCSR [Clinical Classifications Software Refined](https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp)
#' provides a grouping of individual ICD-10 diagnosis codes into broader, clinically meaningful disease categories. The
#' original CCSR grouping was developed based on US ICD-10-CM codes. GEMINI developed an algorithm mapping Canadian
#' diagnosis codes (ICD-10-CA) to CCSR categories.
#' The full mapping procedure is described [here](https://www.medrxiv.org/content/10.1101/2022.11.29.22282888v1).
#'
#' This function returns the GEMINI-derived CCSR mapping for each ICD-10-CA diagnosis in the `diagnosis_table` input.
#'
#' By default, the function will only return the CCSR category(s) for the most responsible discharge diagnosis (MRDx).
#' This typically refers to type-M diagnoses, unless a patient has a type-6 diagnosis (see
#' [CIHI definition of MRDx](https://www.cihi.ca/sites/default/files/document/hospital-standardized-mortality-ratio-meth-notes-en.pdf)
#' page 13). Users can choose to obtain CCSR categories for all diagnosis types by setting
#' `type_mrdx` to `FALSE`.
#'
#' @concept diagnoses, CCSR, ICD-10
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param dxtable (`data.frame` | `data.table`)
#' Table containing ICD-10-CA diagnosis codes of interest. Typically, this refers to the `ipdiagnosis` table, which
#' contains the CIHI in-patient diagnoses for each encounter (see
#' [GEMINI database schema](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#'
#' If a different type of diagnosis table is provided as input (e.g., `erdiagnosis`), please make sure the table
#' contains a column named `diagnosis_code` (`character`) where each row refers to a single, alphanumeric diagnosis
#' code consisting of 3-7 characters. If `type_mrdx` is set to `TRUE`, the following additional columns are required:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `diagnosis_type` (`character`): Type of each diagnosis code according to
#' [CIHI diagnosis type definitions](https://www.cihi.ca/sites/default/files/document/diagnosis-type-definitions-en.pdf).
#'
#' Note, each encounter may have multiple rows, referring to diagnosis codes of different types. However, typically,
#' each encounter should only have a single MRDx. In case `type_mrdx` is set to `TRUE`, the function will return a
#' warning if any encounters have multiple `MRDx` codes. By default, those encounters will be returned with multiple
#' rows (one per MRDx). However, users may specify `unique_mrdx = TRUE` (see below), which will cause the function to
#' quit in case multiple `MRDx` codes were found.
#'
#' @param type_mrdx (`logical`)
#' Flag indicating whether to filter by most responsible discharge diagnosis.
#' If `TRUE` (default), only MRDx codes (type 6 if present, otherwise, type M) will be returned. This option is
#' recommended and should typically return a single diagnosis code per encounter. If multiple `MRDx` codes are found,
#' a warning message will be shown (and if `unique_mrdx = TRUE`, the function will stop). Additionally, the function
#' will show a warning if there are any encounters with missing MRDx code.
#' If `type_mrdx = FALSE`, the function Will return all diagnosis codes (of any diagnosis type) that are present in
#' `dxtable`. In that case, any customized filtering for specific diagnosis types should be applied by the user outside
#' of this function. When using this option, please refer to the function's vignette to find out more about potential
#' issues when interpreting default CCSR categories for non-MRDx diagnosis types.
#'
#' @param unique_mrdx (`logical`)
#' Flag indicating whether the requirement for a unique MRDx code per encounter is strict. This is only relevant when
#' `type_mrdx = TRUE`.
#' If `FALSE` (default), multiple MRDx codes/encounter will only result in a warning to inform the user. The function
#' will finish running and will return multiple rows per encounter.
#' If `TRUE`, the function will quit if any `genc_ids` with multiple MRDx codes are found and the user is advised to
#' investigate the cause of multiple MRDx codes.
#'
#' @param replace_invalidpdx (`logical`)
#' Flag indicating whether to replace invalid default CCSR categories (default = `TRUE`).
#' If set to `FALSE`, the function will return the CCSR default categories as derived based on the original US CCSR
#' tool, which may include some ICD-10-CA codes where `ccsr_default = 'XXX000'` ("invalid PDX"; see below for details).
#' If set to `TRUE` (default), `XXX000` values will be replaced with one of the other (valid) CCSR categories 1-6 that
#' have been mapped to a given code. Specifically, if a given ICD-10-CA code has been mapped to a single CCSR category
#' (`ccsr_1`), `XXX000` will be replaced with that category.  Otherwise, if a code has been mapped to multiple CCSR 1-6
#' categories, the function will check which one of those is the most frequent CCSR default category among ICD-10-CA
#' codes that start with the same first 3 characters (same ICD-10 chapter). If other codes from the same ICD-10 chapter
#' don't share the same CCSR default category (or default is `XXX000` for all of them), `ccsr_1` is used as default.
#'
#' @return `data.table`
#' This function returns a table containing the ICD-10-CA diagnosis codes of interest, together with their corresponding
#' (GEMINI-derived) CCSR category(s).
#' For each row in the output table, the following variables are returned:
#' - `diagnosis_code`: ICD-10-CA code
#' - `diagnosis_code_desc`: description of the ICD-10-CA code
#' - `ccsr_default`: default CCSR category, which is the main disease group that was assigned to each diagnosis code and
#' is typically the main category of interest to be used in further analyses.
#' - `ccsr_default_desc`: description of the CCSR default category
#' - `ccsr_1` through `ccsr_6`: All 1-6 CCSR categories that were mapped to a given code. Although `ccsr_default` is
#' typically the main category of interest, some ICD-10-CA codes are mapped to more than 1 (up to 6)
#' CCSR categories. In some circumstances, it may be of interest to analyse all 1-6 CCSR categories, instead of only
#' analyzing the default CCSR. If required, the descriptions for CCSR 1-6 categories can be obtained from the
#' `lookup_ccsr` table.
#'
#'
#' @note
#' The GEMINI-derived CCSR mappings were validated by clinical experts; however, users are advised to spot-check the
#' mappings to ensure accuracy. Please notify the GEMINI team if you encounter any issues. Some mappings were derived
#' based on an older version of CCSR (v2020.3) and may be subject to change with future updates.
#'
#' For some diagnosis codes, `ccsr_default` will be `NA` (`ccsr_default_desc = 'Unmapped'`), which indicates that the
#' diagnosis code has not been mapped to any CCSR category yet. This is less likely to happen if `type_mrdx` is set to
#' `TRUE` (default) since the vast majority of MRDx codes have been mapped by GEMINI.
#' Note: Type-4 morphology codes will always be returned as `'Unmapped'` since they are numeric codes based on ICD-0
#' (oncology) diagnoses, which are not meant to be mapped to CCSR categories.
#'
#' Some diagnosis codes will be returned with `ccsr_default = 'XXX000'`, which indicates that the diagnosis code is
#' not a valid in-patient principal diagnosis (PDX) according to US diagnostic coding following Medicare Code Edits
#' guidelines (see [here](https://hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR-User-Guide-v2023-1.pdf). Due to
#' mapping ambiguity and differences in coding between the US and Canada, you may find that some ICD-10-CA MRDx codes
#' are returned as `'XXX000'`(invalid PDX). Additionally, for most clinical research projects, the distinction between
#' valid vs. invalid principle diagnosis coding may not be relevant. Similarly, if users are interested in CCSR groups
#' for all diagnosis types (`type_mrdx = FALSE`), the definition of CCSR default categories as valid vs. invalid PDX
#' categories might not be meaningful. Therefore, by default, `replace_invalidpdx` is set to `TRUE`. However, please
#' carefully read the documentation to understand how exactly invalid PDX categories are replaced within this function.
#'
#' @seealso `vignette("icd_to_ccsr", package = "Rgemini")`
#' The vignette provides further context and some example code illustrating how to use CCSR categories in analyses.
#'
#'
#' @import DBI RPostgreSQL
#' @importFrom stringr str_sub
#'
#' @export
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'                         dbname = "db",
#'                         host = "domain_name.ca",
#'                         port = 1234,
#'                         user = getPass("Enter user:"),
#'                         password = getPass("password"))
#'
#' dxtable <- dbGetQuery(dbcon, "select * from ipdiagnosis") %>% data.table()
#' icd_to_ccsr(dbcon, dxtable)
#' }
icd_to_ccsr <- function(dbcon, dxtable, type_mrdx = TRUE, unique_mrdx = FALSE, replace_invalidpdx = TRUE) {

  mapping_message("ICD-10-CA codes to CCSR categories")

  cat(paste0(
    "\nObtaining CCSR categories for ICD-10-CA codes in input table ",
    deparse(substitute(dxtable)), "\n "))

  #######  Check user inputs  #######
  ## Valid DB connection?
  if (!isPostgresqlIdCurrent(dbcon)) {
    stop("Invalid user input for argument dbcon. Please input a valid database connection.")
  }

  ## dxtable provided as data.frame/data.table?
  if (!any(class(dxtable) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for argument dxtable.
         Please provide a data frame (or data table) containing diagnosis codes.")
  }

  ## Missing columns in dxtable? (depending on whether type_mrdx == T/F)
  if (type_mrdx == TRUE && any(!c("genc_id", "diagnosis_code", "diagnosis_type") %in% names(dxtable))) {
    stop("Input dxtable is missing at least one of the following variables:
          genc_id, diagnosis_code, and/or diagnosis_type.
          Please refer to the function documentation for more details.")
  } else if (type_mrdx == FALSE && !"diagnosis_code" %in% names(dxtable)) { # only diagnosis_code column required
    stop("Input dxtable is missing 'diagnosis_code' variable.
         Please refer to the function documentation for more details.")
  }

  ## Invalid flags?
  if (any(!is.logical(c(type_mrdx, unique_mrdx, replace_invalidpdx)))) {
    stop("The following input arguments have to be logical (TRUE|FALSE): type_mrdx, unique_mrdx, replace_invalidpdx.
          Please refer to the function documentation for more details.")
  }

  ## Plausible flags? unique_mrdx = TRUE does not apply when type_mrdx = FALSE
  if (unique_mrdx == TRUE && type_mrdx == FALSE) {
    warning("Ignoring user input 'unique_mrdx = TRUE' because type_mrdx is set to FALSE.
            Please refer to the function documentation for more details.", immediate. = TRUE)
  }

  #######  Load relevant lookup tables from DB  #######
  ## ICD-to-CCSR mapping table
  lookup_icd_to_ccsr <- dbGetQuery(dbcon, "select * from lookup_icd10_ca_to_ccsr; ") %>% as.data.table()
  ## CCSR category descriptions
  lookup_ccsr <- dbGetQuery(dbcon, "select * from lookup_ccsr; ") %>%
    rename(ccsr_default = ccsr, ccsr_default_desc = ccsr_desc) %>%
    as.data.table()
  ## ICD-10-CA diagnosis code descriptions
  lookup_icd10_ca <- dbGetQuery(dbcon, "select * from lookup_icd10_ca_description; ") %>%
    rename(diagnosis_code_desc = long_description) %>%
    as.data.table()



  #######  Prepare data  #######
  ## clean up dxtable
  dxtable <- coerce_to_datatable(dxtable)
  dxtable[dxtable == ""] <- NA
  # add ICD-10-CA code descriptions
  dxtable <- merge(dxtable, lookup_icd10_ca[, .(diagnosis_code, diagnosis_code_desc)],
                   by = "diagnosis_code", all.x = TRUE)

  ## clean up CCSR lookup table
  lookup_icd_to_ccsr[lookup_icd_to_ccsr == ""] <- NA
  # add CCSR category descriptions to default CCSR categories
  lookup_icd_to_ccsr <- merge(lookup_icd_to_ccsr, lookup_ccsr, by = "ccsr_default", all.x = TRUE)
  # change column order for clarity
  setcolorder(lookup_icd_to_ccsr,
              c("ccsr_default", "ccsr_default_desc",
                first(setdiff(names(lookup_icd_to_ccsr), c("ccsr_default", "ccsr_default_desc")))))


  #######  Filter by diagnosis type  #######
  ## Default setting: only include MRDx (type-6 if present, otherwise type-M)
  if (type_mrdx == TRUE) {
    # if present, use type-6 diagnosis; otherwise, type-M diagnosis
    type_6 <- dxtable[diagnosis_type == "6" & !is.na(diagnosis_code), ]
    type_M <- dxtable[diagnosis_type == "M" & !is.na(diagnosis_code) & !genc_id %in% type_6$genc_id, ]

    dxtable_final <- rbind(type_6, type_M)
  } else { # include all rows (any diagnosis types) that exist in dxtable
    cat("\nAll diagnosis types will be returned in the output.\n")
    dxtable_final <- dxtable
  }



  #######  Quality checks  #######
  ## 1) Check for duplicated diagnosis codes in ICD-to-CCSR lookup table (each code should have unique mapping to CCSR)
  if (sum(duplicated(lookup_icd_to_ccsr$diagnosis)) > 0) {
    warning(paste0("Duplicate entries in lookup table.
      The ICD-to-CCSR lookup table in the database contained duplicate entries for ",
      sum(duplicated(lookup_icd_to_ccsr$diagnosis)), " diagnosis codes.
      Each ICD-10-CA code should have a unique mapping to its corresponding CCSR category(s).
      Therefore, all duplicated entries have been automatically removed by this function.
      Please contact Gemini[dot]Data[at]unityhealth[dot]to in order to notify us about this issue."), immediate. = TRUE)

    # remove any duplicated ICD-10-CA codes (select mapping based on latest CCSR version)
    lookup_icd_to_ccsr <- lookup_icd_to_ccsr[order(ccsr_version)][, .SD[.N], by = diagnosis_code]
  }

  ## 2) Check for unique MRDx code per encounter
  # only when type_mrdx == TRUE; otherwise, multiple dx per genc_id are acceptable/expected
  if (type_mrdx == TRUE) {
    multi_mrdx <- dxtable_final[, .N, by = "genc_id"]
    if (any(multi_mrdx$N > 1)) {
      warning(paste0("Multiple MRDx codes per encounter.
      ", length(unique(multi_mrdx[N > 1, genc_id])), " encounters have more than 1 MRDx diagnosis code.
      Typically, each encounter should have exactly 1 MRDx (type-6 or type-M) diagnosis code.
      Multiple MRDx codes could be due to the following reasons:
         1) If you combined in-patient & ED diagnosis codes, each encounter may have 2 MRDx codes.
         2) You may have created multiple MRDx rows per genc_id when pre-processing the input diagnosis table
            (e.g., due to merging with other tables, or creating long-format data).
            If 1) or 2) were intended for the purpose of your analyses, you can ignore this warning message.
         3) If 1) and 2) do not apply, multiple MRDx codes may indicate a data quality issue that should only affect a small percentage of the cohort\n "),
        immediate. = TRUE
      )

      if (unique_mrdx == TRUE) { # if unique MRDx criterion is strict
        stop("Function terminated due to multiple MRDx codes per encounter.")
      }
    }
  }

  ## 3) Check for missing diagnosis codes
  if (type_mrdx == TRUE) { # if MRDx: Any genc_ids without MRDx code?
    # any genc_id's in original input table that don't exist anymore after filtering by MRDx codes?
    # i.e., missing/empty type-6/-M diagnosis codes
    missing_mrdx <- dxtable[!genc_id %in% dxtable_final$genc_id, ]
    if (nrow(missing_mrdx) > 0) {
      warning(paste0("Missing MRDx codes.
      ", length(unique(missing_mrdx$genc_id)),
      ' genc_ids in the diagnosis table input do not have any MRDx (type-6 or type-M) diagnosis code.
      These encounters are returned with diagnosis_code = NA and ccsr_default_desc = "Missing diagnosis code".
      Missing MRDx codes may reflect a data quality issue, which should only affect a very small percentage of encounters (<0.01%).'),
        immediate. = TRUE
      )
    }
  } else { # if including all diagnosis types, just check for any NAs in diagnosis_code
    missing_dx <- dxtable[is.na(diagnosis_code), ]
    if (nrow(missing_dx) > 0) {
      warning(paste0("Missing diagnosis codes.", nrow(missing_dx),
      ' diagnosis codes in the input table have missing values (diagnosis_code = NA).
      These rows are returned with ccsr_default_desc = "Missing diagnosis code".
      Missing codes likely reflect a data quality issue, which should only affect a very small percentage of encounters (<0.01%).'),
      immediate. = TRUE)
    }
  }



  #######  Get ICD-to-CCSR mapping  #######
  # merge with lookup table
  dxtable_final <- merge(dxtable_final, lookup_icd_to_ccsr[, -c("gemini_derived", "ccsr_version")],
                         by = "diagnosis_code", all.x = TRUE)
  dxtable_final[is.na(ccsr_default_desc), ccsr_default_desc := "Unmapped"]



  #######  Replace invalid PDX (ccsr_default = 'XXX000')  #######
  if (replace_invalidpdx == TRUE) {
    #######  find all invalid PDX in cohort
    invalid_pdx <- dxtable_final[grepl("XXX", ccsr_default), ]

    if (nrow(invalid_pdx) > 0) {
      cat(paste0("\nReplacing CCSR default categories for ", length(unique(invalid_pdx$diagnosis_code)),
      " unique diagnosis codes  (n = ", nrow(invalid_pdx), ' encounters) that were mapped to "invalid PDX".
      Please refer to the function documentation for details.\n '))


      #######  Create new lookup table for any codes that were mapped to XXX000
      lookup_icd_to_ccsr_new <- copy(lookup_icd_to_ccsr)

      ### 1) For codes with only 1 CCSR category:
      # use ccsr_1 as default CCSR
      lookup_icd_to_ccsr_new[grepl("XXX", ccsr_default) & is.na(ccsr_2), ccsr_default := ccsr_1]

      ### 2) For codes with more than 1 CCSR category:
      # use most frequent CCSR default category among codes from the same ICD chapter (same first 3 characters)

      # 2a) find all CCSR categories among each ICD-10 chapter (ignoring XXX000)
      ccsr_chapter <- lookup_icd_to_ccsr_new[, icd_chapter := str_sub(diagnosis_code, 1, 3)] # get ICD chapter
      ccsr_chapter <- ccsr_chapter[, .N, by = c("icd_chapter", "ccsr_default")] # frequency of CCSR defaults by chapter
      ccsr_chapter <- ccsr_chapter[!grepl("XXX", ccsr_default), ] # remove any invalid PDX from this lookup table

      # 2b) get potential default categories for each diagnosis code based on ICD chapter
      lookup_icd_to_ccsr_new2 <- merge(
        lookup_icd_to_ccsr_new[grepl("XXX", ccsr_default) & !is.na(ccsr_2), .(
          diagnosis_code, icd_chapter, ccsr_1, ccsr_2, ccsr_3, ccsr_4, ccsr_5, ccsr_6)],
        ccsr_chapter,
        by = "icd_chapter", all.x = TRUE, allow.cartesian = TRUE)

      # 2c) only keep rows where potential default category is among CCSR 1-6 of corresponding code
      lookup_icd_to_ccsr_new2 <- lookup_icd_to_ccsr_new2[apply(lookup_icd_to_ccsr_new2, 1, function(row) {
        any(row["ccsr_default"] == row[c("ccsr_1", "ccsr_2", "ccsr_3", "ccsr_4", "ccsr_5", "ccsr_6")])
      }) == TRUE, ]

      # 2d) sort default categories by frequency and only most frequent CCSR default
      lookup_icd_to_ccsr_new2 <- lookup_icd_to_ccsr_new2[
        order(diagnosis_code, desc(N))][, .SD[1], by = "diagnosis_code"]

      # 2e) add updated mappings for codes with > 1 CCSR category to new lookup table
      lookup_icd_to_ccsr_new <- lookup_icd_to_ccsr_new[
        diagnosis_code %in% lookup_icd_to_ccsr_new2$diagnosis_code,
        ccsr_default := lookup_icd_to_ccsr_new2[.SD, ccsr_default, on = "diagnosis_code"]]


      ### 3) For any remaining codes:
      # if ccsr_default is still 'XXX000', simply use CCSR 1 category. This can happen if...
      # ...1) there are no default CCSR categories other than 'XXX000' among chapter codes, OR
      # ...2) there are other CCSR categories but they are not shared by the searched code
      # [note: this mostly affects V/W/X/Y codes, which typically all have XXX000 as default;
      # those are codes referring to external causes of injury/morbidity, health status, or
      # intentional self-harm and are unlikely to be MRDx]
      lookup_icd_to_ccsr_new[grepl("XXX", ccsr_default), ccsr_default := ccsr_1]


      ### Update mappings according to new lookup table
      dxtable_final[diagnosis_code %in% invalid_pdx$diagnosis_code, ccsr_default :=
                      lookup_icd_to_ccsr_new[.SD, ccsr_default, on = "diagnosis_code"]]
      # Update CCSR default description for invalid PDX according to newly assigned ccsr_default
      dxtable_final[diagnosis_code %in% invalid_pdx$diagnosis_code, ccsr_default_desc :=
                      lookup_ccsr[.SD, ccsr_default_desc, on = "ccsr_default"]]

      ## Test: Check that no more 'XXX000' among default categories
      if (nrow(dxtable_final[grepl("XXX", ccsr_default), ]) == 0) {
        message("all 'invalid PDX' (ccsr_default == 'XXX000') have been replaced.")

      } else {
        message("some 'invalid PDX' (ccsr_default == 'XXX000') have not been replaced.")
      }
    }
  }



  #######  Prepare final output  #######
  ## Missing Dx/MRDx codes: diagnosis_code/type = NA and ccsr_default_desc = "Missing diagnosis code"
  if (type_mrdx == TRUE) {
    if (nrow(missing_mrdx) > 0) {
      # append unique genc_ids with missing MRDx
      dxtable_final <- rbind(dxtable_final, unique(missing_mrdx[, "genc_id"]),
                             fill = TRUE)[order(genc_id)] # diagnosis_code = NA for missing MRDx
    }
  }
  dxtable_final[is.na(diagnosis_code), ccsr_default_desc := "Missing diagnosis code"]


  ## Return all columns contained in original dxtable input
  # if genc_id/diagnosis_type exist, put them first for clarity
  if ("diagnosis_type" %in% names(dxtable_final)) {
    setcolorder(dxtable_final, c("diagnosis_type", setdiff(names(dxtable_final), "diagnosis_type")))
    dxtable_final <- dxtable_final[order(diagnosis_type)]
  }
  if ("genc_id" %in% names(dxtable_final)) {
    setcolorder(dxtable_final, c("genc_id", setdiff(names(dxtable_final), "genc_id")))
    dxtable_final <- dxtable_final[order(genc_id)]
  }



  return(dxtable_final)
}
