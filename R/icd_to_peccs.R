#' @title Identify PECCS-CA categories for ICD-10-CA diagnosis codes
#'
#' @description
#' PECCS-CA (Pediatric Clinical Classification System) provides a grouping of individual ICD-10-CA diagnosis codes
#' into broader, clinically meaningful disease categories.
#'
#' This function returns the PECCS-CA mapping for each ICD-10-CA diagnosis in the `dxtable` input.
#'
#' The function will only return the PECCS-CA category for the most responsible discharge diagnosis
#' (MRDx). This function uses the M-type diagnosis as MRDx.
#'
#' @concept diagnoses, PECCS-CA, ICD-10
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param dxtable (`data.frame` | `data.table`)
#' Table containing ICD-10-CA diagnosis codes of interest. Typically, this refers to the `ipdiagnosis` table, which
#' contains the CIHI in-patient diagnoses for each encounter (see
#' [GEMINI database schema](https://geminimedicine.ca/the-gemini-database/)).
#'
#' If a different type of diagnosis table is provided as input (e.g., `erdiagnosis`), please make sure the table
#' contains a column named `diagnosis_code` (`character`) where each row refers to a single, alphanumeric diagnosis
#' code consisting of 3-7 characters. In addition to `diagnosis_code`, ensure the table contains both `genc_id` and
#' `diagnosis_type`.
#'
#' Note, each encounter may have multiple rows, referring to diagnosis codes of different types. However, typically,
#' each encounter should only have a single MRDx.
#'
#' @return `data.table`
#' This function returns a table containing the ICD-10-CA diagnosis codes of interest, together with their
#' corresponding PECCS-CA category.
#' For each row in the output table, the following variables are returned:
#' - `diagnosis_code`: ICD-10-CA code
#' - `diagnosis_code_desc`: description of the ICD-10-CA code
#' - `peccs_ca_code`: PECCS-CA category code
#' - `peccs_ca_category_description`: description of the PECCS-CA category
#'
#' @note
#' For some diagnosis codes, `peccs_ca_code` will be `NA` (`peccs_ca_category_description = 'Unmapped'`), which
#' indicates that the diagnosis code has not been mapped to any PECCS-CA category yet.
#'
#' Encounters with a missing diagnosis code are returned with `diagnosis_code = NA` and
#' `peccs_ca_category_description = 'Missing diagnosis code'`.
#'
#' @import DBI
#'
#' @export
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
#' dxtable <- dbGetQuery(dbcon, "select * from ipdiagnosis") %>% data.table()
#' icd_to_peccs(dbcon, dxtable)
#' }
icd_to_peccs <- function(dbcon, dxtable) {
  mapping_message("ICD-10-CA codes to PECCS categories")

  cat(paste0(
    "\nObtaining PECCS categories for ICD-10-CA codes in input table ",
    deparse(substitute(dxtable)), "\n "
  ))

  #######  Check user inputs  #######
  ## Valid DB connection?
  check_input(dbcon, "DBI")

  ## dxtable provided as data.frame/data.table?
  if (!any(class(dxtable) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for argument dxtable.
         Please provide a data frame (or data table) containing diagnosis codes.")
  }

  ## check for missing columns in diagnosis table
  if (any(!c("genc_id", "diagnosis_code", "diagnosis_type") %in% names(dxtable))) {
    stop("Input dxtable is missing at least one of the following variables:
          genc_id, diagnosis_code, and/or diagnosis_type.
          Please refer to the function documentation for more details.")
  }

  ## warn users that the function ONLY uses type-M diagnoses
  warning("This function only returns PECCS codes for M-type diagnoses", immediate. = TRUE)

  ## load lookup table from db
  peccs_lookup <- dbGetQuery(dbcon, "select * from lookup_icd10_ca_to_peccs") %>%
    data.table()

  ## align lookup column names with GEMINI diagnosis table conventions
  setnames(peccs_lookup,
    old = c("icd_10_ca_code", "icd_10_ca_code_description"),
    new = c("diagnosis_code", "diagnosis_code_desc")
  )

  #######  Prepare data  #######
  ## clean up dxtable
  dxtable <- coerce_to_datatable(dxtable)


  ## set empty values to NA
  dxtable[dxtable == ""] <- NA

  ## filter for M-type diagnoses, merge with PECCS table
  dxtable_final <- merge(dxtable[diagnosis_type == "M" & !is.na(diagnosis_code)],
    peccs_lookup[, .(
      diagnosis_code, diagnosis_code_desc,
      peccs_ca_code, peccs_ca_category_description
    )],
    by = "diagnosis_code", all.x = TRUE
  )

  #######  Quality checks  #######
  ## 1) Check for unique MRDx code per encounter
  multi_mrdx <- dxtable_final[, .N, by = "genc_id"]
  if (any(multi_mrdx$N > 1)) {
    warning(paste0("Multiple MRDx codes per encounter.
      ", length(unique(multi_mrdx[N > 1, genc_id])), " encounter(s) have more than 1 MRDx diagnosis code.
      Typically, each encounter should have exactly 1 MRDx (type-M) diagnosis code.
      Multiple MRDx codes could be due to the following reasons:
         1) If you combined in-patient & ED diagnosis codes, each encounter may have 2 MRDx codes.
         2) You may have created multiple MRDx rows per genc_id when pre-processing the input diagnosis table
            (e.g., due to merging with other tables, or creating long-format data).
            If 1) or 2) were intended for the purpose of your analyses, you can ignore this warning message.
         3) If 1) and 2) do not apply, multiple MRDx codes may indicate a data quality issue that should only affect a small percentage of the cohort\n "),
      immediate. = TRUE
    )
  }

  ## 2) Check for missing diagnosis codes
  # check for genc_ids without MRDx code
  missing_mrdx <- dxtable[!genc_id %in% dxtable_final$genc_id, ]
  if (nrow(missing_mrdx) > 0) {
    warning(
      paste0(
        "Missing MRDx codes.
      ", length(unique(missing_mrdx$genc_id)),
        ' genc_id(s) in the diagnosis table input do not have any MRDx (type-M) diagnosis code.
      These encounters are returned with diagnosis_code = NA and peccs_ca_category_description = "Missing diagnosis code".
      Missing MRDx codes may reflect a data quality issue, which should only affect a very small percentage of encounters (<0.01%).'
      ),
      immediate. = TRUE
    )
  }

  #######  Prepare final output  #######
  ## Missing Dx/MRDx codes: diagnosis_code/type = NA and peccs_ca_category_description = "Missing diagnosis code"
  if (nrow(missing_mrdx) > 0) {
    # append unique genc_ids with missing MRDx
    dxtable_final <- rbind(dxtable_final, unique(missing_mrdx[, "genc_id"]),
      fill = TRUE
    )[order(genc_id)] # diagnosis_code = NA for missing MRDx
  }

  ## convert missing descriptions to unmapped (if any)
  dxtable_final[is.na(peccs_ca_category_description), peccs_ca_category_description := "Unmapped"]

  ## handle cases of missing diagnosis codes
  dxtable_final[is.na(diagnosis_code), peccs_ca_category_description := "Missing diagnosis code"]

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
