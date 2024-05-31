#' @title
#' Compute the number of routine bloodwork tests per encounter
#'
#' @description
#' `n_routine_bloodwork` returns the number of routine bloodwork (Sodium and
#' Hemoglobin tests) for each hospital admission. Sodium and Heamoglobin tests
#' are defined by OMOP codes. Sodium is the code 3019550. Hemoglobin is 3000963.
#'
#' @details
#' This function takes a list of admissions and DRM table equivalent of lab
#' table (see
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5))
#' to generate numeric fields counting the number of Sodium and Hemoglobin tests
#' for each admission.
#'
#' Lab table in the database should include field that classifies each test into
#' a few different categories. Therefore, this function should be run after some
#' standardization efforts on lab table (performed by GEMINI). Currently, the
#' number of routine bloodwork tests is one of the performance metrics in
#' MyPracticeReport.
#'
#' Function automatically removes tests without valid numeric result value.
#' Exceptions are boundary result value (e.g. ">120" mmol/L), and these
#' are still counted as valid tests.
#'
#' Any tests being performed in both ED and in-patient settings will be counted.
#'
#' @section Warning:
#' Function returns data.table with id field `genc_id` and one numeric field
#' indicating the number of bloodwork tests per encounter. By design,
#' function will not return any NA values. If a `genc_id` does not have any
#' entries in the "lab" table, the admission gets assigned 0 number of tests.
#' When one tries to left-join the output of this function with another table
#' (another list of admissions in the left), make sure list of admissions (or
#' patient) aligns in both tables
#'
#' This function requires mappings by a Subject Matter Expert to ensure that all
#' tests are mapped to
#' 3019550 - Sodium (Moles/volume) in Serum or Plasma, and
#' 3000963 - Hemoglobin (Mass/volume) in Blood
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#'
#' @import RPostgreSQL
#' @return
#' data.table object with the same number of rows as input "cohort", with
#' additional derived numeric field labelled as "n_routine_bloodwork_derived"
#'
#' @export
#'

n_routine_bloodwork <- function(dbcon,
                                cohort) {
  cohort <- coerce_to_datatable(cohort)

  mapping_message("Sodium and Hemoglobin tests")

  cat("\nThis function may take a few minutes to run...\n\n")

  startwith.any <- function(x, prefix) {
    mat <- matrix(0, nrow = length(x), ncol = length(prefix))
    for (i in 1:length(prefix)) {
      mat[, i] <- startsWith(x, prefix[i])
    }
    return(as.vector(apply(mat, MARGIN = 1, FUN = sum)) > 0)
  }


  temp_d_glist <- cohort$genc_id

  ### query lab table:
  DBI::dbSendQuery(dbcon, "Drop table if exists temp_data;")
  DBI::dbWriteTable(dbcon, c("pg_temp", "temp_data"),
    cohort[, .(genc_id)],
    row.names = FALSE,
    overwrite = TRUE
  )

  # find table name for lab table
  lab_table <- find_db_tablename(dbcon, "lab", verbose = FALSE)

  lab <- DBI::dbGetQuery(
    conn = dbcon,
    paste0(
      "select genc_id, result_value, test_type_mapped_omop from ",
      lab_table,
      " where test_type_mapped_omop in ('3019550', '3000963')
      and genc_id in (select genc_id from temp_data);"
    )
  ) %>%
    as.data.table()


  lab <-
    lab[, result_value := .(trimws(result_value))] %>%
    .[!is.na(as.numeric(result_value)) | startwith.any(result_value, c("<", ">"))] %>%
    .[, .(n_routine_bloodwork_derived = .N), .(genc_id)]

  ## merge with provided admission list
  res <-
    merge(cohort[, .(genc_id)],
      lab,
      by.x = "genc_id",
      by.y = "genc_id",
      all.x = TRUE
    ) %>%
    dplyr::mutate_at(
      vars(n_routine_bloodwork_derived),
      ~ coalesce(., 0)
    )

  return(res)
}
