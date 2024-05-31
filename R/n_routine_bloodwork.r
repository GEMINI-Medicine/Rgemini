#' @title
#' Compute the number of routine bloodwork tests per encounter
#'
#' @description
#' `n_routine_bloodwork` returns the number of routine bloodwork (Sodium and
#' Hemoglobin tests) for each hospital admission. Sodium and Hemoglobin tests
#' are defined by OMOP codes. Sodium is the code 3019550. Hemoglobin is 3000963.
#'
#' @details
#' This function takes a list of admissions and an GEMINI databse connection to
#' generate numeric fields counting the number of Sodium and Hemoglobin tests
#' for each admission.
#'
#' Lab table in the database should include field that classifies each test into
#' a few different categories. Therefore, this function should be run after some
#' standardization efforts on lab table (performed by GEMINI). Currently, the
#' number of routine bloodwork tests is one of the performance metrics in
#' [MyPracticeReport](https://www.hqontario.ca/Quality-Improvement/Practice-Reports/MyPractice-General-Medicine).
#'
#' Function automatically removes tests without valid numeric result value.
#' Exceptions are boundary result value (e.g. ">120" mmol/L), and these
#' are still counted as valid tests.
#'
#' @section Warning:
#' Function returns data.table with id field `genc_id` and one numeric field
#' indicating the number of bloodwork tests per encounter. By design,
#' function will not return any NA values. If a `genc_id` does not have any
#' entries in the "lab" table, the admission gets assigned 0 number of tests.
#' User should check lab data coverage and decide whether the imputed `0`s are
#' appropriate or not.
#' When one tries to left-join the output of this function with another table
#' (another list of admissions in the left), make sure list of admissions (or
#' patient) aligns in both tables.
#'
#' This function requires mappings by a Subject Matter Expert to ensure that all
#' tests are mapped to
#' 3019550 - Sodium (Moles/volume) in Serum or Plasma, and
#' 3000963 - Hemoglobin (Mass/volume) in Blood
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database. `DBI` connection is recommended
#' as `odbc` connection may cause connection issues in certain environment.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#'
#' @param exclude_ed (`logical`)
#' Whether to exclude tests in emergency department. When set to `TRUE`, tests
#' performed in ED settings (testing time before admission time) will not be
#' counted. When set to `FALSE`, tests will not be filtered by time and all tests
#' in lab table will be counted. Please be aware that this may include tests before
#' triage time, tests after discharge time, and tests without testing time.
#' Tests in ED are defined as `collection_date_time` earlier than
#' `admission_date_time`. Tests with missing `collection_date_time` will be
#' excluded when `exclude_ed` is set to `TRUE`.
#'
#' @import RPostgreSQL dplyr
#'
#' @return
#' data.table object with the same number of rows as input "cohort", with
#' additional derived numeric field labelled as "n_routine_bloodwork_derived"
#'
#' @export
#'

n_routine_bloodwork <- function(dbcon,
                                cohort,
                                exclude_ed = FALSE) {
  # mapping warning
  mapping_message("Sodium and Hemoglobin tests")
  # warning to remind user of availability check
  coverage_message("lab")

  cat("\nThis function may take a few minutes to run...\n\n")


  # check input type and column name
  check_input(cohort, argtype = c("data.table", "data.frame"), colnames =  c("genc_id"))
  check_input(exclude_ed, argtype = "logical")
  cohort <- coerce_to_datatable(cohort)

  # find table names for all relevant tables (admdad/lab)
  admdad_table <- find_db_tablename(dbcon, "admdad", verbose = FALSE)
  lab_table <- find_db_tablename(dbcon, "lab", verbose = FALSE)

  # load lab from db
  lab <- dbGetQuery(
    dbcon,
    paste0(
      ifelse(
        exclude_ed == TRUE,
        # filter by admission date time and exclude tests before admission
        paste(
          "select l.genc_id, l.collection_date_time, l.result_value,
           l.test_type_mapped_omop, a.admission_date_time
           from", lab_table, "l
           left join", admdad_table, "a
           on l.genc_id = a.genc_id
           where l.test_type_mapped_omop in ('3000963','3019550') and
           l.collection_date_time >= a.admission_date_time and a.genc_id in ("
        ),
        # no filter on collection date time
        paste(
          "select genc_id, collection_date_time, result_value, test_type_mapped_omop
           from", lab_table,
           "where test_type_mapped_omop in ('3019550', '3000963') and genc_id in ("
        )
      ),
      # IN method is used instead of temp table method to pull based on genc_id list,
      # to ensure function works in HPC environment
      paste(cohort$genc_id, collapse = ", "), ")"
    )
  ) %>% as.data.table

  # only count tests with a valid
  # to be consistent with MyPracticeReport definition, all results are included
  # without restricting to numeric result values.
  lab <- lab[, .(n_routine_bloodwork_derived = .N), .(genc_id)]

  # merge with provided admission list
  res <-
    merge(cohort[, .(genc_id)],
      lab,
      by = "genc_id",
      all.x = TRUE
    ) %>%
    dplyr::mutate_at(
      vars(n_routine_bloodwork_derived),
      ~ coalesce(., 0)
    )

  return(res)
}
