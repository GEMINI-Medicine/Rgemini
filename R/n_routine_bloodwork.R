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
#' Function removes tests without valid numeric result value.
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
#' A database connection to any GEMINI database. Only `DBI` connection is
#' accepted as `odbc` connection may cause connection issues in certain environment.
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
  check_input(dbcon, argtype = "DBI")
  check_input(cohort, argtype = c("data.table", "data.frame"), colnames =  c("genc_id"))
  check_input(exclude_ed, argtype = "logical")
  cohort <- coerce_to_datatable(cohort)

  # find table names for all relevant tables (admdad/lab)
  admdad_table <- find_db_tablename(dbcon, "admdad", verbose = FALSE)
  lab_table <- find_db_tablename(dbcon, "lab", verbose = FALSE)

  # speed up query by using temp table with analyze
  DBI::dbSendQuery(dbcon, "Drop table if exists cohort_data;")
  DBI::dbWriteTable(dbcon, c("pg_temp", "cohort_data"), cohort[, .(genc_id)], row.names = FALSE, overwrite = TRUE)
  DBI::dbSendQuery(dbcon, "Analyze cohort_data")

  # load lab from db
  lab <- dbGetQuery(
    dbcon,
    ifelse(
      exclude_ed == TRUE,
      # filter by admission date time and exclude tests before admission
      paste(
        "select l.genc_id, l.result_value,
           a.admission_date_time
           from", lab_table, "l
           left join", admdad_table, "a
           on l.genc_id = a.genc_id where exists (select 1 from cohort_data c where c.genc_id=a.genc_id)
           and l.test_type_mapped_omop in ('3000963', '3019550') and
           l.collection_date_time >= a.admission_date_time"
      ),
      # no filter on collection date time
      paste(
        "select l.genc_id, l.result_value
           from", lab_table, "l where exists (select 1 from cohort_data c where c.genc_id=l.genc_id)",
        "and l.test_type_mapped_omop in ('3000963', '3019550')"
      )
    )
  ) %>% as.data.table

# Convert result_value to numeric after removing special characters
lab <- lab[, result_value :=
  as.numeric(stringr::str_replace_all(
    tolower(result_value),
    "@([a-z0-9]*)|<|>|less than|greater than|;", ""))]

  # only count tests with valid numeric result values.
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
