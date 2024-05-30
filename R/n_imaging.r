#' @title
#' Compute the number of radiology tests per encounter
#'
#' @description
#' `n_imaging` returns the number of radiology tests for hospital admission.
#'
#' @details
#' This function takes a list of admissions and a GEMINI database connection to
#' generate numeric fields counting the number of different radiology tests for
#' each admission.
#'
#' Imaging table in the database should include field that classifies each test
#' into below 7 categories.
#'
#' \itemize{
#'  \item{1 : }{X-Ray}
#'  \item{2 : }{CT}
#'  \item{3 : }{MRI}
#'  \item{4 : }{Ultrasound}
#'  \item{5 : }{Other}
#'  \item{6 : }{Interventional radiology procedures}
#'  \item{7 : }{Echo (Excluded)}
#' }
#'
#' Therefore, this function should be run after some standardization efforts on
#' imaging table (performed by GEMINI team). Currently, the number of advanced
#' radiology tests is one of the performance metrics in [MyPracticeReport](
#' https://www.hqontario.ca/Quality-Improvement/Practice-Reports/MyPractice-General-Medicine). CT,
#' MRI and Ultrasound are considered advance imaging tests, which can be
#' retrieved by the derived variable (`n_img_ct_mri_us_derived`).
#'
#' @section Warning:
#' Function returns `data.table` with id field and several numeric fields. By
#' design, function will not return any NA values. When one tries to left-join
#' the output of this function with another table (another list of admissions in
#' the left), make sure list of admissions (or patient) aligns in both tables.
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
#' Whether to exclude tests in emergency department. When set to `TRUE`, tests performed
#' in ED (testing time before admission time) will not be counted. When set to `FALSE`,
#' tests will not be filtered by time and all tests in radiology table will be counted.
#' Please be aware that this may include tests before triage time, tests after discharge
#' time, and tests without testing time.
#'
#' Tests in ED are defined as `ordered_date_time` earlier than `admission_date_time`.
#' When `ordered_date_time` is not available, `performed_date_time` is used instead.
#'
#' @return
#' data.table with the same number of rows as input `cohort`, with additional
#' derived numeric fields labelled as "n_img_xray_derived",  "n_img_ct_derived",
#' "n_img_mri_derived",  "n_img_us_derived", "n_img_other_derived",
#' "n_img_int_derived" and "n_img_ct_mri_us_derived".
#'
#' @note
#' Currently, the function does not take radiology data coverage into account. For
#' patients without imaging tests, the function will return 0 in result columns.
#' User should check radiology data coverage and decide whether the imputed
#' `0`s are appropriate or not.
#'
#' @export
n_imaging <- function(dbcon,
                      cohort,
                      exclude_ed = FALSE) {
  # warning messages
  mapping_message("imaging modalities")
  coverage_message("radiology")

  # check input type and column name
  check_input(cohort, argtype = c("data.table", "data.frame"), colnames =  c("genc_id"))
  check_input(exclude_ed, argtype = "logical")
  cohort <- coerce_to_datatable(cohort)

  # identify tables in db, currently the function does not work on radiology so
  # it is hard coded as "radiology" in query
  admdad_table <- find_db_tablename(dbcon, "admdad", verbose = FALSE)
  radiology_table <- find_db_tablename(dbcon, "radiology", verbose = FALSE)

  # query db to pull imaging data
  imaging <- dbGetQuery(
    dbcon,
    paste0(
      ifelse(exclude_ed == TRUE,
            # filter by admission date time and exclude tests before admission
            paste("with temp as (
              select r.*,a.admission_date_time,
              case when r.ordered_date_time is null or r.ordered_date_time = '' or
              r.ordered_date_time = ' ' then r.performed_date_time >= a.admission_date_time
              else r.ordered_date_time >= a.admission_date_time end as case_result
              from", radiology_table, "r
              left join", admdad_table, "a on r.genc_id = a.genc_id
            ) select *
            from temp
            where case_result = 'true' and genc_id IN ("),

            # not filter by admission date time
            paste("select * from", radiology_table,  "r
            where genc_id IN (")
      ),
      # IN method is used instead of temp table method to pull based on genc_id list,
      # to ensure function works in HPC environment
      paste(cohort$genc_id, collapse = ", "), ")"
    )
    ) %>% as.data.table()

  # compute imaging number
  output_vars_names <- c(
    "n_img_xray_derived",
    "n_img_ct_derived",
    "n_img_mri_derived",
    "n_img_us_derived",
    "n_img_other_derived",
    "n_img_int_derived",
    "n_img_ct_mri_us_derived"
  )

  mapped_values <- c(
    "xray",
    "ct",
    "mri",
    "ultrasound",
    "other",
    "interventional"
  )

  # compute number of tests and format variable names
  imaging <-
    imaging %>%
    dplyr::mutate(across(modality_mapped, ~ na_if(.x, ""))) %>% # catch cases where "" is present in the dataset
    mutate(modality_mapped = ifelse(tolower(modality_mapped) %in% c("x-ray", "xray"), "xray", modality_mapped)) %>% # fix x-ray name
    dcast(., genc_id ~ modality_mapped, length, fill = 0) %>%
    janitor::clean_names() %>% # spread
    setnames(old = mapped_values, new = output_vars_names[1:6], skip_absent = TRUE) %>%
    .[, n_img_ct_mri_us_derived := rowSums(.SD),
      .SDcols = c("n_img_ct_derived", "n_img_mri_derived", "n_img_us_derived")
    ] %>%
    dplyr::select(any_of(c("genc_id", output_vars_names)))

  # merge with input cohort
  res <-
    merge(cohort[, .(genc_id)], imaging, by = "genc_id", all.x = TRUE) %>%
    dplyr::mutate_all(~ coalesce(., 0))

  return(res)
}
