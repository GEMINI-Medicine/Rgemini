#' @title
#' Compute the number of radiology tests per encounter
#'
#' @description
#' `n_imaging` returns the number of radiology tests for hospital admission.
#'
#' @details
#' This function takes a list of admissions and DRM table equivalent of imaging
#' table to generate numeric fields counting the number of different radiology
#' tests for each admission.
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
#' radiology tests is one of the performance metrics in MyPracticeReport. CT,
#' MRI and Ultrasound are considered advance imaging tests, which can be
#' retrieved by the derived variable (`n_img_ct_mri_us_derived`).
#'
#' Any tests being performed in both ED and in-patient settings will be counted.
#'
#' @section Warning:
#' Function returns data.table with id field and several numeric fields. By
#' design, function will not return any NA values. When one tries to left-join
#' the output of this function with another table (another list of admissions in
#' the left), make sure list of admissions (or patient) aligns in both tables.
#'
#' @section Note:
#' The function takes optional inputs specifying the field label for the
#' variables corresponding to date-time of the performed imaging test and
#' mapped imaging modality, respectively. This is to avoid hardcoding variable
#' names within the function in case field labels differ between databases.
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
#' Whether to exclude tests in emergency department. When set to `TRUE`, only
#' tests being performed in inpatient settings are counted. When set to `FALSE`,
#' tests in both ED and in-patient settings will be counted. To distinguish
#' tests in ED and in-patient settings, following
#' the methodology implemented in
#' [My Practice Report](https://www.hqontario.ca/Portals/0/documents/qi/practice-reports/general-medicine-sample-report.html#imaging-qi),
#' `ordered_date_time` is compared against `admission_date_time` to identify
#' imaging tests in ED. When `ordered_date_time` is not available,
#' `performed_date_time` is used instead. If both `ordered_date_time` and
#' `performed_date_time` are not available, the test will not be counted in the
#'  output.
#'
#' @return
#' data.table with the same number of rows as input "cohort", with additional
#' derived numeric fields labelled as "n_img_xray_derived",  "n_img_ct_derived",
#' "n_img_mri_derived",  "n_img_us_derived", "n_img_other_derived",
#' "n_img_int_derived" and "n_img_ct_mri_us_derived".
#'
#' @note
#' Currently, the function does not take data availability into account. For
#' patients without imaging tests, the function will return 0 in result columns.
#' User should check radiology data availability and decide whether the imputed
#' `0`s are appropriate or not.
#'
#' @export
n_imaging <- function(dbcon,
                      cohort,
                      exclude_ed = FALSE) {

  mapping_message("imaging modalities")

  # warning to remind user of availability check
  cat(
    "\n***Note:***
    This function does not check radiology data availability for the input cohort,
    and returns 0 when test is not found in rad table. These 0s may not be
    appropriate especially for patients where radiology data is not available.
    User should check the coverage of radiology table and modify the
    result if necessary.\n")

  # check input type and column name
  check_input(cohort, argtype = c("data.table", "data.frame"), colnames =  c("genc_id"))
  check_input(exclude_ed, arginput = "logical")
  cohort <- coerce_to_datatable(cohort)

  # identify tables in db, currently the function does not work on radiology so
  # it is hard coded as "radiology" in query
  admdad_table <- find_db_tablename(dbcon, "admdad", verbose = FALSE)

  # query db to pull imaging data
  imaging <- dbGetQuery(
    dbcon,
    paste0(
      ifelse(exclude_ed == TRUE,
            # filter by admission date time and exclude tests before admission
            paste0("with temp as (
              select r.*,a.admission_date_time,
              case when r.ordered_date_time is null or r.ordered_date_time = '' or
              r.ordered_date_time = ' ' then r.performed_date_time >= a.admission_date_time
              else r.ordered_date_time >= a.admission_date_time end as case_result
              from radiology r
              left join ", admdad_table, " a on r.genc_id = a.genc_id
            ) select *
            from temp
            where case_result = 'true' and genc_id IN ("),

            # not filter by admission date time
            paste0("select * from radiology r
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
