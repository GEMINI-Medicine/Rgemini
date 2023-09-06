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

#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#' @param imaging (`data.table` or `data.frame`)\cr
#' Table equivalent to DRM table "radiology" as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Table must contain three fields: `genc_id`, imaging performed date-time (see
#' `dtvar`, in "yyyy-mm-dd hh:mm" format), and a field that standardizes the
#' imaging test type (see `mapvar`).
#' @param dtvar (`character`)
#' Name of the column in `imaging` table containing date-time of performed
#' imaging test (usually `"performed_date_time"`).
#' @param mapvar (`character`)
#' Name of the column in `imaging` table containing the imaging test type
#' (usually `"modality_mapped"`).
#'
#' @return
#' data.table with the same number of rows as input "cohort", with additional
#' derived numeric fields labelled as "n_img_xray_derived",  "n_img_ct_derived",
#' "n_img_mri_derived",  "n_img_us_derived", "n_img_other_derived",
#' "n_img_int_derived" and "n_img_ct_mri_us_derived".
#'
#' @export
n_imaging <- function(cohort,
                      imaging,
                      dtvar = "performed_date_time",
                      mapvar = "modality_mapped") {

  cat("\n***Note:***
  The output of this function is based on manual mapping of imaging modalities by a GEMINI Subject Matter Expert.
  Please carefully check mapping coverage for your cohort of interest, or contact the GEMINI team if you require additional support.\n")

  ## remap variable names in case field names change in the database
  cohort <- coerce_to_datatable(cohort)
  imaging <- coerce_to_datatable(imaging)

  res <- cohort[, .(genc_id)]

  imaging <- imaging[, .(genc_id,
    dtvar = get(dtvar),
    mapvar = get(mapvar)
  )]

  ## compute imaging number
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

  imaging <-
    imaging %>%
    dplyr::mutate(across(where(is.character), ~ na_if(.x, ""))) %>% # catch cases where "" is present in the dataset
    .[, mapvar := ifelse(tolower(mapvar) %in% c("x-ray", "xray"), "xray", mapvar)] %>% # fix x-ray name
    dcast(., genc_id ~ mapvar, length, fill = 0) %>%
    janitor::clean_names() %>% # spread
    setnames(old = mapped_values, new = output_vars_names[1:6], skip_absent = TRUE) %>%
    .[, n_img_ct_mri_us_derived := rowSums(.SD),
      .SDcols = c("n_img_ct_derived", "n_img_mri_derived", "n_img_us_derived")
    ] %>%
    dplyr::select(any_of(c("genc_id", output_vars_names)))

  ## merge with provided admission list
  res <-
    merge(res, imaging, by = "genc_id", all.x = TRUE) %>%
    dplyr::mutate_all(~ coalesce(., 0))

  return(res)
}
