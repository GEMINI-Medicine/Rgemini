#' @title
#' Compute the number of radiology tests
#'
#' @description
#' `n_imaging` returns the number of radiology tests for hospital admission.
#'
#' @details
#' This function takes a list of admissions and DRM table equivalent of imaging table to generate
#' numeric fields counting the number of different radiology tests for each admission.
#'
#' Imaging table in the database should include field that classifies each test into below 7 categories.
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
#' Therefore, this function should be run after some standardization efforts on imaging table.
#' Currently, the number of advanced radiology tests is one of the performance metrics in MyPracticeReport.
#' CT, MRI and Ultrasound are considered for this purpose and can be retrieved by one of the resulting fields (n_img_ct_mri_us_derived).
#'
#' Function automatically removes tests without valid performed date time.
#' Any tests being performed in both ED and in-patient settings will be counted.
#'
#' @section Warning:
#' Function returns data.table with id field and several numeric fields. By design, function will not return any NA values.
#' When one tries to left-join the output of this function with another table (another list of admissions in the left),
#' make sure list of admissions (or patient) aligns in both tables
#'
#' @section Note:
#' Function takes required fields labels as input, this was intended not to hardcode variable names
#' within the function. When field labels change in database, there is no need to go through the lines to change
#' the hardcoded variable names.
#'
#' @param ipadmdad a data.table object equivalent of DRM table "ipadmdad". Table must contain admission id
#' @param imaging a data.table object equivalent of DRM table "imaging". Table must contain three fields,
#' admission id, imaging performed date time and a field that standardizes types of imaging test
#' @param dtvar2 a character string of label equivalent to "imaging performed date time" in "imaging" table.
#' Its value must be in "yyyy-mm-dd hh:mm" format
#' @param mapvar2 a character string of label that standardizes types of imaging tests
#'
#' @return
#' data.table with the same number of rows with input "ipadmdad" with addtional derived numeric fields labelled as
#' "n_img_xray_derived",  "n_img_ct_derived", "n_img_mri_derived",  "n_img_us_derived", "n_img_other_derived", "n_img_int_derived" and
#' "n_img_ct_mri_us_derived".
#'
#' @export
n_imaging <- function(ipadmdad,
                              imaging,
                              dtvar2 = "performed_date_time",
                              mapvar2 = "modality_mapped") {

  ## remap variable names in case field names change in the database
  ipadmdad <- coerce_to_datatable(ipadmdad)
  imaging <- coerce_to_datatable(imaging)

  res <- ipadmdad[, .(genc_id)]

  imaging <- imaging[, .(genc_id,
                         dtvar2 = get(dtvar2),
                         mapvar2 = get(mapvar2))]

  ## compute imaging number
  output_vars_names <- c("n_img_xray_derived",
                         "n_img_ct_derived",
                         "n_img_mri_derived",
                         "n_img_us_derived",
                         "n_img_other_derived",
                         "n_img_int_derived",
                         "n_img_ct_mri_us_derived")

  mapped_values <- c("xray",
                     "ct",
                     "mri",
                     "ultrasound",
                     "other",
                     "interventional")

  imaging <-
    imaging %>%
    dplyr::mutate(across(where(is.character), ~na_if(.x, ""))) %>% # catch cases where "" is present in the dataset
    .[, mapvar2 := ifelse(tolower(mapvar2) %in% c("x-ray", "xray"), "xray", mapvar2)] %>% #fix x-ray name
    dcast(., genc_id ~ mapvar2, length, fill = 0) %>%
    janitor::clean_names() %>% # spread
    setnames(old = mapped_values, new = output_vars_names[1:6], skip_absent = TRUE) %>%
    .[, n_img_ct_mri_us_derived := rowSums(.SD),
      .SDcols = names(imaging) %in% c("n_img_ct_derived", "n_img_mri_derived", "n_img_us_derived")] %>%
    dplyr::select(any_of(c("genc_id", output_vars_names)))

  ## merge with provided admission list
  res <-
    merge(res, imaging, by = "genc_id", all.x = TRUE) %>%
    dplyr::mutate_all(~ coalesce(., 0))

  return(res)
}
