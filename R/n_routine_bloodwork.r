#' @title
#' Compute the number of routine bloodwork tests for a admission
#'
#' @description
#' `n_routine_bloodwork` returns the number of routine bloodwork (Sodium and Hemoglobin tests) for hospital admission.
#'
#' @details
#' This function takes a list of admissions and DRM table equivalent of lab table to generate
#' numeric fields counting the number of Sodium and Hemoglobin tests for each admission.
#'
#' Lab table in the database should include field that classifies each test into a few different categories.
#' Therefore, this function should be run after some standardization efforts on lab table.
#' Currently, the number of routine bloodwork tests is one of the performance metrics in MyPracticeReport.
#'
#' Function automatically removes tests without valid numeric result value. Exceptions are boundary result value (e.g. ">120" mmol/L), and these
#' are still counted as valid tests.
#'
#' Any tests being performed in both ED and in-patient settings will be counted.
#'
#' @section Warning:
#' Function returns data.table with id field and one numeric field. By design, function will not return any NA values.
#' If id in the "ipadmdad" is not captured in "lab" table, admission gets assigned 0 number of tests.
#' When one tries to left-join the output of this function with another table (another list of admissions in the left),
#' make sure list of admissions (or patient) aligns in both tables
#'
#' @section Note:
#' Function takes required fields labels as input, this was intended not to hardcode variable names
#' within the function. When field labels change in database, there is no need to go through the lines to change
#' the hardcoded variable names.
#'
#' @param ipadmdad a data.table object equivalent of DRM table "ipadmdad". Table must contain admission id
#' @param lab a data.table object equivalent of DRM table "lab". Table must contain three fields,
#' admission id, test result value and a field that standardizes types of lab tests
#' @param idvar1 a character string of label for admission id in "ipadmdad" table
#' @param idvar2 a character string of label for admission id in "lab" table
#' @param valvar2 a character string of label equivalent to "test result value" in "lab" table
#' @param mapvar2 a character string of label that standardizes types of lab tests
#'
#' @return
#' data.table with the same number of rows with input "ipadmdad" with addtional derived numeric field labelled as
#' "n_routine_bloodwork_derived"
#'
#' @export
n_routine_bloodwork <- function(ipadmdad,
                                        lab,
                                        idvar1 = "genc_id",
                                        idvar2 = "genc_id",
                                        valvar2 = "result_value",
                                        mapvar2 = "test_type_mapped"){

  ## remap variable names in case field names change in the database
  res <- ipadmdad[, .(idvar1 = get(idvar1))]

  lab <- lab[, .(idvar2 = get(idvar2),
                 valvar2 = get(valvar2),
                 mapvar2 = get(mapvar2))]


  ## compute sodium and hemoglobin test
  routine_tests <- c("Sodium", "Hemoglobin")

  lab <-
    lab %>%
    dplyr::filter(mapvar2 %in% routine_tests) %>%
    dplyr::mutate_all(na_if, "") %>% # catch cases where "" is present in the dataset
    dplyr::mutate(valvar2 = trimws(as.character(valvar2))) %>%
    .[!is.na(as.numeric(valvar2)) | startwith.any(valvar2, c("<", ">"))] %>%
    .[, .(n_routine_bloodwork_derived = .N), .(idvar2)]

  ## merge with provided admission list
  res <-
    merge(res, lab, by.x = "idvar1", by.y = "idvar2", all.x = T) %>%
    dplyr::mutate_all(~ coalesce(., 0))

  ## set back to initial id variable label
  data.table::setnames(res,
                       old = "idvar1",
                       new = idvar1)
  return(res)
}
