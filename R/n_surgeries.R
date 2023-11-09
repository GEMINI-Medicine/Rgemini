#' @title
#' Compute the number of surgical interventions per encounter
#'
#' @description
#' `n_surgeries` returns the number of surgeries per `genc_id` based on CCI intervention codes.
#'
#' @details
#' This function takes a `cohort` and `intervention` data input and generates a
#' new numeric field corresponding to the number of surgical interventions per
#' `genc_id` in the cohort of interest.
#'
#' A list of all CCI intervention codes that are classified as surgeries can be
#' found in the [supplemental material](https://www.cmaj.ca/content/cmaj/suppl/2020/11/10/192.46.E1440.DC1/200068-res-1-at.pdf)
#' of this [paper](https://www.cmaj.ca/content/192/46/E1440/tab-related-content).
#'
#' @section Warning:
#' By design, this function will not return any `NA` values. Any `genc_id` for which no surgery entries were found in the
#' intervention table are returned with `n_surgeries = 0`. When left-joining the output of this function with another table,
#' make sure list of genc_ids aligns in both tables.
#'
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#' @param interventions (`data.frame` or `data.table`)
#' All intervention data of interest. This typically refers to the `ipintervention`
#' table containing all in-patient interventions
#' (see [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#' However, researchers may optionally choose to also include interventions performed
#' in the emergency department (`erintervention`) by merging the corresponding
#' tables outside of this function. Similarly, researchers can perform any other
#' relevant exclusions/inclusions (e.g., interventions within first 24h after admission)
#' prior to running this function.
#'
#' This table must contain the `genc_id` and `intervention_code` (CCI code) fields in long format.
#' The intervention codes must be free from any punctuation or special characters.
#'
#' @return `data.table`
#' This function returns a table with all encounters identified by the `cohort` table input and
#' additional derived numeric field `n_surgeries` indicating the number of surgical interventions per `genc_id`.
#' If no surgical intervention was found, the corresponding `genc_id` will be returned with `n_surgeries = 0`.
#'
#' @references
#' Jerath, A., Sutherland, J., Austin, P. C., Ko, D. T., Wijeysundera, H. C., Fremes, S., ... & Wijeysundera, D. N.
#' (2020). Delayed discharge after major surgical procedures in Ontario, Canada: a population-based cohort study. CMAJ, 192(46), E1440-E1452.
#'
#' @importFrom stringr str_sub
#' @export
#'
n_surgeries <- function(cohort, interventions) {

  ############# CHECK & PREPARE DATA #############
  ## check that cohort contains genc_ids

  ## check that interventions contain genc_id & intervention_code

  ## Prepare output - should have 1 row per genc_id that exists in cohort file
  res <- distinct(cohort[, 'genc_id']) %>% data.table()

  ## Ensure user inputs are in data.table format before proceeding
  interventions <- coerce_to_datatable(interventions[, c('genc_id', 'intervention_code')])

  ## Pre-process surgery codes
  # only keep first 5 characters, containing relevant level of detail to identify surgeries
  interventions[, intervention_code_sub := str_sub(intervention_code, 1, 5)]


  ############# GET SURGERY INTERVENTIONS #############
  ## Define surgery codes
  surgery_codes <- c(
    '1KA76', '1KA80', '1KA87', '1VQ93', '1VC93', '1KG76', '1KG57', '1KG87', # Vascular surgery #  [I believe "IKA80" listed in the paper is a typo? First character should be number, probably 1...]
    '1NM87', '1NM89', '1NM91', '1NQ87', '1NQ89', # General surgery
    '1GR87', '1GR89', '1GR91', '1GT87', '1GT89', '1GT91', # Thoracic Surgery
    '1PC87', '1PC89', '1PC91', # Major urology surgery
    '1VA53', '1VG53', '1SC74', '1SC75', '1SC80', '1SC87', '1SC89', '1VA74', '1VA80', '1VA87', # Orthopaedic Surgery
    '1AA52', '1AB52', '1AC87', '1AJ87', '1AK87', '1AN87', # Neurosurgery
    '1HY85', '1GT85', '1HZ85', '1OA85', '1PC85', '1OK85', # Solid organ transplant
    '1IJ76', '1IJ80', '1IJ86', '1HS80', '1HS90', '1HT80', '1HT89', '1HT90', '1HU80', '1HU90', '1HV80', '1HV90', # Cardiac surgery
    '1IA57', '1IA79', '1IA80' , '1IA86', '1IA87', '1IB57', '1IB76', '1IB80', '1IB82', '1IB87', '1HP53', '1HP78', '1HP87'
  )

  ## Usually, all codes starting with characters listed above should be included, but there are a few exceptions
  # For these ones, make sure we capture all that START WITH same characters (i.e., look past first 5 characters)
  exclude_codes <- c('^1KG57GQX|^1NM87BA|^1NQ87BA|^1AA52HA|^1OA85VCXXK') # some don't exist in DB but keep for future reference...

  ## Filter for surgical interventions & count n per genc_id
  surgeries <- interventions[
    intervention_code_sub %in% surgery_codes & !grepl(exclude_codes, intervention_code), ][
      , .N, by = "genc_id"]

  ## Combine with all genc_ids in cohort
  res <- merge(res, surgeries, by = 'genc_id', all.x = TRUE) %>%
    dplyr::mutate_all(~ coalesce(., 0)) # fill in missing values with 0 (no surgery)


  return(res)
}
