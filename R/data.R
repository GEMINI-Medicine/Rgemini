#' @title
#' Dummy ipdiagnosis data
#'
#' @description
#' This mimics the GEMINI ipdiagnosis table. And is generated with the `dummy_diag` function.
#' See the [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)
#' for details.
#'
#' @keywords internal
#' @name dummy_ipdiagnosis
#' @docType data
#'
NULL

#' @title
#' Dummy daily census data
#'
#' @description
#' Used to generate mock output for the `daily_census` vignette.
#'
#' @keywords internal
#' @name dummy_census
#' @docType data
#'
NULL

#' @title
#' Dummy CCSR data
#'
#' @description
#' Used to generate mock output for the `icd_to_ccsr` vignette.
#'
#' @keywords internal
#' @name dummy_ccsr
#' @docType data
#'
NULL

#' @title
#' Dummy coverage data
#'
#' @description
#' Used to generate mock output for the `data_coverage` vignette.
#'
#' @keywords internal
#' @name coverage
#' @docType data
#'
NULL

#' @title
#' Dummy coverage data (encounter-level)
#'
#' @description
#' Used to generate mock output for the `data_coverage` vignette.
#'
#' @keywords internal
#' @name coverage_enc
#' @docType data
#'
NULL

#' @title
#' Dummy coverage data (timeline)
#'
#' @description
#' Used to generate mock output for the `data_coverage` vignette.
#'
#' @keywords internal
#' @name coverage_timeline
#' @docType data
#'
NULL


#' Mapping data for `frailty_score` function
#'
#' @name mapping_cihi_frailty
#' @docType data
NULL


#' Mapping data for `disability` function
#'
#' @name mapping_disability
#' @docType data
NULL

#' Blood product lookup table
#'
#' A dataset containing common blood product codes, their raw names, and relative proportions.
#'
#' @format A data frame with 23 rows and 3 columns:
#' \describe{
#'   \item{blood_product_mapped_omop}{Mapped OMOP codes for each blood product}
#'   \item{blood_product_mapped_raw}{Raw names for each blood product}
#'   \item{prob}{Relative proportions of each product, 0-1}
#' }
#'
#' @name blood_product_lookup
#' @docType data
NULL
