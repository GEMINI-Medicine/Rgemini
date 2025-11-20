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
#'
"blood_product_lookup"
