#' @title
#' Compute in-hospital mortality using CIHI DAD
#'
#' @description
#' `in_hospital_mortality` returns whether a patient has deceased
#' in hospital based on the CIHI DAD field discharge disposition
#'
#' @details
#' This function takes the CIHI DAD "discharge disposition" field
#' (Group 05, Field 05 As of 19/20 CIHI DAD) and generates a boolean
#' variable indicating whether the patient has deceased during hospital stay.
#'
#' The current version of the function captures the four possible
#' values indicating that a patient has deceased in hospital:
#'
#' \itemize{
#'  \item{7 :  }{Died (Deprecated post 2017)}
#'  \item{72 : }{Died in Facility}
#'  \item{73 : }{Medical Assistance in Dying}
#'  \item{74 : }{Suicide in Facility (Excluded in calculation by default;
#'  can be included by setting `suicide` argument to `TRUE`)}
#' }
#'
#' @section Warning:
#' `NA` values in returned data.table indicate missing
#' discharge disposition field values.
#'
#' @param ipadmdad (`data.frame` or `data.table`)\cr
#' Table with all relevant encounters of interest from DRM table "ipadmdad" (see
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#' Must contain two fields: `genc_id` and `discharge_disposition`.
#'
#' @param suicide (`logical`)\cr
#' a TRUE/FALSE argument indicating whether suicides in facility
#' should be counted towards in-hospital mortality rate or not.
#'
#' @return
#' `data.table` with the same number of rows as input `ipadmdad`, with
#' additional derived boolean field labelled as `in_hospital_mortality_derived`.
#' Possible values are `TRUE`, `FALSE` or `NA`, where `NA` indicates missing
#' discharge disposition.
#'
#' @export
#'
in_hospital_mortality <- function(ipadmdad, suicide = FALSE) {
  ## convert data input into data.table format
  ipadmdad <- coerce_to_datatable(ipadmdad)

  ## subset to the fields that are used for calculation
  res <- ipadmdad[, .(genc_id,
    dischvar = trimws(as.character(discharge_disposition))
  )]


  if (suicide == FALSE) {
    ## for suicide = FALSE
    # in-hospital suicide is returned with in_hospital_mortality = FALSE
    res[, ":="(in_hospital_mortality_derived = ifelse(is.na(dischvar), NA,
      ifelse(dischvar %in% c("7", "72", "73"), TRUE,
        FALSE
      )
    ),
    dischvar = NULL)][]
    cat("
  \nNote: Suicide in facility is excluded from the calculation\n
      ")
  } else if (suicide == TRUE) {
    ## for suicide = TRUE
    # in-hospital suicide is returned with in_hospital_mortality = TRUE
    res[, ":="(in_hospital_mortality_derived = ifelse(is.na(dischvar), NA,
      ifelse(dischvar %in% c("7", "72", "73", "74"), TRUE,
        FALSE
      )
    ),
    dischvar = NULL)][]
    cat("
  \nNote: Suicide in facility is included in the calculation\n
      ")
  }

  return(res)
}
