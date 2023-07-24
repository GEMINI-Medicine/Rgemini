#' @title
#' Compute in-hospital mortality using CIHI DAD
#'
#' @description
#' `in_hospital_mortality` returns whether patient has deceased
#' in hospital based on the CIHI DAD field
#'
#' @details
#' This function takes the CIHI DAD "discharge disposition"
#' field (Group 05, Field 05 As of 19/20 CIHI DAD) and generate
#' boolean field indicating whether the patient has deceased during hospital stay.
#'
#' The current version of the function captures the four possible
#' values indicating patient has deceased in hospital:
#'
#' \itemize{
#'  \item{7 :  }{Died (Deprecated post 2017)}
#'  \item{72 : }{Died in Facility}
#'  \item{73 : }{Medical Assistance in Dying}
#'  \item{74 : }{Suicide in Facility (Excluded in calculation by default; can be included in argument)}
#' }
#'
#' @section Warning:
#' NA values in returned data.table indicates missing
#' discharge disposition field values.
#'
#' @param ipadmdad a data.table object equivalent of DRM table "ipadmdad".
#' Table must contain two fields:
#' admission id and discharge disposition
#' @param id a character string of label for admission id
#' in "ipadmdad" table
#' @param dischvar a character string of label equivalent to
#' CIHI-defined "discharge disposition".
#' @param suicide a TRUE/FALSE argument indicating whether suicides in facility
#' should be factored in the calculation.
#'
#' @return
#' data.table with the same number of rows with input "ipadmdad" with additional
#' derived boolean field labelled as
#' "in_hospital_mortality_derived". Possible values are TRUE, FALSE or NA
#'
#' @export
in_hospital_mortality <- function(ipadmdad, suicide = FALSE) {

  # convert data input into data.table format
  ipadmdad <- as.data.table(ipadmdad)

  ## remap variable names in case field names change in the database
  res <- ipadmdad[, .(
    id = genc_id,
    dischvar = discharge_disposition
  )]

  ## subset to the fields that's used for calculation
  res <- res[, .(id,
                 dischvar = trimws(as.character(dischvar))
  )]


  if (suicide == FALSE) {
    ## for suicide = FALSE
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
