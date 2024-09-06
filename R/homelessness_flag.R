#' @title
#' Homelessness Flag
#'
#' @description
#' `homelessness_flag` returns homelessness status for each
#' `genc_id` based on ICD-10-CA diagnosis codes.
#'
#' Returns a table with a flag identifying encounters with an ICD-10-CA diagnosis code Z59.0 or Z59.1.
#'
#' Homelessness (Z59.0) is coded when an individual
#' is determined to be homeless. Effective of the year 2018-2019, CIHI
#' mandated that Z59.0 is to be coded when a "patient's record
#' shows that they are homeless upon admission". After the mandate,
#' prevalence of the flag increased by 84% compared to the previous year,
#' suggesting that ICD-10-CA diagnosis codes underestimate true homelessness rates 
#` prior to the reporting mandate. (For details, see
#' [2018 CIHI mandate to code homelessness](https://www.cihi.ca/en/better-quality-hospital-data-for-identifying-patients-experiencing-homelessness))

#' Inadequate housing (Z59.1) is coded when an individual is
#' determined to be experiencing poor housing conditions, their
#' usual housing is uninhabitable, or if they are experiencing
#' a lack of utilities. Some examples include unsafe living
#' conditions, safety issues such as a lack of heating, and
#' if the home isn't safely inhabitable due to repairs in progress.
#'
#' @details
#' Below are the current ICD-10-CA codes related to homelessness.
#' For more information, please refer to the references in this page.
#'
#' \itemize{
#'  \item{Z59.0: }{For confirmed homelessness assign Z59.0.}
#'  \item{Z59.1: }{For confirmed inadequate housing due to factors relating
#'  to safety or accessibility, but not necessarily a complete lack
#'  of housing assign Z59.1.}
#' }
#'
#' @section Notes:
#' The accuracy of this function relies on hospitals maintaining codings
#' Z59.0 for homelessness and Z59.1 for inadequate housing. These flags
#' are validated, adapted flags of ICES validated homelessness indicators.
#'
#' For data since 2018, previous studies have
#' shown that "ICD-10-CA codes have a sensitivity of 60-70% for
#' detecting whether a patient has experienced homelessness at the
#' time of hospitalization" (Richard et al, 2024). Due to this,
#' we recommend to only use ICD-10-CA codes to identify
#' homelessness after 2018.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#'
#' @param ipdiag (`data.table`)
#' `ipdiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' This table must contain `genc_id` and `diagnosis_code` (as ICD-10-CA
#' alphanumeric code) in long format.
#'
#' @param erdiag (`data.table`)
#' `erdiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' This table must contain `genc_id` and `er_diagnosis_code` (as ICD-10-CA
#' alphanumeric code) in long format.
#'
#' Typically, ER diagnoses should be included when deriving the
#' homelessness flags in order to increase sensitivity. However, in
#' certain scenarios, users may choose to only include IP diagnoses by
#' specifying `erdiag = NULL`. This may be useful when comparing cohorts
#' with different rates of ER admissions.
#'
#' @return
#' data.table with the same number of rows as input `cohort` with an
#' additional derived boolean field labelled as `"homelessness_icd__flag"`.
#' Possible values are `TRUE`, `FALSE` or `NA`. `NA` indicates that an
#' encounter does not have a diagnosis code in the diagnosis table input.
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "report_db_v3",
#'   host = "prime.smh.gemini-hpc.ca",
#'   port = 5432,
#'   user = getPass("Enter user:"),
#'   password = getPass("password")
#' )
#' ipadm <- dbGetQuery(dbcon, "select * from admdad") %>% data.table()
#' ipdiagnosis <- dbGetQuery(dbcon, "select * from ipdiagnosis") %>% data.table()
#' erdiagnosis <- dbGetQuery(dbcon, "select * from erdiagnosis") %>% data.table()
#' homeless <- homelessness_flag(cohort = ipadm, ipdiag = ipdiagnosis)
#' # view only genc_id's with homelessness flag
#' homeless <- homeless %>% filter(homeless$homelessness_icd_flag == TRUE)
#' }
#'
#' @references
#' \itemize{
#'    \item{[ALC Diagnosis List](https://www.cihi.ca/sites/default/files/document/alternate-level-care-diagnosis-list-job-aid-en.pdf)}
#'    \item{Identification of homelessness using health administrative data in Ontario: Richard Lucie, et al. J. Clin. Epidimiol., 2024. https://doi.org/10.1016/j.jclinepi.2024.111430}
#' }
#'@export
homelessness_flag <- function(
    cohort,
    ipdiag,
    erdiag = NULL) {
  ############# CHECK & PREPARE DATA #############
  if (is.null(erdiag)) {
    cat("\n*** Based on the input you provided, only in-patient diagnoses (ipdiag) will be included in the derived homelessness flag.
        If you want to include ER diagnoses, please provide the correspondig table as an input to `erdiag`. ***\n")
  }

  ## check that cohort contains genc_ids
  check_input(cohort, c("data.table", "data.frame"), colnames = c("genc_id"))

  ## check that ipdiag/erdiag contains genc_id & diagnosis_code
  check_input(ipdiag, c("data.table", "data.frame"),
    colnames = c("genc_id", "diagnosis_code"),
    coltypes = c("", "character")
  )
  if (!is.null(erdiag)) {
    check_input(erdiag, c("data.table", "data.frame"),
      colnames = c("genc_id", "er_diagnosis_code"),
      coltypes = c("", "character")
    )
  }
  ## Ensure ipdiag/erdiag are in data.table format before proceeding
  diagnoses <- coerce_to_datatable(ipdiag[, c("genc_id", "diagnosis_code")])
  if (!is.null(erdiag)) {
    erdiag <- coerce_to_datatable(erdiag[, c("genc_id", "er_diagnosis_code")])
    setnames(erdiag, "er_diagnosis_code", "diagnosis_code")
    # combine all diagnoses
    diagnoses <- rbind(diagnoses, erdiag)
  }


  ## prepare output table, should have single row per genc_id
  res <- cohort %>%
    select(genc_id) %>%
    distinct() %>%
    data.table()

  ## add flags
  res[, homelessness_icd_flag := ifelse(!genc_id %in% diagnoses$genc_id, NA, # if no diagnosis code at all for genc_id, set flag to NA
    ifelse(genc_id %in% diagnoses[grepl("Z591|Z590", diagnosis_code, ignore.case = TRUE)]$genc_id, TRUE, FALSE) # if a diagnosis code is present
  )]
  return(res)
}
