#' @title
#' Identify encounters with disability
#'
#' @description
#' This function identifies whether or not an encounter had any ICD-10-CA
#' diagnosis code(s) indicating a physical, sensory, or intellectual/
#' developmental disability as defined by
#' [Brown et al., 2021](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2776018).
#'
#' Broadly, this includes any diagnosis codes for conditions that are likely to
#' result in functional limitations and are considered to be chronic (see
#' Supplemental eTable 2 in [Brown et al., 2021](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2776018)
#' for a full list of diagnosis codes). However, note that this function only
#' searches for relevant diagnosis codes at the encounter level and does not
#' check whether chronic conditions are coded consistently across different
#' encounters for a given patient.
#'
#' In addition to returning a global disability flag indicating whether a
#' `genc_id` was diagnosed with *any* disability, users may choose to return
#' individual flags for different disability categories (e.g., physical/sensory/
#' developmental disabilities) by using `component_wise = TRUE`.
#'
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
#' Typically, ER diagnoses should be included when deriving disability in order
#' to increase sensitivity. However, in certain scenarios, users may choose to
#' only include IP diagnoses by specifying `erdiag = NULL`. This may be useful
#' when comparing cohorts with different rates of ER admissions. Additionally,
#' please note that the reliability of ER diagnosis codes has been shown to be
#' lower compared to in-patient diagnoses. Thus, users may want to
#' exclude ER diagnoses in certain situations.
#'
#' @param component_wise (`logical`)
#' If `FALSE`, only a single (global) disability flag will be returned
#' indicating whether `genc_id` was diagnosed with *any* disability.
#' If `TRUE`, 7 additional columns will be returned with an individual flag for
#' each of the following disability categories:
#' - Physical disability: Congenital Anomalies
#' - Physical disability: Musculoskeletal disorders
#' - Physical disability: Neurological disorders
#' - Physical disability: Permanent Injuries
#' - Sensory disabilities: Hearing impairments
#' - Sensory disabilities: Vision impairments
#' - Developmental Disabilities
#'
#'
#' @section Warning:
#' This function does not differentiate between diagnosis types. That is, the
#' disability flags are derived based on all diagnosis codes that are provided
#' as input to this function. By default, users should include all diagnosis
#' types to identify disabilities. However, if users wish to include only
#' certain diagnosis types (e.g., type-M for most responsible discharge
#' diagnosis), the `ipdiag` and `erdiag` input tables should be filtered
#' accordingly based on `diagnosis_type` *prior to running this function* (for
#' more details, see [CIHI diagnosis type definitions](https://www.cihi.ca/sites/default/files/document/diagnosis-type-definitions-en.pdf).
#'
#' @return `data.table`
#' This function returns a table with all encounters identified by the `cohort`
#' table input and an additional derived field `disability` (`logical`)
#' indicating whether any diagnosis code for a disability was identified.
#' If a `genc_id` does not have any entry in the diagnosis table at all,
#' `disability = NA`. Note that this is very rare: If no additional filtering
#' was performed, >99.9% of `genc_ids` should have an entry in the `ipdiagnosis`
#' (and >99.9% of `genc_ids` that were admitted via ER should have an entry in
#' the `erdiagnosis` table).
#' If `component_wise = TRUE`, 7 additional columns will be returned containing
#' flags for the individual physicial/sensory/developmental disability
#' categories.
#'
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass("Enter user:"),
#'   password = getPass("password")
#' )
#'
#' ipadmdad <- dbGetQuery(dbcon, "select * from admdad") %>% data.table()
#' ipdiagnosis <- dbGetQuery(dbcon, "select * from ipdiagnosis") %>% data.table()
#' erdiagnosis <- dbGetQuery(db, "select * from erdiagnosis") %>% data.table()
#'
#' # including ER diagnosis codes
#' disability(cohort = ipadmdad, ipdiag = ipdiagnosis, erdiag = erdiagnosis)
#'
#' # not including ER diagnosis codes
#' disability(cohort = ipadmdad, ipdiag = ipdiagnosis, erdiag = NULL)
#'
#' # returning component-wise disability flags
#' disability(ipadmdad, ipdiagnosis, erdiagnosis, component_wise = TRUE)
#' }
#'
#' @references
#' Brown, H. K., Ray, J. G., Chen, S., Guttmann, A., Havercamp, S. M., Parish,
#' S., ... & Lunsky, Y. (2021). Association of preexisting disability with
#' severe maternal morbidity or mortality in Ontario, Canada. JAMA Network Open,
#' 4(2), e2034993-e2034993.
#'
#' @importFrom stringr str_detect
#' @export
#'
disability <- function(cohort, ipdiag, erdiag, component_wise = FALSE) {

  ############# CHECK & PREPARE DATA #############
  if (is.null(erdiag)) {
    cat("\n*** Based on the input you provided, only in-patient diagnoses (ipdiag) will be included in the calculation of the disability flag.
    If you want to include ER diagnoses, please provide the correspondig table as an input to `erdiag`. ***\n")
  }

  ## check that cohort contains genc_ids -> add these once check_input function from branch #26 has been reviewed
  # check_input(cohort, c("data.table", "data.frame"), colnames = c("genc_id"))

  ## check that ipdiag/erdiag contains genc_id & diagnosis_code
  # check_input(ipdiag, c("data.table", "data.frame"),
  #             colnames = c("genc_id", "diagnosis_code"),
  #             coltypes = c("", "character"))
  # if (!is.null(erdiag)){
  #  check_input(erdiag, c("data.table", "data.frame"),
  #              colnames = c("genc_id", "er_diagnosis_code"),
  #              coltypes = c("", "character"))
  # }

  ## Prepare output - should have 1 row per genc_id in cohort file
  res <- cohort %>%
    select(genc_id) %>%
    distinct() %>%
    data.table()

  ## Ensure ipdiag/erdiag are in data.table format before proceeding
  diagnoses <- coerce_to_datatable(ipdiag[, c("genc_id", "diagnosis_code")])
  if (!is.null(erdiag)) {
    erdiag <- coerce_to_datatable(erdiag[, c("genc_id", "er_diagnosis_code")])
    setnames(erdiag, "er_diagnosis_code", "diagnosis_code")
    # combine all diagnoses
    diagnoses <- rbind(diagnoses, erdiag)
  }

  ## Read file with disability codes from data folder
  data(mapping_disability, package = "Rgemini")
  disability_codes <- mapping_disability %>% data.table()

  ## Derive flag for any disability
  res[, disability := ifelse(!genc_id %in% diagnoses$genc_id, NA, # if no diagnosis code at all for genc_id, set flag to NA
    ifelse(genc_id %in% diagnoses[ # if entry in mapped diagnosis codes, set disability to TRUE, otherwise: FALSE
      str_detect(diagnoses$diagnosis_code, paste0("^", disability_codes$ICD_10_CA_code, collapse = "|")),
    ]$genc_id, TRUE, FALSE)
  )]

  ## Derive individual flags for each disability category
  if (component_wise == TRUE) {
    disability_categories <- unique(disability_codes$Category)

    sapply(disability_categories, function(disability_cat) {
      res[, paste0(disability_cat) := ifelse(!genc_id %in% diagnoses$genc_id, NA, # if no diagnosis code at all for genc_id, set flag to NA
        ifelse(genc_id %in% diagnoses[ # if entry in mapped diagnosis codes, set disability to TRUE, otherwise: FALSE
          str_detect(diagnoses$diagnosis_code, paste0("^", disability_codes[Category == disability_cat, ]$ICD_10_CA_code, collapse = "|")),
        ]$genc_id, TRUE, FALSE)
      )]
    })
  }

  return(res)
}
