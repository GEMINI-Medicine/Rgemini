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
#' By setting `component_wise` to `TRUE`, users can choose to return all
#' identified diagnosis codes for encounters with a disability, together with
#' the disability category each code was matched with (e.g., physical/sensory/
#' developmental disability - see below).
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#'
#' @param ipdiag (`data.table`)
#' `ipdiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#' This table must contain `genc_id` and `diagnosis_code` (as ICD-10-CA
#' alphanumeric code) in long format.
#'
#' @param erdiag (`data.table`)
#' `erdiagnosis` table as defined in the [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#' This table must contain `genc_id` and `er_diagnosis_code` (as ICD-10-CA
#' alphanumeric code) in long format.
#' Typically, ER diagnoses should be included when deriving disability in order
#' to increase sensitivity. However, in certain scenarios, users may choose to
#' only include IP diagnoses by specifying `erdiag = NULL`. This may be useful
#' when comparing cohorts with different rates of ER admissions.
#'
#' @param component_wise (`logical`)
#' If `component_wise == FALSE` (default), the function calculates a single
#' (global) disability flag indicating whether each `genc_id` was diagnosed with
#' *any* disability.
#'
#' If `component_wise == TRUE`, for each `genc_id` with a disability,
#' all identified disability diagnosis codes are returned, together with one of
#' the following 7 disability categories:
#' - Physical disability - Congenital Anomalies
#' - Physical disability - Musculoskeletal disorders
#' - Physical disability - Neurological disorders
#' - Physical disability - Permanent Injuries
#' - Sensory disabilities - Hearing impairments
#' - Sensory disabilities - Vision impairments
#' - Developmental Disabilities
#'
#' @section Notes:
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
#' If `component_wise == FALSE`, returns a table with all encounters identified
#' by the `cohort` table input and an additional derived field `disability`
#' (`logical`) indicating whether any diagnosis code for a disability was
#' identified. If a `genc_id` does not have any entry in the diagnosis table at
#' all, `disability = NA`. Note that this is very rare: If no additional
#' filtering was performed, >99.9% of `genc_ids` should have an entry in the
#' `ipdiagnosis` (and >99.9% of `genc_ids` that were admitted via ER should have
#' an entry in the `erdiagnosis` table).
#'
#' If `component_wise == TRUE`, will only return `genc_ids` with a disability.
#' The output is returned in long format, where each row corresponds to a
#' disability diagnosis (`diagnosis_code`) and its corresponding
#' `disability_category` (`character`).
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
#' erdiagnosis <- dbGetQuery(dbcon, "select * from erdiagnosis") %>% data.table()
#'
#' # including ER diagnosis codes
#' disability(cohort = ipadmdad, ipdiag = ipdiagnosis, erdiag = erdiagnosis)
#'
#' # not including ER diagnosis codes
#' disability(cohort = ipadmdad, ipdiag = ipdiagnosis, erdiag = NULL)
#'
#' # returning component-wise disability categories
#' disability(ipadmdad, ipdiagnosis, erdiagnosis, component_wise = TRUE)
#' }
#'
#' @references
#' Brown HK, et al. JAMA Netw Open, 2021. https://doi.org/10.1001/jamanetworkopen.2020.34993
#'
#' @export
disability <- function(cohort, ipdiag, erdiag, component_wise = FALSE) {
  ############# CHECK & PREPARE DATA #############
  if (is.null(erdiag)) {
    cat("\n*** Based on the input you provided, only in-patient diagnoses (ipdiag) will be included in the derived disability indicator.
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

  ## Read file with disability codes from data folder
  disability_codes <- Rgemini::mapping_disability %>% data.table()

  ## Get encounters with disability
  # Previously used fuzzyjoin, but now removed that dependency due to downtime on CRAN, and to improve speed of function
  # Case-insensitive match (like ignore_case = TRUE)
  diagnoses[, diagnosis_code := toupper(diagnosis_code)]
  disability_codes[, `:=`(prefix = toupper(diagnosis_code), n = nchar(diagnosis_code))]

  res_component <- rbindlist(lapply(unique(disability_codes$n), \(k) {
    mapk <- disability_codes[n == k, .(prefix, disability_category)]
    setkey(mapk, prefix)
    # match based on pre-fix
    diagnoses[, .(genc_id, diagnosis_code, prefix = substr(diagnosis_code, 1, k))][
      mapk,
      on = "prefix", nomatch = 0L
    ][, .(genc_id, diagnosis_code, disability_category)]
  }), use.names = TRUE)


  ## Prepare output based on component-wise flag
  if (component_wise == FALSE) {
    ## Output with single row per genc_id in cohort file & global disability flag
    res <- cohort %>%
      select(genc_id) %>%
      distinct() %>%
      data.table()

    res[, disability := ifelse(!genc_id %in% diagnoses$genc_id, NA, # if no diagnosis code at all for genc_id, set flag to NA
      ifelse(genc_id %in% res_component$genc_id, TRUE, FALSE)
    )] # if entry in mapped diagnosis codes, set disability to TRUE, otherwise: FALSE

    return(res)
  } else {
    ## Return component-wise output with disability categories (sort by genc_id)
    res_component <- res_component[order(genc_id, diagnosis_code)]
    res_component <- res_component[genc_id %in% cohort$genc_id, ]
    return(res_component)
  }
}
