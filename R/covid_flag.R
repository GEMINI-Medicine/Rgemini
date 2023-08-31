#' @title
#' COVID Flag
#'
#' @description
#' `covid_flag` returns COVID-19 diagnoses status
#' for hospital admissions based on ICD-10-CA
#'
#' Return a dataframe with two separate flags identifying
#' encounters with confirmed and suspected COVID-19 diagnoses
#' using ICD-10-CA U071 and U072 respectively
#'
#' A confirmed COVID-19 diagnosis (U071) is coded when
#' there is a positive COVID-19 test regardless of the the test type.
#'
#' A suspected COVID-19 diagnosis (U072) is coded when an
#' encounter is clinically or epidemiological diagnosed
#' but the associated COVID-19 tests are inconclusive,
#' not available, or not performed.
#'
#'
#' @details
#' This function takes a list of admissions and the list of ICD-10-CA
#' to generate boolean fields indicating whether
#' each admission had confirmed or suspected COVID-19 diagnoses.
#'
#' Function can take any data frame object but coerced into data.table
#' with ICD-10-CA but these flags were meant to be calculated based on
#' in-patient diagnoses (ED diagnoses have lower sensitivity/specificity
#' for certain conditions).
#' Function does not differentiate between diagnosis type.
#'
#' Below is the current ICD-10-CA codings related to COVID-19.
#' For more details, please refer to the references in this page.
#'
#' \itemize{
#'  \item{U07.1 : }{For confirmed cases of COVID-19, assign U07.1
#'  Emergency use of U07.1 (COVID-19, virus identified)}
#'  \item{U07.2 : }{For suspected cases of COVID-19, assign U07.2
#'  Emergency use of U07.2 (COVID-19, virus not identified)}
#'  \item{U07.3 : }{Multisystem inflammatory syndrome associated
#'  with COVID-19}
#'  \item{U07.4 : }{post COVID-19 condition}
#'  \item{U07.5 : }{personal history of COVID-19}
#'  \item{U07.6 : }{Need for immunization against COVID-19.
#'  This code is for use when a person encounters health services
#'  for the sole purpose of receiving the COVID-19 vaccine.}
#'  \item{U07.7 : }{COVID-19 vaccines causing adverse effect in therapeutic use.
#'  This emergency use code is an external cause code.
#'  This code would normally be located at the ICD-10-CA block Y40-Y59 Drugs,
#'  medicaments and biological substances causing
#'  adverse effects in therapeutic use.}
#' }
#'
#' @section Warning:
#' Function returns data.table with id field and two boolen fields. NA value in
#' the output indicates that the encounter did not have any entry in the
#' diagnosis table.
#' When one tries to left-join the output of this function with another table
#' (another list of admissions in the left),
#' make sure list of admissions (or patient) aligns in both tables.
#'
#' @param ipadmdad a data.table object equivalent of DRM table "ipadmdad".
#' Table must contain field genc_id
#' @param diagnosis a data.table object equivalent of DRM table "ipdiagnosis".
#' Table must contain two fields,
#' genc_id and diagnosis_code (as ICD-10-CA) and the function
#' handles long format table only
#' Its value must be free from any special character.
#' If user wishes to use erdiagnosis instead or in combination with ipdiagnosis,
#' be sure to rename er_diagnosis_code to diagnosis_code before combining
#' with ipdiagnosis or input into the function.
#'
#' @return
#' data.table with the same number of rows with input "ipadmdad" with addtional
#' derived boolean fields labelled as "covid_icd_confirmed_flag"
#' and "covid_icd_suspected_flag". Possible values are
#' TRUE, FALSE or NA
#' NA indicates that an encounter does not have a diagnosis code
#' in diagnosis table
#'
#' @examples
#' \dontrun{
#' db_driver <- "PostgreSQL"
#' db_password <- getPass::getPass
#' db_host <- "172.XX.XX.XXX"
#' db_port <- 1234
#'
#' db <- DBI::dbConnect(DBI::dbDriver(db_driver),
#'   dbname = "db",
#'   host = db_host,
#'   port = db_port,
#'   user = db_password("Enter DB credential username"),
#'   password = db_password("Enter DB credential password")
#' )
#'
#' ipadm <- dbGetQuery(db, "select * from admdad") %>% data.table()
#'
#' dx <- dbGetQuery(db, "select * from ipdiagnosis") %>% data.table()
#'
#' covid <- covid_flag(ipadmdad = ipadm, diagnosis = dx)
#' }
#'
#' @references
#' https://www.cihi.ca/en/covid-19-resources/covid-19-data-collection-and-coding-direction
#'
#' @export
covid_flag <- function(ipadmdad,
                       diagnosis) {
  ## remap variable names in case field names change in the database
  res <- coerce_to_datatable(ipadmdad)[, .(genc_id)]
  res2 <- coerce_to_datatable(diagnosis)[, .(genc_id, diagnosis_code)]

  ## flag those with U071 or U072 in any diagnosis type
  confirmed <- res2[grep("U071", diagnosis_code), genc_id]
  suspected <- res2[grep("U072", diagnosis_code), genc_id]

  res[, ":="(covid_icd_confirmed_flag = ifelse(!genc_id %in% res2$genc_id, NA,
    ifelse(genc_id %in% confirmed,
      TRUE, FALSE
    )
  ),

  covid_icd_suspected_flag = ifelse(!genc_id %in% res2$genc_id, NA,
    ifelse(genc_id %in% suspected,
      TRUE, FALSE
    )
  ))][]

  return(res)
}
