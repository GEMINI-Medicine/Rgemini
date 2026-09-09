#' @title
#' Birth Hospitalizations flag
#'
#' @description
#' Flag encounters corresponding to a birth hospitalization.
#' An encounter is flagged `TRUE` if it has at least one diagnosis code
#' containing `"Z38"` in the `ipdiagnosis` table (or, if
#' `include_er = TRUE`, both `ipdiagnosis` and
#' `erdiagnosis` tables), and a paediatric age in
#' weeks of 0 (`paeds_age_weeks = 0`) in the `admdad` table.
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param cohort (`data.table` or `data.frame`)\cr
#' Table with all relevant encounters of interest, where each row corresponds
#' to a single encounter. Must contain `genc_id`.
#'
#' @param include_er (`logical`)\cr
#' Whether to include diagnosis codes from the `erdiagnosis` table in addition
#' to `ipdiagnosis` when identifying `"Z38"` codes.
#'
#' @param healthy_birth (`logical`)\cr
#' Placeholder argument, currently unused. Reserved for a future
#' `healthy_birth` flag
#'
#' @return (`data.table`)\cr
#' For each unique `genc_id` in the input `cohort`, returns the `genc_id` and
#' a derived logical column `birth_hospitalization`:
#' - `TRUE`: at least one `"Z38"` diagnosis code found and
#'   `paeds_age_weeks = 0`.
#' - `FALSE`: at least one diagnosis code found, but the encounter does not
#'   meet both criteria above (no `"Z38"` code, or `paeds_age_weeks` not
#'   equal to 0).
#' - `NA`: no diagnosis codes found for the encounter in the table(s) queried,
#'   or `paeds_age_weeks` is missing (flag cannot be determined).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # establish a DB connection
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = "user",
#'   password = getPass("Enter password:")
#' )
#'
#' # flag birth hospitalizations for a cohort of encounters
#' birth_flags <- birth_hospitalizations(
#'   dbcon = dbcon, cohort = cohort,
#'   include_er = TRUE
#' )
#' }
#'
birth_hospitalizations <- function(dbcon, cohort, include_er = FALSE, healthy_birth = FALSE) {
  ## check that cohort contains genc_ids
  check_input(cohort, c("data.table", "data.frame"), colnames = c("genc_id"))

  ## create temp table of cohort
  temp_table(dbcon, cohort)

  ## identify actual DB table names
  ## (HPC datacuts may use `_subset`-suffixed table names)
  ipdiagnosis_table <- find_db_tablename(dbcon, "ipdiagnosis", verbose = FALSE)
  admdad_table <- find_db_tablename(dbcon, "admdad", verbose = FALSE)

  ## pull ipdiagnosis
  ipdiagnosis <- dbGetQuery(
    dbcon,
    paste0(
      "select ip.genc_id, ip.diagnosis_code from ", ipdiagnosis_table,
      " ip join rgemini_temp_table t on ip.genc_id = t.genc_id"
    )
  ) %>%
    data.table()

  ## if flagged, pull erdiagnosis, combine tables
  if (include_er == TRUE) {
    erdiagnosis_table <- find_db_tablename(dbcon, "erdiagnosis", verbose = FALSE)

    ## pull erdiagnosis
    erdiagnosis <- dbGetQuery(
      dbcon,
      paste0(
        "select er.genc_id, er.er_diagnosis_code from ", erdiagnosis_table,
        " er join rgemini_temp_table t on er.genc_id = t.genc_id"
      )
    ) %>%
      data.table()

    ## update column names for rbind
    setnames(erdiagnosis, "er_diagnosis_code", "diagnosis_code")
    ## combine all diagnoses
    diagnoses <- rbind(ipdiagnosis, erdiagnosis)
  } else {
    diagnoses <- ipdiagnosis
  }

  ## pull admdad for paeds_age_weeks
  admdad <- dbGetQuery(
    dbcon,
    paste0(
      "select a.genc_id, a.paeds_age_weeks from ", admdad_table,
      " a join rgemini_temp_table t on a.genc_id = t.genc_id"
    )
  ) %>%
    data.table()

  ## create output table
  res <- cohort %>%
    distinct(genc_id) %>%
    data.table()

  ## add birth_hospitalization flag
  res[, birth_hospitalization := ifelse(!genc_id %in% diagnoses$genc_id, NA, # if no diagnosis code at all for genc_id, set flag to NA
    ifelse(genc_id %in% diagnoses[grepl("^Z38", diagnosis_code, ignore.case = TRUE)]$genc_id & genc_id %in% admdad[paeds_age_weeks == 0]$genc_id, TRUE, FALSE) # if a diagnosis code is present
  )]

  ## for encounters with missing paeds_age_weeks, set flags to NA
  res[genc_id %in% admdad[is.na(paeds_age_weeks)]$genc_id, birth_hospitalization := NA]

  #   if(healthy_birth==TRUE){
  #     ## template code to make healthy birth variable
  #     res[, healthy_birth := ]
  #     res[genc_id %in% admdad[is.na(paeds_age_weeks)]$genc_id, healthy_birth := NA]
  #   }

  ## output
  return(res)
}
