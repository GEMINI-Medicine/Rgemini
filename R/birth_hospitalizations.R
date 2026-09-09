birth_hospitalizations <- function(dbcon, cohort, include_er, healthy_birth) {
  ## check that cohort contains genc_ids
  check_input(cohort, c("data.table", "data.frame"), colnames = c("genc_id"))

  ## create temp table of cohort
  temp_table(dbcon, cohort)

  ## if flagged, pull erdiagnosis, combine tables
  if (include_er == TRUE) {
    ## pull ipdiagnosis
    ipdiagnosis <- dbGetQuery(dbcon, "select ip.genc_id, ip.diagnosis_code from ipdiagnosis ip join rgemini_temp_table t on ip.genc_id = t.genc_id") %>%
      data.table()

    ## pull erdiagnosis
    erdiagnosis <- dbGetQuery(dbcon, "select er.genc_id, er.er_diagnosis_code from erdiagnosis er join rgemini_temp_table t on er.genc_id = t.genc_id") %>% data.table()

    ## update column names for rbind
    setnames(erdiagnosis, "er_diagnosis_code", "diagnosis_code")
    ## combine all diagnoses
    diagnoses <- rbind(ipdiagnosis, erdiagnosis)
  } else {
    ## pull ipdiagnosis
    diagnoses <- dbGetQuery(dbcon, "select ip.genc_id, ip.diagnosis_code from ipdiagnosis ip join rgemini_temp_table t on ip.genc_id = t.genc_id") %>%
      data.table()
  }

  ## pull admdad for paeds_age_weeks
  admdad <- dbGetQuery(dbcon, "select a.genc_id, a.paeds_age_weeks from admdad a join rgemini_temp_table t on a.genc_id = t.genc_id") %>% data.table()

  ## create output table
  res <- cohort %>%
    distinct(genc_id) %>%
    data.table()

  ## add birth_hospitalization flag
  res[, birth_hospitalization := ifelse(!genc_id %in% diagnoses$genc_id, NA, # if no diagnosis code at all for genc_id, set flag to NA
      ifelse(genc_id %in% diagnoses[grepl("Z38", diagnosis_code, ignore.case = TRUE)]$genc_id & genc_id %in% admdad[paeds_age_weeks == 0]$genc_id, TRUE, FALSE) # if a diagnosis code is present
  )]

#   if(healthy_birth==TRUE){
#     ## template code to make healthy birth variable
#     res[, healthy_birth := ]
#   }

  ## output
  return(res)
}
