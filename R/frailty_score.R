frailty_score  <- function(cohort, ipdiag, erdiag, componentwise=F){
  
  # load CIHI HFRS-ICD mapping from package data folder
  data("mapping_cihi_hfrs_icd", package = "Rgemini")
  frailty_map <- mapping_cihi_hfrs_icd %>% mutate(diagnosis_code = gsub("\\.", "", icd10ca)) %>% data.table()
 
   # input checks    
    cohort <- coerce_to_datatable(cohort)
    ipdiag <- coerce_to_datatable(ipdiag)
    erdiag <- coerce_to_datatable(erdiag)
    elig_enc <- unique(cohort[age>=65,]$genc_id)

  if (!all(c("genc_id", "age") %in% colnames(cohort))) {
    stop("Cohort must be a dataframe with columns:\n","genc_id, age")
    }

  # clean and merge all diagnosis codes; return a warning if EXPLICITLY set to NULL by user
  if (is.null(erdiag)){
    warning(
      "\nBased on user input, `erdiag` is set to NULL. This is NOT recommended. The CIHI frailty score was developed and validated based on diagnosis codes from both NACRS and DAD. Excluding erdiagnosis can underestimate the level of frailty (see reference in function documentation for details).\n"
      )
    } 
  else {
    erdiag <- erdiag[genc_id %in% elig_enc, .(genc_id, er_diagnosis_code)] %>% rename(diagnosis_code = er_diagnosis_code)
    }
 
  ipdiag <- ipdiag[genc_id %in% elig_enc, .(genc_id, diagnosis_code)]
  
  alldiag <- rbind(ipdiag, erdiag)   
  
  # Calculate score
  frailty <- alldiag %>% 
    regex_left_join(frailty_map, by = "diagnosis_code", ignore_case = TRUE) %>% # per CIHI manual frailty conditions are identified by 595 icd-10-ca codes and any codes starting with these codes
    data.table() %>% .[!is.na(frailty_categories), .(genc_id, diagnosis_code.x, diagnosis_code.y, frailty_categories)] # filter to encounters with diagnosis mapped to frailty conditions

  # Derive score (# of unique frailty categories) to encounters w/ diagnoses mapped
  res_score <- frailty[, .(frailty_score_derived = length(unique(frailty_categories))), genc_id]
  
  # Assign score=0 to encounters w/o diagnosis mapped to frailty conditions
  res_0 <- alldiag[!(genc_id %in% res_score$genc_id), .(genc_id) ] %>% .[, frailty_score_derived := 0] %>% distinct()
  
  res <- rbind(res_score, res_0) # combine
  
  # Componentwise result (available only for encounters w/ diagnoses mapped)
  res_component <- frailty[, .(genc_id, diagnosis_code.x, frailty_categories)] %>% rename(diagnosis_code = diagnosis_code.x)

  if (componentwise){
    return (res_component)
    } 

  return(res_score)

}