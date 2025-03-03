#' @title
#' Compute readmission and episodes of care.
#'
#' @description
#' `readmission.R` computes whether or not a patient associated with an episode of care was readmitted to a GEMINI
#' hospital within a time window of interest. The default readmission time window is 7 or 30 days.
#'
#' In line with CIHI guidelines, readmission calculations are based on episodes of care instead of individual
#' encounters in order to avoid that linked transfers are counted as readmissions (also see documentation for
#' `episodes_of_care()`). Specifically, an episode of care refers to all contiguous inpatient hospitalizations to
#' any medical or intensive care service within the GEMINI network. Episodes involving inter-facility transfers are
#' linked regardless of diagnosis. An acute care transfer is assumed to have occurred if either of the following
#' criteria are met:
#' \itemize{
#'   \item{An admission to a medical or intensive care service at a GEMINI hospital within 7 hours after discharge
#'   from another GEMINI hospital, regardless of whether the transfer is coded}
#'   \item{An admission to a medical or intensive care service at a GEMINI hospital within 7-12 hours after discharge
#'   from another GEMINI hospital, and at least 1 hospital has coded the transfer: Coded transfers are based on the
#'   DAD Institution From and Institution To fields}
#'  }
#' For episodes of care involving acute care transfers, readmissions are attributed to the last hospital from which
#' the patient was discharged before readmission.
#'
#' @section Warning:
#' Warnings are produced if
#'  1) either the `death` or `elective_admit` flags are set to `FALSE`,
#'  2) the input contains a user-specified restricted cohort, or
#'  3) more than 25% of episodes of care are removed from the denominator due to the CIHI flags or buffer period.
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#' @param elective_admit (`logical`)\cr
#' If `TRUE` (default), elective episodes of care are not considered to be true readmissions, and are therefore
#' removed from the numerator. Specifically, if the first encounter of the (n)th episode of care is elective
#' (`admit_category = 'L'`), the (n-1)th episode of care is not considered to be followed by a readmission
#' (`readmit(n-1) = FALSE`).
#' @param death (`logical`)\cr
#' If `TRUE` (default), episodes of care ending in in-hospital death are removed from the denominator. Specifically,
#' if the (n)th episode of care ended in death, it is impossible for a patient to be readmitted, and therefore the
#' episode of care is not considered in the readmission calculation (`readmit(n) = NA`).
#' @param MAID (`logical`)\cr
#' If `TRUE` (default): Episodes of care that involve MAID are not considered to be true readmissions, and are therefore
#' removed from the numerator. Specifically, if any encounter of the (n)th episode of care involves MAID, the (n-1)th
#' episode of care is not considered to be followed by a readmission (`readmit(n-1) = FALSE`).
#' Note that episodes of care involving MAID end in death, however, they are NOT removed from the denominator unless
#' the `death` flag is set to `TRUE` as well (recommended approach).
#' @param palliative (`logical`)\cr
#' If `TRUE` (default): Episodes of care involving palliative care are excluded from both the numerator and denominator.
#' Specifically, episode of care with palliative care as a type-M diagnosis are not considered to be true readmissions
#' (`readmit(n-1) = FALSE`). Additionally, similarly to death, palliative episodes of care cannot be
#' followed by future readmission and are therefore excluded from the denominator (`readmit(n) = NA`)
#' @param chemo (`logical`)\cr
#' If `TRUE` (default): Episodes of care that involve chemotherapy are not considered to be true readmissions, and are
#' therefore removed from the numerator. Specifically, if any encounter of the (n)th episode of care involves
#' chemotherapy, the (n-1)th episode of care is not considered to be followed by a readmission
#' (`readmit(n-1) = FALSE`).
#' @param mental (`logical`)\cr
#' If `TRUE` (default): Episodes of care for mental health are excluded from both the numerator and denominator.
#' Specifically, episodes of care that fall under CIHI's Major Clinical Category (MCC) 17 are not considered to be true
#' readmissions and are therefore removed from the numerator (`readmit(n-1) = FALSE`). Additionally, they
#' are not considered to be acute care and are therefore not included in the readmission denominator
#' (`readmit(n) = NA`)
#' @param obstetric (`logical`)\cr
#' If `TRUE` (default): Episodes of care that involve obstetric delivery are not considered to be true readmissions, and
#' are therefore removed from the numerator. Specifically, if any encounter of the (n)th episode of care involves
#' obstetric delivery, the (n-1)th episode of care is not considered to be followed by a readmission
#' (`readmit(n-1) = FALSE`).
#' @param signout (`logical`)\cr
#' If `TRUE` (default), episodes of care where the last encounter was self sign-out/left against medical advice (LAMA)
#' are removed from the denominator. Specifically, if the (n)th episode of care ended in self-signout/LAMA, it is not
#' considered in the readmission calculation (`readmit(n) = NA`).
#' @param return_readmit_enc (`logical`)\cr
#' If `TRUE`: The function will additionally return a column 
#' for each readmission window the user provides, containing the `genc_id` 
#' corresponding to the readmission encounter (i.e., encounter following the index encounter within n days). 
#' This can be used to analyze characteristics of the readmission encounter (e.g., time of readmission, diagnoses/clinical outcomes at the time of readmission etc.).
#'For index encounters where the readmission flag is `FALSE`/`NA`, `readmit(n)_genc_id` will be returned as `NA`.
#' @param restricted_cohort (`data.frame` or `data.table`)\cr
#' User specified cohort that is a restricted subset of all encounters in DRM table "ipadmdad" (see
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/)).
#' Must contain `genc_id` as the identifier. Default is `Null`, which loads the entire "ipadmdad"
#' table in the user-provided database (recommended approach).
#'
#' @param readm_win (`integer` | `vector`)\cr
#' readmission window(s) of interest, in days (by default: `readm_win = c(7,30)` to calculate 7- & 30-day readmission)
#'
#' @details
#' This function calls the `episodes_of_care()` function to link encounters with acute care transfers into episodes
#' of care. When conducting readmission calculations using the function output, the number of encounters where
#' `readmission == TRUE` is the numerator, and the number of eligible episodes of care (where `readmission != NA`)
#' is the denominator. For example, 7-day readmission rate can be calculated by `mean(readmit7, na.rm=TRUE)`, where
#' `readmit7` is the variable returned by the `readmission()` function. This function is designed to be consistent
#' with [CIHI guidelines for readmission calculations](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital).
#' Please carefully review this reference before applying the function for research purposes. Users may choose to deviate from
#' the CIHI definition of readmission by setting any of the CIHI flags (see above) to `FALSE`.
#'
#' @return
#' This function returns a `data.table` containing `genc_id`, `AT_in_occurred`, `AT_out_occurred`, `epicare`
#' (see [Rgemini::episodes_of_care()]), `readmit7` (default), and `readmit30` (default). `readmit7` and `readmit30`
#' are logical variables indicating whether or not an episode of care was followed by a readmission. Note that
#' readmissions are attributed to the last encounter of each episode of care. Episodes of care where the last encounter
#' has `readmitX = NA` have been removed from the denominator.
#'
#' Note: If the very last encounter of an episode of care has a transfer-out coding (but the encounter associated with
#' that transfer does not exist in the GEMINI database), the episode of care is excluded from the readmission
#' denominator (`readmit(n) = NA`). This is to avoid inaccurate attribution of a potential readmission to
#' the hospital associated with the intermediate (i.e., not the last) encounter of that episode of care.
#'
#' @seealso `vignette("epicare_and_readmission", package = "Rgemini")`
#'
#' @references
#' [CIHI readmission guidelines](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital)
#'
#' @export
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
#' ## default readmission calculation (with elective = TRUE & death = TRUE by default)
#' readmission(dbcon)
#'
#' ## Readmission calculation following CIHI definition
#' readmission(dbcon,
#'   elective_admit = TRUE, death = TRUE, MAID = TRUE, palliative = TRUE,
#'   chemo = TRUE, mental = TRUE, obstetric = TRUE, signout = TRUE
#' )
#' }
#'
#' @export


readmission <- function(dbcon,
                        elective_admit = TRUE,
                        death = TRUE,
                        MAID = FALSE,
                        palliative = FALSE,
                        chemo = FALSE,
                        mental = FALSE,
                        obstetric = FALSE,
                        signout = FALSE,
                        return_readmit_enc = FALSE,
                        restricted_cohort = NULL,
                        readm_win = c(7, 30)) {
  ## check user inputs
  check_input(dbcon, "DBI")
  check_input(list(elective_admit, death, MAID, palliative, chemo, mental, obstetric, signout), "logical")
  check_input(readm_win, "integer", interval = c(1, Inf)) # readmission window needs to be positive integer
  if (!is.null(restricted_cohort)) {
    check_input(restricted_cohort, c("data.table", "data.frame"),
      colnames = "genc_id"
    )
  }

  ## Warning if death == FALSE
  if (death == FALSE) {
    warning("By default, the `death` flag should always be set to `TRUE`.
    This is because, by definition, patients who died in hospital cannot be readmitted.
    We strongly recommend changing your input to `death = TRUE` to exclude patients who died from the readmission cohort.",
      immediate. = TRUE
    )
  }

  ## Warning if elective == FALSE
  if (elective_admit == FALSE) {
    warning("By default, the `elective` flag should always be set to `TRUE`.
    This is because elective readmissions are planned, and are therefore not considered to be true readmissions.
    We strongly recommend changing your input to `elective = TRUE` to avoid counting elective episodes of care as readmissions.",
      immediate. = TRUE
    )
  }

  cat("\nThis function may take a few minutes to run...\n")

  ############ Derive epicare = episode of care based on transfers ############
  cat("\nDeriving episodes of care ...\n")
  # Note: restricted cohort is the same in episodes_of_care & readmission function
  epicares <- episodes_of_care(dbcon = dbcon, restricted_cohort = restricted_cohort)

  # load epicare as a temptable
  DBI::dbSendQuery(dbcon, "Drop table if exists epicares_data;")
  DBI::dbWriteTable(dbcon, c("pg_temp", "epicares_data"), epicares[, .(genc_id)], row.names = F, overwrite = T)
  # Analyze speed up the use of temp table
  DBI::dbSendQuery(dbcon, "Analyze epicares_data")

  ############ Get additional DB variables ######################
  cat("Querying database ...\n")

  # find table corresponding to admdad
  admdad_name <- find_db_tablename(dbcon, "admdad", verbose = FALSE)

  # find variable name corresponding to hospital identifier (hospital_id/hospital_num)
  # to do minimial changes to querying one row to get all the column names instead
  admdad_cols <- dbGetQuery(dbcon, paste0("SELECT * from ", admdad_name, " limit 1;")) %>% data.table()

  admdad_cols <- data.table(column_name = colnames(admdad_cols))

  hospital_var <- admdad_cols[column_name %in% c("hospital_id", "hospital_num")]$column_name[1] # if multiple, use first identified variable

  admdad <- DBI::dbGetQuery(dbcon, paste0(
    "select genc_id, ", hospital_var, ", admission_date_time, discharge_date_time, admit_category, discharge_disposition
    from ", find_db_tablename(dbcon, "admdad", verbose = FALSE),
    " a where exists (select 1 from epicares_data t where t.genc_id=a.genc_id) ;"
  )) %>% as.data.table()
  data <- merge(admdad, epicares, by = "genc_id") %>% as.data.table()

  ## Ipdiagnosis data (if any relevant CIHI flag = TRUE)
  if (any(c(MAID, palliative, chemo, mental, obstetric))) {
    ipdiagnosis <- DBI::dbGetQuery(dbcon, paste0(
      "select genc_id, diagnosis_code, diagnosis_type
      from ", find_db_tablename(dbcon, "ipdiagnosis", verbose = FALSE),
      " i where exists (select 1 from epicares_data t where t.genc_id=i.genc_id);"
    )) %>% as.data.table()
  }

  ## Ipintervention data (for MAID)
  if (MAID) {
    ipintervention <- DBI::dbGetQuery(dbcon, paste0(
      "select genc_id, intervention_code
      from ", find_db_tablename(dbcon, "ipintervention", verbose = FALSE),
      " i where intervention_code in ('1ZZ35HAP7','1ZZ35HAP1','1ZZ35HAN3') AND
                                      exists (select 1 from epicares_data t where t.genc_id=i.genc_id);"
    )) %>% as.data.table()
  }

  ## Ipcmg data (for mental, see information below)
  if (mental) {
    ipcmg <- DBI::dbGetQuery(dbcon, paste0(
      "select  genc_id, cmg
      from ", find_db_tablename(dbcon, "ipcmg", verbose = FALSE),
      " i where exists (select 1 from epicares_data t where t.genc_id=i.genc_id)"
    )) %>% as.data.table()
  }

  ############  Convert date-times into appropriate format   ############
  data[, discharge_date_time := convert_dt(discharge_date_time)]
  data[, admission_date_time := convert_dt(admission_date_time)]

  data <- data[order(patient_id_hashed, discharge_date_time, admission_date_time)]

  # store next related genc_id, even if it's in the same episode of care
  # for now
  if (return_readmit_enc == TRUE) {
    data[, next_genc_id := shift(genc_id, type = "lead"), by = patient_id_hashed]
  }

  ## readmit coding by encounter based on time_to_next_admission
  # 7-/30-/X-day readmission = TRUE if time_to_next_admission is within 7/30/X days, otherwise readmission = FALSE
  lapply(readm_win, function(win) { # calculate readmit flag for each value in `readm_win`
    data[time_to_next_admission <= win * 24, paste0("readmit", win) := TRUE]
    data[time_to_next_admission > win * 24, paste0("readmit", win) := FALSE]
  })



  ############ ** Special condition to GEMINI readmission computation ** ############
  # If the very last encounter of an EPICARE has transfer out coding (defined by institution_to_type=='AT' |
  # acute_transfer_out=='AT in `episodes_of_care()`) AND time_to_next_admission >12, the epicare is excluded from
  # computation of readmission rate altogether (i.e. removed from denominator) as this indicates the patient
  # was transferred to a non-GEMINI hospital (i.e., data not available in GEMINI).
  # The exclusion is to avoid inaccurate attribution of potential readmission to the intermediate hospital/physician.
  epi_special_transferout <- data[AT_out_coded & time_to_next_admission > 12, .SD[.N], by = epicare]$epicare
  lapply(readm_win, function(win) {
    # set readmit7/30/X to NA to remove from denominator
    data[epicare %in% epi_special_transferout, paste0("readmit", win) := NA]
  })


  ############ numerator, denominator flags ############
  # For a given episode of care `n`:
  # - Removal from NUMERATOR depends on (n+1)th epicare (e.g., readmit(n) = FALSE if (n+1)th epicare is not
  #   considered to be true readmission)
  # - Removal from DENOMINATOR depends on the (n)th epicare itself (e.g., whether this epicare should be excluded
  #   from readmission calculation, i.e., readmit(n) = NA)


  ### flag 1. elective admit
  # Elective admissions are planned, and therefore, episodes of care that are elective are removed from numerator
  # Removal from numerator: The epicare prior to an elective encounter has readmitX = FALSE
  # Note:
  # An episode of care is considered elective if the FIRST encounter of that episode is elective (admit_category = 'L')
  # Since we only keep last encounter of each episode of care (see below) we only need to shift all rows by 1 to check
  # if 1st encounter of next epicare is elective
  if (elective_admit == TRUE) {
    cat("\nExcluding elective admissions ...\n")
    data <- data[order(patient_id_hashed, discharge_date_time, admission_date_time)]
    data[, admit_category_lead := shift(admit_category, type = "lead"), by = patient_id_hashed]

    ## remove from numerator: readmit = FALSE for epicare before elective admission
    lapply(readm_win, function(win) {
      # only set readmitX to FALSE if readmitX is currently not NA due to previous flags
      # (= removal from denominator, which takes priority)
      data[admit_category_lead == "L" & !is.na(get(paste0("readmit", win))), paste0("readmit", win) := FALSE]
    })

    data[, c("admit_category_lead")] <- NULL
  }


  ### flag 2. death
  # If the index admission results in death, there cannot be any readmission.
  # Therefore, episodes of care with death are excluded from the denominator (i.e. reamitX=NA).
  if (death == TRUE) {
    cat("\nExcluding deaths ...\n")
    ## Remove episodes with deaths from denominator
    death_epi <- data[discharge_disposition %in% c(7, 72, 73, 74), ]$epicare
    lapply(readm_win, function(win) {
      data[epicare %in% death_epi, paste0("readmit", win) := NA] # set readmit7/30/X to NA to remove from denominator
    })
  }


  ### flag 3. MAID
  # If an epicare has ANY encounter with MAID, it is not considered to be a true readmission and is therefore
  # removed from numerator.
  # Thus, the (n-1)th epicare (i.e. epicare BEFORE the MAID epicare) has readmitX=FALSE.
  # As discharge disposition=73 is a CIHI code that only appeared after 2018-04-01, different criteria are used to
  # identify MAID before 2018-04-01.
  # Specifically, before 2018-04-01 MAID is TRUE if ANY genc_id of an epicare has discharge disposition=7 AND
  # all 3 records of drugs (1st drug lets patient relax, 2nd drug stops heart beat, 3rd drug stops brain function;
  # indicated by intervention_codes)
  # Note: If death = TRUE, the epicare with MAID (which results in death) is additionally removed from the denominator.
  # If death = FALSE, the epicare with MAID is kept in the denominator. The recommendation is to set death to TRUE.
  if (MAID == TRUE) {
    cat("\nExcluding MAID ...\n")
    ## Identify MAID epicare: consider whole epicare as maid=TRUE if ANY encounter of the epicare has maid=TRUE
    # MAID epicare after FY18-19
    epi_maid_post <- data[discharge_date_time >= lubridate::ymd_hm("2018-04-01 00:00") &
      discharge_disposition == 73]$epicare

    # MAID epicare before FY18-19
    cci_maid <- ipintervention[intervention_code %in% c("1ZZ35HAP7", "1ZZ35HAP1", "1ZZ35HAN3"), ] # MAID intervention
    cci_maid <- merge(cci_maid, data[, c("genc_id", "discharge_date_time")], by = "genc_id", all.x = TRUE)
    cci_maid <- cci_maid[discharge_date_time < lubridate::ymd_hm("2018-04-01 00:00") &
      genc_id %in% data[discharge_disposition == 7, genc_id]]
    ids <- cci_maid[, lunique(intervention_code), genc_id] %>% .[V1 == 3, genc_id]
    epi_maid_pre <- data[genc_id %in% ids]$epicare

    # Define MAID epicare
    data[epicare %in% c(epi_maid_pre, epi_maid_post), maid := TRUE]

    # Removal from numerator: episode before MAID readmitX=FALSE, make sure overwriting doesn't apply to readmitX=NA,
    # because readmitX=NA means removal from denominator which takes priority over numerator coding.
    data <- data[order(patient_id_hashed, discharge_date_time, admission_date_time)]
    data[, maid_lead := shift(maid, type = "lead"), by = patient_id_hashed]
    lapply(readm_win, function(win) {
      # only set readmitX to FALSE if readmitX is currently not NA due to previous flags
      # (= removal from denominator, which takes priority)
      data[maid_lead == TRUE & !is.na(get(paste0("readmit", win))), paste0("readmit", win) := FALSE]
    })

    data[, c("maid", "maid_lead")] <- NULL
  }


  ### flag 4. palliative
  # When an episode of care includes ANY encounter with palliative care as most responsible discharge diagnosis,
  # it is removed from numerator and denominator
  # 1) Removal from numerator: The epicare prior to palliative care has readmitX = FALSE
  # 2) Removal from denominator: The epicare with palliative care is removed from cohort altogether (readmitX = NA)
  if (palliative == TRUE) {
    cat("\nExcluding palliative care ...\n")
    ## Get ICD-10-CA codes for palliative care
    icd_pall <- ipdiagnosis[diagnosis_type == "M" & diagnosis_code == "Z515", ]

    ## Identify episodes of care that have ANY encounter with palliative care
    epi_palliative <- data[genc_id %in% icd_pall$genc_id]$epicare

    ## remove from numerator: readmit7=FALSE for epicare before palliative care
    data[epicare %in% epi_palliative, palliative := TRUE]
    data[, palliative_lead := shift(palliative, type = "lead"), by = patient_id_hashed]
    lapply(readm_win, function(win) {
      # only set readmitX to FALSE if readmitX is currently not NA due to previous flags
      # (= removal from denominator, which takes priority)
      data[palliative_lead == TRUE & !is.na(get(paste0("readmit", win))), paste0("readmit", win) := FALSE]
    })


    ## remove denominator: readmit7=NA for epicare with palliative care
    lapply(readm_win, function(win) {
      data[epicare %in% epi_palliative, paste0("readmit", win) := NA]
    })

    data[, c("palliative", "palliative_lead")] <- NULL
  }


  ### flag 5. chemo
  # When an episode of care (epicare) included ANY encounter with chemotherapy of diagnosis types M|1|W|X|Y, the entire
  # epicare is considered as a chemotherapy episode.
  # Readmission with chemotherapy is not considered a true readmission, and it is removed from numerator.
  # Removal from numerator: The epicare prior to chemotherapy epicare has readmitX=FALSE.
  if (chemo == TRUE) {
    cat("\nExcluding chemotherapy ...\n")
    ## Get ICD-10-CA codes for chemotherapy
    icd_chemo <- ipdiagnosis[diagnosis_type %in% c("M", "1", "W", "X", "Y") & diagnosis_code == "Z511", ]

    # Identify chemo epicare (i.e. epicares that have ANY encounters of chemotherapy coded)
    epi_chemo <- data[genc_id %in% icd_chemo$genc_id]$epicare

    # Remove from numerator: the epicare BEFORE chemo gets readmitX=FALSE
    data[epicare %in% epi_chemo, chemo := TRUE]

    data <- data[order(patient_id_hashed, discharge_date_time, admission_date_time)]
    data[, chemo_lead := shift(chemo, type = "lead"), by = patient_id_hashed]

    lapply(readm_win, function(win) {
      # only set readmitX to FALSE if readmitX is currently not NA due to previous flags
      # (= removal from denominator, which takes priority)
      data[chemo_lead == TRUE & !is.na(get(paste0("readmit", win))), paste0("readmit", win) := FALSE]
    })

    data[, c("chemo", "chemo_lead")] <- NULL
  }

  ### flag 6. mental health
  # When an episode of care includes ANY encounter with a mental health diagnosis as defined in paragraph below,
  # it is removed from numerator and denominator
  # 1) Removal from numerator: The epicare prior to mental health epicare has readmitX = FALSE
  # 2) Removal from denominator: The epicare with mental health diagnosis is removed from cohort altogether by
  #    setting readmitX = NA

  # Mental health diagnosis is defined by CIHI major clinical categories (MCC), MCC=17.
  # Overall limitations: No data contain MCC, so we need to use CMG (case mix group) from ipcmg table & ICD-10-CA
  # diagnosis codes from ipdiagnoisis table to approximate MCC.
  # Limitation in ipcmg: some sites don't have ipcmg data (e.g., entire site missing/missing time periods)
  # Limitation in ipdiagnosis: Part of CIHI's criteria of defining MCC=17 includes the use of asterisk ICD-10-CA codes
  # (e.g. F001*) for diagnosis_type=6. However, our data does not distinguish asterisk vs non-asterisk diagnosis codes.
  # Thus, both ipdiagnosis and ipcmg are used.
  # Coding logic
  # 1) If cmg field is present, encounters having any cmg values under MCC of 17 are labelled as having a mental health
  #    diagnosis. We prioritize cmg over diagnosis coding to avoid limitation in ipdiagnosis (i.e. uncertainty in
  #    asterisk vs non-asterisk coding).
  # 2) If cmg field is missing OR encounters are only found in ipdiagnosis, encounters having diagnosis codes & type of
  #    M or 6 are labelled as having a mental health diagnosis. This approach may slightly overcount due to the
  #    inclusion of non-asterisk coding from diagnosis_type=6.


  if (mental == TRUE) {
    cat("\nExcluding mental health ...\n")
    ## Define mental health encounters based on cmg/diagnosis codes

    # if cmg present, use cmg (takes priority over ICD codes)
    cmg_codes <- c(
      "704", "670", "671", "672", "673", "678", "683", "684", "685", "686", "687", "689",
      "691", "693", "694", "697", "698", "702", "703", "707", "708", "709"
    )
    cmg_mental <- ipcmg[cmg %in% cmg_codes]

    # Get diagnosis codes for mental health
    diag_codes <- c(
      "F000", "F001", "F002", "F009", "F010", "F011", "F012", "F013", "F018", "F019", "F020", "F021", "F022", "F023",
      "F024", "F028", "F03", "F04", "F050", "F051", "F058", "F059", "F060", "F061", "F062", "F063", "F064", "F065",
      "F066", "F067", "F068", "F069", "F070", "F071", "F078", "F079", "F09", "F100", "F101", "F102", "F103", "F104",
      "F105", "F106", "F107", "F108", "F109", "F110", "F111", "F112", "F113", "F114", "F115", "F116", "F117", "F118",
      "F119", "F1200", "F1201", "F1210", "F1211", "F1220", "F1221", "F1230", "F1231", "F1240", "F1241", "F1250",
      "F1251", "F1260", "F1261", "F1270", "F1271", "F1280", "F1281", "F1290", "F1291", "F130", "F131", "F132", "F133",
      "F134", "F135", "F136", "F137", "F138", "F139", "F140", "F141", "F142", "F143", "F144", "F145", "F146", "F147",
      "F148", "F149", "F1500", "F1508", "F1509", "F1510", "F1518", "F1519", "F1520", "F1528", "F1529", "F1530", "F1538",
      "F1539", "F1540", "F1548", "F1549", "F1550", "F1558", "F1559", "F1560", "F1568", "F1569", "F1570", "F1578",
      "F1579", "F1580", "F1588", "F1589", "F1590", "F1598", "F1599", "F160", "F161", "F162", "F163", "F164", "F165",
      "F166", "F167", "F168", "F169", "F170", "F171", "F172", "F173", "F174", "F175", "F176", "F177", "F178", "F179",
      "F180", "F181", "F182", "F183", "F184", "F185", "F186", "F187", "F188", "F189", "F1908", "F19098", "F19099",
      "F1918", "F19198", "F19199", "F1928", "F19298", "F19299", "F1938", "F19398", "F19399", "F1948", "F19498",
      "F19499", "F1958", "F19598", "F19599", "F1968", "F19698", "F19699", "F1978", "F19798", "F19799", "F1988",
      "F19898", "F19899", "F1998", "F19998", "F19999", "F200", "F201", "F202", "F203", "F204", "F205", "F206", "F208",
      "F209", "F21", "F220", "F228", "F229", "F230", "F231", "F232", "F233", "F238", "F239", "F24", "F250", "F251",
      "F252", "F258", "F259", "F28", "F29", "F300", "F301", "F302", "F308", "F309", "F310", "F311", "F312", "F313",
      "F314", "F315", "F316", "F317", "F318", "F319", "F320", "F321", "F322", "F323", "F328", "F329", "F330", "F331",
      "F332", "F333", "F334", "F338", "F339", "F340", "F341", "F348", "F349", "F380", "F381", "F388", "F39", "F400",
      "F401", "F402", "F408", "F409", "F410", "F411", "F412", "F413", "F418", "F419", "F420", "F421", "F422", "F428",
      "F429", "F430", "F431", "F432", "F438", "F439", "F440", "F441", "F442", "F443", "F444", "F445", "F446", "F447",
      "F448", "F449", "F450", "F451", "F452", "F453", "F454", "F458", "F459", "F480", "F481", "F488", "F489", "F500",
      "F501", "F502", "F503", "F504", "F505", "F508", "F509", "F510", "F511", "F512", "F513", "F514", "F515", "F518",
      "F519", "F520", "F521", "F522", "F523", "F524", "F525", "F526", "F527", "F528", "F529", "F530", "F531", "F538",
      "F539", "F54", "F55", "F59", "F600", "F601", "F602", "F603", "F604", "F605", "F606", "F607", "F608", "F609",
      "F61", "F620", "F621", "F628", "F629", "F630", "F631", "F632", "F633", "F638", "F639", "F64", "F652", "F653",
      "F654", "F658", "F659", "F680", "F681", "F688", "F69", "F700", "F701", "F708", "F709", "F710", "F711", "F718",
      "F719", "F720", "F721", "F728", "F729", "F730", "F731", "F738", "F739", "F780", "F781", "F788", "F789", "F790",
      "F791", "F798", "F799", "F800", "F801", "F802", "F803", "F808", "F809", "F810", "F811", "F812", "F813", "F818",
      "F819", "F82", "F83", "F840", "F841", "F843", "F844", "F845", "F848", "F849", "F88", "F89", "F900", "F901",
      "F908", "F909", "F910", "F911", "F912", "F913", "F918", "F919", "F920", "F928", "F929", "F930", "F931", "F932",
      "F933", "F938", "F939", "F940", "F941", "F942", "F948", "F949", "F980", "F981", "F982", "F983", "F984", "F985",
      "F986", "F988", "F989", "F99", "G300", "G301", "G3080", "G3081", "G3082", "G3088", "G309", "G312", "Q900", "Q901",
      "Q902", "Q909", "Q910", "Q911", "Q912", "Q913", "Q914", "Q915", "Q916", "Q917", "Q930", "Q931", "Q932", "Q933",
      "Q934", "Q935", "Q936", "Q937", "Q938", "Q939", "R440", "R442", "R480", "R481", "R482", "R488", "R54", "Z004",
      "Z032", "Z046", "Z133", "Z134", "Z502", "Z503", "Z504", "Z730", "Z733"
    )

    # if cmg field is missing OR genc_id not found in ipcmg, use diagnosis code + type= M or 6 in ipdiagnosis
    cmg_na <- ipcmg[is.na(cmg) | cmg == ""]
    genc_id_na <- ipdiagnosis[!genc_id %in% ipcmg$genc_id]

    diag_mental <- ipdiagnosis[(genc_id %in% cmg_na$genc_id | genc_id %in% genc_id_na$genc_id) &
      diagnosis_code %in% diag_codes & diagnosis_type %in% c("M", "6")]

    ## Identify episodes of care that have ANY encounter with mental health cmg/diagnosis
    epi_mental <- data[genc_id %in% c(cmg_mental$genc_id, diag_mental$genc_id)]$epicare

    ## remove from numerator: readmit7=FALSE for epicare before mental health epicare
    data[epicare %in% epi_mental, mental := TRUE]
    data[, mental_lead := shift(mental, type = "lead"), by = patient_id_hashed]
    lapply(readm_win, function(win) {
      # only set readmitX to FALSE if readmitX is currently not NA due to previous flags
      # (= removal from denominator, which takes priority)
      data[mental_lead == TRUE & !is.na(get(paste0("readmit", win))), paste0("readmit", win) := FALSE]
    })


    ## remove from denominator: readmit7=NA for epicare with mental health diagnosis
    lapply(readm_win, function(win) {
      data[epicare %in% epi_mental, paste0("readmit", win) := NA] # set readmit7/30/X to NA to remove from denominator
    })

    data[, c("mental", "mental_lead")] <- NULL
  }


  ### flag 7. obstetric delivery
  # Episodes of care that have ANY encounter with obstetric delivery are not considered a true readmission, and are
  # therefore removed from numerator
  # Removal from numerator: The epicare prior to obstetric delivery has readmitX = FALSE
  if (obstetric == TRUE) {
    cat("\nExcluding obstetric delivery ...\n")
    ## Get ICD-10-CA codes for obstetric delivery
    icd_obs <- ipdiagnosis[grepl(
      "^Z37|(^O(1[0-6]|2[1-9]|3[0-7]|4[0-6]|48|6[0-9]|7[0-5]|8[5-9]|9[0-2]|95|98|99)[0-9]{2}[1-2])", # obstetrics codes
      diagnosis_code
    ), c("genc_id", "diagnosis_code")]

    ## Identify episodes of care that have ANY encounter with obstetric delivery
    epi_obs <- data[genc_id %in% icd_obs$genc_id]$epicare
    data[epicare %in% epi_obs, obstetric := TRUE]

    # Shift obstetric variable to check if NEXT epicare of a given patient has an encounter with obstetric delivery
    data <- data[, obstetric_lead := shift(obstetric, type = "lead"), by = patient_id_hashed]

    ## remove from numerator: readmit7=FALSE for epicare before obstetric delivery
    lapply(readm_win, function(win) {
      # only set readmitX to FALSE if readmitX is currently not NA due to previous flags
      # (= removal from denominator, which takes priority)
      data[obstetric_lead == TRUE & !is.na(get(paste0("readmit", win))), paste0("readmit", win) := FALSE]
    })

    data[, c("obstetric", "obstetric_lead")] <- NULL
  }

  ### flag 8. self sign-out / LAMA
  # When LAST encounter of episode of care has discharge disposition = self sign-out/left against medical advice
  # (LAMA), the episode is removed from denominator to avoid attribution of potential readmission to physician
  # Removal from denominator: The epicare with self sign-out is removed from denominator by setting readmitX = NA
  # All discharge_dispositions starting with 6 are left against medical advice (LAMA):
  ## 6  - sign-out (deprecated)
  ## 61 - absent without leave (AWOL)
  ## 62 - left against medical advice
  ## 65 - did not return from pass/leave
  ## 66 - died while on pass/leave
  ## 67 - suicide out of facility
  if (signout == TRUE) {
    cat("\nExcluding self sign-out ...\n")
    # check if last encounter of epicare had discharge disposition starting with 6
    epi_lama <- data[, .SD[.N], by = "epicare"][grepl("^6", discharge_disposition)]$epicare

    # Removal from denominator: readmitX = NA
    lapply(readm_win, function(win) {
      data[epicare %in% epi_lama, paste0("readmit", win) := NA]
    })
  }


  ### time buffer
  cat("\nExcluding episodes in time buffer ...\n")
  # based on data availability for each hospital
  # buffer = readmission window (e.g., 7 or 30 days) + additional 30 days (because patient needs to be discharged before
  # appearing in DB & typical length of hospitalization is < 30 days)
  lapply(readm_win, function(win) {
    data[, paste0("buffer", win) := max(discharge_date_time) - days(win + 30), by = get(hospital_var)]
  })

  # if encounter is out of time buffer, there's not enough time for readmission to happen (readmission=NA)
  lapply(readm_win, function(win) {
    data[data$discharge_date_time > data[[paste0("buffer", win)]], paste0("readmit", win) := NA]
  })


  ### Warning if more than 25% of episodes of care have been removed from denominator:
  # Readmission rate estimates might be unreliable
  # This can happen if...
  # 1) CIHI flags are TRUE and the cohort is highly restricted (e.g., certain cohorts might have a lot of encounters
  #    with palliative/mental care, death, or self-signout, which are all removed from denominator)
  # 2) When a very long readmission window has been selected (long buffer period) and/or the time period of interest is
  #    towards the end of the data availability timeline (e.g., late 2022 data)
  check_idx <- data[, .SD[.N], by = epicare] # get last encounter of each epicare for check
  na_rate_overall <- lapply(readm_win, function(win) {
    # get % epicares where last encounter has readmit = NA, separately for each readmission window
    round(100 * sum(is.na(check_idx[[paste0("readmit", win)]])) / lunique(data$epicare), digits = 1)
  })
  na_rate_buffer <- lapply(readm_win, function(win) {
    # get % epicares where last encounter is past buffer period, separately for each readmission window
    round(100 * nrow(check_idx[discharge_date_time > get(paste0("buffer", win))]) /
      lunique(data$epicare), digits = 1)
  })
  if (any(na_rate_overall > 25)) {
    warning(
      paste0(
        "Up to ", max(unlist(na_rate_overall)),
        "% of episodes of care have been removed from the denominator. This can be due to
                   1) some CIHI flags (death/palliative/mental/signout) or
                   2) removal due to the buffer period, which is 30 days + readmission window.
        The removal due to the buffer period was ", paste(na_rate_buffer, collapse = "%/"), "% for ",
        paste(readm_win, collapse = "/"), "-day readmission.
        Readmission rate estimates might be unreliable!
        Please consider re-calculating readmission rates with shorter readmission windows or with a larger cohort."
      ),
      immediate. = TRUE
    )
  }


  ### Finalize outputs
  ## keep all encounters from original data set but only last encounter of epicare should have readmit flag
  # (readmit7/30 for all other encounters of epicare are set to readmit7/30 = NA)
  # This assumption means that the next_genc_id (if it exists) is truly a 
  # readmissison, rather than a transfer

  data <- data[order(epicare, discharge_date_time, admission_date_time)]
  lapply(readm_win, function(win) {
    # if last encounter of epicare, keep readmit value, otherwise set to NA
    data[, paste0("readmit", win) := ifelse(seq_len(.N) != .N, NA, get(paste0("readmit", win))), by = epicare]
  })

  ## Get final output variables
  # If return_readmit_enc == TRUE, create readmit genc_id column for each window
  if (return_readmit_enc == TRUE) {
    lapply(readm_win, function(win) {
      data[, paste0("readmit", win, "_genc_id") := ifelse(get(paste0("readmit", win)) == TRUE, next_genc_id, NA)]
    })
  }
  
  vars <- c(
    "genc_id", "AT_in_occurred", "AT_out_occurred", "epicare",
    grep("^readmit", names(data), value = TRUE) # all readmit 7/30/X... values
  )
  dataf <- data[, names(data) %in% vars, with = FALSE]

  cat("\nDONE!")
  return(dataf)
}

