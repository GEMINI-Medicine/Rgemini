#' @title 
#' Compute Length of Stay in Emergency Room
#' 
#' @description
#' Calculate the total duration in Emergency Room (ER) that an encounter spent
#' through a hospitalization using CIHI National Ambulatory Care Reporting System 
#' (NACRS) fields
#' 
#' @details
#' This function calculates the length of stay (LoS) in hours and days that an 
#' encounter spent in the ER during a hospital stay.
#' 
#' It uses NACRS fields Triage Date and Time (Data Element Number 24/25) and 
#' Date and Time Patient Left Emergency Department (Data Element Number 116/117).
#' 
#' Rows with either triage date-time or left ER date-time missing will be 
#' excluded from the calculation.
#' 
#' For encounters with no ER visit, the function returns 0 in both
#' `er_los_hrs_derived` and `er_los_days_derived`
#' 
#' @param cohort (`data.table` or `data.frame`)\cr
#' Table with all relevant encounters of interest, where each row corresponds to
#' a single encounter. Must contain GEMINI Encounter ID (`genc_id`).
#'
#' @param er (`data.table` or `data.frame`)\cr
#' Table equivalent to the `er` table defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Table must contain fields:
#' GEMINI Encounter ID (`genc_id`),
#' ER triage date-time (`triage_date_time` in "yyyy-mm-dd hh:mm" format),
#' Left ER date-time (`left_er_date_time` in "yyyy-mm-dd hh:mm" format).
#' 
#' @return (`data.table`)\cr
#' By default, for each encounter in input "cohort" returns the corresponding 
#' derived numeric fields "er_los_hrs_derived".
#' 
#' @note
#' By design, function will not return any NA values.
#' Encounter IDs in the `cohort` table that are not present in the `er` table are assumed to have no ER visit.
#' For these encounters, a value of 0 will be assigned to the derived fields.
#' 
#' @export
#' 
#' @examples
#' # Compute ER LoS for all encounters in ipadmdad;
#' \dontrun{
#' er_los(cohort = ipadmdad, er = er)
#' }

er_los <- function(cohort, er){
  
  ###### Check user inputs ######
  ## table provided as data.frame/data.table
  if (!any(class(cohort) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for cohort. Please provide a data.frame or a data.table.")}
  
  if (!any(class(er) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for ipscu Please provide a data.frame or a data.table.")}
  
  ## table contains required fields
  if (any(!"genc_id" %in% names(cohort))) {
    stop("Input cohort is missing the required variable 'genc_id'.
          Refer to function documentation for details.")}
  
  if (any(!c("genc_id", "triage_date_time", "left_er_date_time") %in% names(er))) {
    stop("Input er is missing at least one of the required variables.
          Refer to function documentation for details.")}
  
  ###### Prepare data ######
  ## coerce tables to data.table as function uses data.table syntax
  cohort <- coerce_to_datatable(cohort)
  er <- coerce_to_datatable(er)
  
  ##### prepare er data #####
  er[, `:=`(triage_date = lubridate::ymd_hm(triage_date_time),
            left_er_date_time = lubridate::ymd_hm(left_er_date_time))]
  er <- er[!is.na(triage_date_time) & !is.na(left_er_date_time)]
  
  ### Derive er length of stay ###
  er[, `:=`(er_los_hrs_derived = as.numeric(difftime(left_er_date_time, triage_date_time, units = "hours")),
            er_los_days_derived = as.numeric(difftime(left_er_date_time, triage_date_time, units = "days"))
  )]
  
  ### Merge result with cohort ###
  res <- merge(cohort[, .(genc_id)],
               er[,.(genc_id, er_los_hrs_derived, er_los_days_derived)],
               by = "genc_id",
               all.x = T, all.y = F) %>%
    # assign 0 to encounters with no ER visits
    dplyr::mutate(across(starts_with("er_los"), ~ ifelse(is.na(.), 0, .)))
  
  return(res)
}
