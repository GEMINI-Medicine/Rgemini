#' @title
#' Compute entry to Intensive Care Unit
#'
#' @description
#' Determines whether an encounter has entered Intensive Care Unit (ICU) during
#' hospital stay using CIHI Discharge Abstract Database (DAD) fields.
#'
#' @details
#' This function uses DAD fields Admission date/time (Group 04 Fields 01/02),
#' and SCU Admit date/time (Group 13, Field 03/04) to derive boolean fields
#' indicating ICU entries at any time during hospital stay,
#' and within specified time window since hospital admission.
#'
#' By definition in DAD (Group 13, Field 02), SCUs include ICUs and Step-Down Units.
#' Step-Down Units are not considered as ICUs.
#'
#' Therefore, this function excludes below CIHI defined Step-Down Units numbers
#' from calculation:
#' \itemize{
#'  \item{90 : }{Step-Down Medical Unit}
#'  \item{93 : }{Combined Medical/Surgical Step-Down Unit}
#'  \item{95 : }{Step-Down Surgical Unit}
#'  \item{99 : }{No SCU}
#' }
#'
#' Please refer to the CIHI DAD abstracting manual for more details.
#'
#' @param ipadmdad (`data.table`, `data.frame`)\cr
#' Table equivalent to the `admdad` table defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Table must contain fields:
#' GEMINI Encounter ID (`genc_id`) and admission time (`admission_date_time`).
#'
#' @param ipscu (`data.table`, `data.frame`)\cr
#' Table equivalent to the `ipscu` table defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#' Table must contain fields:
#' GEMINI Encounter ID (`genc_id`), SCU admission time (`scu_admit_date_time`), and SCU number (`scu_unit_number`).
#'
#' @param window (`integer`, `vector`)\cr
#' Time window of ICU entry since hospital admission, in hours.
#' By default, `window = c(24, 48, 72)` to calculate ICU entry within 24, 48 or 72 hours since hospital admission.
#'
#' @return (`data.table`)\cr
#' By default, for each encounter in input "ipadmdad" returns the corresponding derived boolean (TRUE/FALSE) fields
#' "icu_entry_derived", "icu_entry_in_24hr_derived", "icu_entry_in_48hr_derived" and "icu_entry_in_72hr_derived".
#' If user specified time window x hour is used, field "icu_entry_in_xhr_derived" is computed in addition to "icu_entry_derived".
#'
#' @note:
#' By design, function will not return any NA values.
#' When one tries to left-join the output of this function to another table,
#' make sure the list of encounters aligns in both tables
#'
#' @export
#'
#' @examples
#' # Default time window 24, 48, 72 hours:
#' \dontrun{
#' icu_entry (ipadmdad, ipscu)
#' }
#'
#' # User specified time window:
#' \dontrun{
#' icu_entry (ipadmdad, ipscu, window=12)
#' }
#'

icu_entry <- function(ipadmdad, ipscu, window = c(24, 48, 72)) {
  ###### Check user inputs ######

  ## table provided as data.frame/data.table
  if (!any(class(ipadmdad) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for ipadmdad. Please provide a data.frame or a data.table.")
  }

  if (!any(class(ipscu) %in% c("data.frame", "data.table"))) {
    stop("Invalid user input for ipscu Please provide a data.frame or a data.table.")
  }

  ## table contains required fields
  if (any(!c("genc_id", "admission_date_time") %in% names(ipadmdad))) {
    stop("Input ipadmdad table is missing at least one of the required variables.
          Refer to function documentation for details.")
  }

  if (any(!c("genc_id", "scu_admit_date_time", "scu_unit_number") %in% names(ipscu))) {
    stop("Input ipscu table is missing at least one of the required variables.
          Refer to function documentation for details.")
  }

  ###### Prepare data ######

  ## coerce tables to data.table as function uses data.table syntax
  ipadmdad <- coerce_to_datatable(ipadmdad)
  ipscu <- coerce_to_datatable(ipscu)

  ## subset and format fields used for calculation
  res <- ipadmdad[, .(genc_id, admission_date_time = lubridate::ymd_hm(admission_date_time))]
  ipscu <- ipscu[, .(genc_id, scu_admit_date_time = lubridate::ymd_hm(scu_admit_date_time), scu_unit_number)]

  ## filter out step-down units as they are not considered as icus, and merge in admission date time
  ipscu <-
    ipscu %>%
    dplyr::mutate(across(where(is.character), na_if, "")) %>%
    .[!trimws(as.character(scu_unit_number)) %in% c("90", "93", "95", "99")] %>%
    .[, .(genc_id, scu_admit_date_time)] %>%
    dplyr::left_join(res, by = "genc_id")

  ###### Derive ICU entry fields ######

  ## derive ICU entry at each time point
  res[, icu_entry_derived := ifelse(genc_id %in% ipscu$genc_id, TRUE, FALSE)] # icu entry at any time

  ## derive ICU entry within a specified time window
  lapply(window, function(x) {
    res[, paste0("icu_entry_in_", x, "hr_derived") :=
      ifelse(genc_id %in% ipscu[scu_admit_date_time <= (admission_date_time + lubridate::hours(x)), genc_id], TRUE, FALSE)]
  })

  ###### Clean up output ######

  ## remove irrelevant field from output
  res[, admission_date_time := NULL][]

  return(res)
}
