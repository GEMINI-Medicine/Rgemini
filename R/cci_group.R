#' @title
#' Grouping CCI intervention codes
#'
#' @description
#' The Canadian Classification of Health Interventions (CCI) codes provide a
#' detailed classification of all in-patient interventions in Canada with more
#' than 17,000 unique codes.
#'
#' The `cci_group()` function categorizes CCI codes into broader, clinically
#' meaningful intervention categories based on each code's first 3 characters.
#' 
#' Broadly, intervention codes can be grouped into the following CCI Sections
#' based on the first digit:
#' - 1: Physical/Physiological Therapeutic Interventions (1AA - 1ZZ)
#' - 2: Diagnostic Interventions (2AA - 2ZZ)
#' - 3: Diagnostic Imaging Interventions (3AF - 3ZZ)
#' - 5: Obstetrical and Fetal Interventions (5AB - 5PD)
#' - 6: Cognitive, Psychosocial and Sensory Therapeutic Interventions (6AA - 6VA)
#' - 7: Other Healthcare Interventions (7SC - 7SJ)
#' - 8: Therapeutic Interventions Strengthening the Immune System and/or Genetic Composition (8AA - 8ZZ)
#'
#' Each section can further be broken down into more detailed subsections, based
#' on the CCI group (2nd-3rd character).For example, CCI codes in Sections 1-3
#' can be differentiated by anatomy region, such as:
#' - Therapeutic Interventions on the Nervous System (1AA - 1BZ)
#' - Therapeutic Interventions on the Eye and Ocular Adnexa (1CC - 1CZ)
#' - Therapeutic Interventions on the Ear and Mastoid (1DA - 1DZ)
#' - etc.
#' 
#' For a complete list of all subsections, see
#' [CIHI CCI sections and code ranges](https://www.cihi.ca/en/overview-of-cci-sections-and-code-ranges).
#' 
#' @param cci_codes (`data.frame` | `data.table`)
#' Table containing `intervention_code` column that lists all CCI intervention
#' codes of interest (e.g., from the `ipintervention` or `erintervention` table).
#'
#' @returns `data.table`
#' Data table containing the CCI codes of interest, together with their
#' corresponding groupings as follows:
#' - `section` (`numeric`): CCI section number (based on first character)
#' - `section_descr` (`character`): Section description
#' - `subsection` (`character`): CCI subsection (first 3 characters)
#' - `subsection_descr` (`character`): Subsection description
#'
#' @examples
#' cci_codes <- data.table(intervention_code = c(
#'     "1MA52HA", "2PB70DA", "3SC10KM", "5LB08ZZ", "6KA02ME", "8AA70BABA"
#'     ))
#' res <- cci_group(cci_codes)
#' 
#' @references
#' https://www.cihi.ca/en/overview-of-cci-sections-and-code-ranges
#' 
#' @export
#' 
cci_group <- function(cci_codes) {

  ## prepare data
  # check input
  check_input(
    cci_codes, c("data.table", "data.frame"),
    colnames = "intervention_code", coltypes = "character"
  )
  cci_grouped <- cci_codes %>% data.table()

  # remove any special characters from intervention_codes
  cci_grouped[, intervention_code := gsub("[^A-Za-z0-9]", "", intervention_code)]

  # read mapping file
  mapping_cci <- Rgemini::mapping_cci %>%
    data.table()
  # remove any leading or trailing white spaces
  mapping_cci[] <- lapply(mapping_cci, function(x) {
    if (is.character(x)) trimws(x) else x
  })
  
  ## add CCI section (based on first number)
  cci_grouped[, section := as.numeric(substr(intervention_code, 1, 1))]
  cci_grouped[, section := ifelse(section %in% c(1, 2, 3, 5, 6, 7, 8), section, NA)]
  cci_grouped <- merge(
    cci_grouped,
    mapping_cci[field == "Section", .(idx = as.numeric(idx), description)],
    by.x = "section", by.y = "idx", all.x = TRUE, sort = FALSE
  )
  setnames(cci_grouped, "description", "section_descr")
  
  ## add CCI subsection (based on first 3 characters)
  cci_grouped[, subsection := substr(intervention_code, 1, 3)]
  mapping_cci[field == "Group", subsection := paste0(section, idx)]
  cci_grouped <- merge(
    cci_grouped,
    mapping_cci[field == "Group", c("subsection", "subsection_descr")],
    by = "subsection", all.x = TRUE, sort = FALSE
  )
  
  # clean up final output
  cci_grouped <- cci_grouped %>%
    select(intervention_code, section, section_descr, subsection, subsection_descr, everything()) %>%
    data.table()
  cci_grouped[cci_grouped == ""] <- NA
  
  return(cci_grouped)
}
