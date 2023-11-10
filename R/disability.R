#' @title
#' Identify encounters with disability
#'
#' @description
#' This function identifies whether or not a `genc_id` has any ICD-10-CA diagnosis
#' code(s) indicating a physical, sensory, and/or cognitive disability. Diagnosis codes
#' that are included as disabilities are based on
#' [Brown et al., 2022](https://www.cmaj.ca/content/194/4/E112).
#'
#' Physical disabilities include
#'
#' Sensory disabilities include
#'
#' Cognitive disabilities refer to developmental
#'
#' @details
#' This function takes a `cohort` and `dxtable` data input and returns a logical
#' output indicating whether a given `genc_id` was diagnosed with a physical,
#' sensory, and/or cognitive disability.
#'
#' A list of all ICD-10-CA diagnosis codes that are classified as physical/sensory/cognitive disability can be
#' found in Table 1 of the paper by [Brown et al., 2022](https://www.cmaj.ca/content/194/4/E112).
#' All codes listed in this table, plus any children codes are considered by this function.
#'
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest, where each row
#' corresponds to a single encounter. Must contain GEMINI Encounter ID
#' (`genc_id`).
#'
#' @param dxtable (`data.frame` or `data.table`)
#' Table containing ICD-10-CA diagnosis codes for each encounter in long format.
#' Must contain the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `diagnosis_code` (`character`): alphanumeric ICD-10-CA diagnosis code consisting of 3-7 characters.
#'
#' Typically, this input corresponds to the `ipdiagnosis` table, which
#' contains the CIHI in-patient diagnoses for each encounter (see
#' [GEMINI database schema](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#' However, users may also include Emergency Department diagnoses (`erdiagnosis` table) by combining
#' `ipdiagnosis` and `erdiagnosis` table prior to running this function.
#'
#' This function does not differentiate between diagnosis types. That is, diagnosis codes are included
#' regardless of whether the diagnosis was coded as a most responsible diagnosis, a comorbidity, or secondary diagnosis. If users wish to only include
#' certain diagnosis types, please filter the `dxtable` input based on `diagnosis_type` prior to running this function (for more
#' details, see [CIHI diagnosis type definitions](https://www.cihi.ca/sites/default/files/document/diagnosis-type-definitions-en.pdf).
#'
#' @return `data.table`
#' This function returns a table with all encounters identified by the `cohort` table input and
#' additional derived logical fields ``.
#' If no diagnosis code referring to any disablility was found...
#' If a `genc_id` does not have any entry in the diagnosis table at all, `disability = NA`. Note that this is very rare when including the
#' full `ipdiagnosis` table (i.e., if no additional filtering was performed, > 99.9% of `genc_ids` should have at least 1 diagnosis code).
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'                         dbname = "db",
#'                         host = "172.XX.XX.XXX",
#'                         port = 1234,
#'                         user = getPass("Enter user:"),
#'                         password = getPass("password"))
#'
#' ipadmdad <- dbGetQuery(dbcon, "select * from admdad") %>% data.table()
#' ipdiagnosis <- dbGetQuery(dbcon, "select * from ipdiagnosis") %>% data.table()
#'
#' disability(cohort = ipadmdad, dxtable = ipdiagnosis)
#' }
#'
#' @references
#' Brown, H. K., Saha, S., Chan, T. C., Cheung, A. M., Fralick, M., Ghassemi, M., ... & Verma, A. A. (2022).
#' Outcomes in patients with and without disability admitted to hospital with COVID-19: a retrospective cohort study.
#' Cmaj, 194(4), E112-E121.
#'
#' @import stringr
#' @export
#'
disability <- function(cohort, dxtable) {

  ############# CHECK & PREPARE DATA #############
  ## check that cohort contains genc_ids

  ## check that dxtable contains genc_id & diagnosis_code

  ## Prepare output - should have 1 row per genc_id in cohort file
  res <- cohort %>% select(genc_id) %>% distinct() %>% data.table()

  ## Ensure dxtable is in data.table format before proceeding
  dxtable <- coerce_to_datatable(dxtable[, c('genc_id', 'diagnosis_code')])


  ############# GET DISABILITY CODES #############
  ## Define disability diagnosis codes
  physical_disability <- str_c(paste0("^",
                                      c('Q675', 'Q66', 'Q676', 'Q677', 'Q678', 'E343', 'E230', 'Q019', 'Q02', 'Q03', 'Q04', 'Q06', 'Q078',
                                        'Q079', 'G901', 'Q75', 'Q76', 'Q77', 'Q78', 'Q79', 'Q72', 'Q73', 'Q74', 'Q71', 'Q05', 'Q70', 'E220',
                                        'M45', 'M46', 'M863', 'M864', 'M865', 'M866', 'M500', 'M502', 'M503', 'M504', 'M505', 'M506', 'M507',
                                        'M508', 'M509', 'M510', 'M512', 'M513', 'M514', 'M515', 'M516', 'M517', 'M518', 'M519', 'M224',
                                        'M232', 'M233', 'M234', 'M235', 'M238', 'M239', 'M15', 'M16', 'M17', 'M18', 'M19', 'M42', 'M91',
                                        'M92', 'M93', 'M87', 'M80', 'M353', 'M05', 'M06', 'M47', 'G80', 'G90', 'G40', 'G81', 'G60', 'G11',
                                        'G328', 'G57', 'G58', 'G35', 'G71', 'G72', 'G70', 'G54', 'G55', 'G36', 'G37', 'G95', 'G10', 'G23',
                                        'G241', 'G242', 'G243', 'G244', 'G245', 'G246', 'G247', 'G248', 'G249', 'G25', 'G82', 'G83', 'G61',
                                        'G62', 'G63', 'G318', 'G20', 'G21', 'I69', 'B91', 'G12', 'S77', 'S87', 'S970', 'T041', 'T043', 'T044',
                                        'T045', 'T046', 'T047', 'T048', 'Z993', 'Z998', 'S324', 'S325', 'S326', 'S327', 'S328', 'T912', 'S140',
                                        'S141', 'S240', 'S241', 'S340', 'S341', 'S343', 'T060', 'T061', 'T913', 'M218', 'S78', 'S88', 'S980',
                                        'S983', 'T05', 'Z894', 'Z895', 'Z896', 'Z897', 'Z898', 'S48', 'S58', 'S683', 'S684', 'Z891', 'Z892', 'Z893')),
                               collapse = '|')

  sensory_disability <- str_c(paste0("^",
                                     c('H90', 'H913', 'H918', 'H919', 'Q160', 'Q161', 'Q163', 'Q164', 'Q165', 'Q166', 'Q167', 'Q168',
                                       'Q169', 'H54', 'H25', 'H26', 'H30', 'H31', 'Q111', 'Q112', 'Q131', 'Q133', 'Q138', 'Q150',
                                       'H44', 'H201', 'H476', 'H40', 'H42', 'H55', 'E1031', 'E1032', 'E1033', 'E1034', 'E1035',
                                       'E1131', 'E1132', 'E1133', 'E1134', 'E1135', 'H34', 'H35', 'H36')),
                              collapse = '|')


  development_disability <- str_c(paste0("^",
                                         c('S020', 'S021', 'S023', 'S027', 'S028', 'S029', 'S061', 'S062', 'S063', 'S064', 'S065',
                                           'S066', 'S067', 'S068', 'S069', 'S07', 'T020', 'T905', 'F700', 'F701', 'F708', 'F709',
                                           'F710', 'F711', 'F718', 'F719', 'F720', 'F721', 'F728', 'F729', 'F730', 'F731', 'F738',
                                           'F739', 'F780', 'F781', 'F788', 'F789', 'F790', 'F791', 'F798', 'F799', 'F840', 'F841',
                                           'F843', 'F844', 'F845', 'F848', 'F849', 'Q851', 'Q860', 'Q861', 'Q871', 'Q8723', 'Q8731',
                                           'Q878', 'Q900', 'Q901', 'Q902', 'Q903', 'Q904', 'Q905', 'Q906', 'Q907', 'Q908', 'Q909',
                                           'Q910', 'Q911', 'Q912', 'Q913', 'Q914', 'Q915', 'Q916', 'Q917', 'Q918', 'Q919', 'Q920',
                                           'Q921', 'Q922', 'Q923', 'Q924', 'Q925', 'Q927', 'Q928', 'Q929', 'Q930', 'Q931', 'Q932',
                                           'Q933', 'Q934', 'Q935', 'Q936', 'Q937', 'Q938', 'Q939', 'Q971', 'Q992', 'Q998')),
                                  collapse = '|')



  ## Create flags
  res[, physical_disability := genc_id %in% dxtable[str_detect(diagnosis_code, physical_disability),]$genc_id]
  res[, sensory_disability := genc_id %in% dxtable[str_detect(diagnosis_code, sensory_disability),]$genc_id]
  res[, development_disability := genc_id %in% dxtable[str_detect(diagnosis_code, development_disability),]$genc_id]


  ## Create single flag indicating whether any disability
  res[disability := physical_disability | sensory_disability | development_disability]


  ## For any genc_ids with no entry in diagnosis table, set all flags to NA
  #res[genc_id %ni% dxtable$genc_id, ':=' .(disability, physical_disability, sensory_disability, development_disability) := NA]


  return(res[ ,.(genc_id, disability, physical_disability, sensory_disability, development_disability)])
}
