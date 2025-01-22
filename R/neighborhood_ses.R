#' @title
#' Obtain commonly used neighbourhood-level socioeconomic status (SES) variables
#'
#' @description
#' The `neighborhood_ses()` function derives neighborhood-level SES variables for a given encounter based on the dissemination area they reside in. All variables returned by this function are based on Statistics Canada Census data and the Ontario Marginalization Index (ON-Marg).
#' 
#' For database versions since `drm_cleandb_v3` / `H4H_template_v4` users can choose between 2016 vs. 2021 census/ON-Marg data. Note that some of the function output returned by this function the function output differs based on the census year (2016 or 2021) provided by the user.
#'
#' @section Statistics Canada Census:
#' All SES variables are sourced from the Statistics Canada Census. The census is collected every 5 years and provides a detailed statistical portrait of communities across Canada, including information about income, education, ethnicity, and immigrant status.
#' 
#' The function currently returns the following census variables:
#' 
#' - **Household income**:
#'  - Sourced from the Canadian Revenue Agency
#'  - Both continuous income and national/community quintiles are provided
#' - **Education**:
#'  - Based on the long-form census questionnaire, which is only administered to 25% of households
#' - [**Visible minorities**](https://www12.statcan.gc.ca/census-recensement/2021/ref/98-500/006/98-500-x2021006-eng.cfm):
#'  - Indicates whether a person identifies as a visible minority as defined by the Employment Equity Act:
#' “persons, other than Aboriginal peoples, who are non-Caucasian in race or non-white in colour” (e.g.,
#' Black, South Asian, Chinese, Latin American etc.)
#'  - Based on the long-form census questionnaire, which is only administered to 25% of households
#' - **Immigrant status**:
#'  - In 2016 census: Based on the long-form questionnaire (25% of households)
#'  - Since 2021: Sourced from Immigration, Refugees and Citizenship Canada
#'
#' All census data are collected by dissemination area (DA), which typically covers a population of 400-700 people. To enable linkage between GEMINI data and DA-level information, the DA of a given encounter was derived from their postal code using the [Postal Code Conversion File Plus (PCCF+)](https://www150.statcan.gc.ca/n1/en/catalogue/92-154-X) program.
#' 
#' @section Ontario Marginalization Index (On-Marg):
#' On-Marg is a neighborhood-level index measuring marginalization differences between areas based on a subset of 42 variables from the Statistics Canada census.
#'  
#' The index was derived from a principal component factor analysis, which resulted in 18 indicators along the following 4 dimensions:
#'  
#' 1) Households and dwellings: Measures housing density and characteristics of family structure (e.g., living alone, % dwellings not owned)
#' 2) Material resources: Measures access to basic material needs (e.g., employment rate)
#' 3) Age and labour force: Includes indicators such as the % of seniors (65+), children, and those that are not part of the labour force 
#' 4) Racialized and newcomer populations: Measures the % of people who are newcomers or identify as a visible minority
#' 
#' In the 2016 version of ON-Marg, the 4 dimensions were called "Residential instability", "Material deprivation", "Dependency", and "Ethnic concentration" respectively. The dimensions were renamed in 2021 to avoid deficit-based language and better reflect the census measures associated with each dimension. 
#'
#' All ON-Marg variables are available as continuous factor scores as well as quintiles, where higher values represent a higher degree of marginalization. 
#'
#' @section Missing values:
#' Some encounters could not be linked to Statistics Canada data due to missing/invalid postal codes, or due to the fact that they reside in an area not covereded by the census. These encounters will be returned with `dauid = NA`
#' Additionally, Statistics Canada suppresses results from certain DAs due to low response rates or data quality issues. The corresponding census/ON-Marg variables will be returned as `NA` for all `genc_ids` in that DA.
#' 
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param census_year (`numeric` | `character`)\cr
#' Statistics Canada census year. Only 2016 or 2021 are valid inputs.
#'
#' @param cohort (`data.table` | `data.frame`)\cr
#' Table with all relevant encounters of interest, where each row corresponds to
#' a single encounter. Must contain GEMINI Encounter ID (`genc_id`).
#'
#' @return (`data.frame` | `data.table`)\cr
#' This function returns a `data.table`` where each row corresponds to a `genc_id` from the user-provided cohort input, together with the following columns:
#' 
#' - DA the encounter resides in: `dauid`
#' - Neighbourhood-level income (continuous):
#'  - `atippe` (neighbourhood after tax income per single person equivalent)
#'  - `btippe` (neighbourhood before tax income per single person equivalent)
#' - Neighbourhood-level income (quintiles from PCCF+):
#'  - `qnatippe` and `qnbtippe`: Quintiles of `atippe` and `btippe` calculated based on *national* income distribution
#'  - `qaatippe` and `qabtippe`: Quintiles of `atippe` and `btippe` calculated based on distribution within a given community (based on census metropolitan area, census agglomeration, or residual area within each province).
#' - % visible minorities: `vismin_pct`
#' - % immigrants: `immsta_pct`
#' - % with post-secondary education:
#'  - Including all respondents > 15 years of age: `ed_15over_postsec_pct`
#'  - Only including respondents between 25-64 years: `ed_25to64_postsec_pct`
#' - Ontario Marginalization Index (continuous):
#'  - If `census_year` = 2021: `households_dwellings`, `material_resources`, `age_labourforce`, `racialized_NC_pop`
#'  - If `census_year` = 2016: `instability`, `deprivation`, `dependency`, `ethniccon`
#' - Ontario Marginalization Index (quintiles):
#'  - All ON-Marg variables are additionally returned as quintiles, as indicated by the suffix `_q` (e.g., `households_dwellings_q`)
#'
#' @references
#' [Statistics Canada Census.](https://www12.statcan.gc.ca/census-recensement/2021/ref/98-304/2021001/chap1-eng.cfm)
#' [ON-Marg guide 2021](https://www.publichealthontario.ca/-/media/Event-Presentations/2023/09/ontario-marginalization-index-updates-products.pdf?rev=07baae2569164c17abaa18464075aa20&sc_lang=en)
#' [ON-Marg guide 2016](https://www.publichealthontario.ca/-/media/documents/U/2018/userguide-on-marg.pdf)
#' [Measuring Health Inequalities: A Toolkit Area-Level Equity Stratifiers Using PCCF and PCCF+](https://www.cihi.ca/sites/default/files/document/toolkit-area-level-measurement-pccf-en.pdf)
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
#' neighborhood_ses_table <- neighborhood_ses(dbcon, cohort, 2021)
#' }
#'
neighborhood_ses <- function(dbcon, cohort, census_year) {
  check_input(cohort, c("data.table", "data.frame"), colnames = "genc_id")
  cohort <- coerce_to_datatable(cohort)

  check_input(dbcon, argtype = "DBI")
  ## write a temp table to improve querying efficiency
  DBI::dbSendQuery(dbcon, "Drop table if exists temp_data;")
  DBI::dbWriteTable(dbcon, c("pg_temp", "temp_data"), cohort[, .(genc_id)], row.names = F, overwrite = T)

  ## only da=2016, 2021 are allowed values
  check_input(census_year, c("numeric", "character"), categories = c(2016, 2021))

  substrRight <- function(x, n) {
    substr(x, nchar(x) - n + 1, nchar(x))
  }

  ### check which DB
  tn <- dbListTables(dbcon)
  tnl <- grepl("lookup_statcan_v", tn) %>%
    unique() %>%
    length()
  if (census_year == 2016) {
    if (tnl == 2) {
      var_tbl <- DBI::dbGetQuery(dbcon, "select tmp.genc_id, l.da16uid, s.c16_vismin, s.c16_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c16_immsta, s.c16_immsta_imm, s.c16_ed_15over_postsec, s.c16_ed_15over, s.c16_ed_25to64_postsec, s.c16_ed_25to64, s.instability_da16, s.deprivation_da16, s.dependency_da16, s.ethniccon_da16, s.instability_q_da16, s.deprivation_q_da16, s.dependency_q_da16, s.ethniccon_q_da16

                                          from temp_data tmp

                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan_v2016 s
                                            on l.da16uid = s.da16uid; ") %>% as.data.table()
    }

    if (tnl == 1) {
      var_tbl <- DBI::dbGetQuery(dbcon, "select tmp.genc_id, l.da16uid, s.c16_vismin, s.c16_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c16_immsta, s.c16_immsta_imm, s.c16_ed_15over_postsec, s.c16_ed_15over, s.c16_ed_25to64_postsec, s.c16_ed_25to64, s.instability_da16, s.deprivation_da16, s.dependency_da16, s.ethniccon_da16, s.instability_q_da16, s.deprivation_q_da16, s.dependency_q_da16, s.ethniccon_q_da16

                                          from temp_data tmp

                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan s
                                            on l.da16uid = s.da16uid; ") %>% as.data.table()
    }
  }

  if (census_year == 2021) {
    if (tnl == 1) {
      warning("Your DB version only contains 2016 census data, please change your input to `census_year = 2016`")
    } else {
      var_tbl <- DBI::dbGetQuery(dbcon, 'select tmp.genc_id, l.da21uid, s.c21_vismin, s.c21_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c21_immsta, s.c21_immsta_imm, s.c21_ed_15over_postsec, s.c21_ed_15over, s.c21_ed_25to64_postsec, s.c21_ed_25to64, "households_dwellings_DA21", "material_resources_DA21", "age_labourforce_DA21", "racialized_NC_pop_DA21", "households_dwellings_q_DA21", "material_resources_q_DA21", "age_labourforce_q_DA21", "racialized_NC_pop_q_DA21"

                                          from temp_data tmp

                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan_v2021 s
                                            on l.da21uid = s.da21uid; ') %>% as.data.table()
    }
  }

  # change all invalid income values to NA
  cols_to_modify <- c("qnatippe", "qnbtippe", "qaatippe", "qabtippe", "atippe", "btippe")

  # Replace 9s with NA in the specified columns
  var_tbl[, (cols_to_modify) := lapply(.SD, function(x) fifelse(x %in% c(9, 99999999, 999, 999999), NA, x)), .SDcols = cols_to_modify]
  ## cal %
  yr <- substring(census_year, 3, 4)

  # cal % visible minority
  var_tbl[, vm := round(get(paste0("c", yr, "_vismin")) / (get(paste0("c", yr, "_vismin")) + get(paste0("c", yr, "_vismin_not"))) * 100, 2)]

  # cal % immigration status
  var_tbl[, im := round(get(paste0("c", yr, "_immsta_imm")) / (get(paste0("c", yr, "_immsta"))) * 100, 2)]

  # cal % post-secondary education (>15)
  var_tbl[, ed15 := round(get(paste0("c", yr, "_ed_15over_postsec")) / (get(paste0("c", yr, "_ed_15over"))) * 100, 2)]

  # cal % post-secondary education (25-64)
  var_tbl[, ed25 := round(get(paste0("c", yr, "_ed_25to64_postsec")) / (get(paste0("c", yr, "_ed_25to64"))) * 100, 2)]

  var_tbl[, dalast := substrRight(get(paste0("da", yr, "uid")), 4)]

  da_miss <- var_tbl[is.na(get(paste0("da", yr, "uid"))) | dalast == 9999]

  da_exist <- var_tbl[!is.na(get(paste0("da", yr, "uid")))]
  nar <- rowSums(is.na(da_exist))

  p1 <- nrow(da_miss) / nrow(var_tbl) * 100
  p2 <- length(nar[nar > 0]) / nrow(da_exist) * 100

  message(paste0(round(p1, 2), "% encounters can't be linked due to missing/invalid postal code information"))
  message(paste0("Among encounters with valid DA, ", round(p2, 2), "% encounters have at least 1 missing SES variable due to no census or On-Marg data. Please carefully check the % of NA for the variables of interest"))

  ## clean up colns
  coln_exclude <- c(
    paste0("c", yr, "_vismin"), paste0("c", yr, "_vismin_not"),
    paste0("c", yr, "_immsta_imm"), paste0("c", yr, "_immsta"),
    paste0("c", yr, "_ed_15over_postsec"), paste0("c", yr, "_ed_15over"),
    paste0("c", yr, "_ed_25to64_postsec"), paste0("c", yr, "_ed_25to64"),
    "dalast"
  )

  coln_keep <- setdiff(names(var_tbl), coln_exclude)

  rec <- var_tbl %>%
    select(all_of(coln_keep))

  # change to final names
  setnames(rec, "vm", "vismin_pct")
  setnames(rec, "im", "immsta_pct")
  setnames(rec, "ed15", "ed_15over_postsec_pct")
  setnames(rec, "ed25", "ed_25to64_postsec_pct")

  return(rec)
}


