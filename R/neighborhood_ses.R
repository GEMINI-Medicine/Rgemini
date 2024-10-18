#' @title
#' Obtain commonly used neighbourhood-level socioeconomic (SES) variables
#'
#' @description
#' The `neighborhood_ses()` function derives neighborhood-level SES variables based on the dissemination area a given encounter resides in, using the Postal Code OM Conversion File Plus (PCCF+) program.
#'
#' @section Statistics Canada Census of Population
#' All SES variables are sourced from Statistics Canada Census. The Census of Population is collected every 5 years by Statistics Canada, aiming to create a comprehensive portrait of Canada.
#'
#' The census data was collected via short-form (75% households) and long-form (25% households) questionnaire, linked to administrative data.
#'
#' - Income was sourced from Canadian Revenue Agency.
#' - Education and visible minority were collected from long-form questionnaire.
#' – Immigration status was previously collected from long-form questionnaire in 2016. Starting in 2021, this information was sourced from Immigration, Refugees and Citizenship Canada in 2021.
#'
#' @section Ontario Marginalization Index (On-Marg)
#' On-Marg is a neighborhood-level index showing marginalization differences between areas. It’s created using a portion of Statistics Canada Census variables.
#'
#' Based on theoretical framework, 42 Census variables are selected. After principal component factor analysis, those variables are reduced to 18 indicators used to create 4 ON-Marg dimensions. Each dimension is available in quintiles (Q) – Q1 represents the least marginalization, while Q5 represents the most marginalization.
#'
#' For On-Marg 2016, 4 dimensions are: residential instability, material deprivation, dependency, ethnic concentration.
#' - residential instability captures family structure and housing densities
#' - material deprivation refers to inability to fulfill basic material needs
#' - dependency entails people without employment income
#' - ethnic concentration refers to recent immigrants or visible minorities
#'
#' For On-Marg 2021, 4 dimensions are: households and dwellings, material resources, age and labour force, racialized and newcomer populations.
#' - households and dwellings measures cohesiveness of family and community.
#' - material resources refers to access barriers to basic materials
#' - age and labour force records impacts of unemployment and disability
#' - racialized and newcomer populations refers to people who are newcomers, non-white, or non-Indigenous
#'
#' There was a name change in 2021 to avoid deficit-based language and better reflect the census measures associated with each dimension
#'
#' Summary score can be used if all dimensions are in the same direction.
#'
#'
#' @section Missing data
#' This function reports both the reason and percentage of missingness.
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param census_year (`numeric` | `character`)\cr
#' Version of census_year chosen. Only 2016 or 2021 are valid inputs.
#'
#' @param cohort (`data.table` | `data.frame`)\cr
#' Table with all relevant encounters of interest, where each row corresponds to
#' a single encounter. Must contain GEMINI Encounter ID (`genc_id`).
#'
#' @return (`data.frame` | `data.table`)\cr
#' This function returns a data.table where each row corresponds to a genc_id from the user-provided cohort input, together with the following columns:
#' - DA (dissemination area) ID from the corresponding census year: da16uid or da21uid. DA is ['a small area composed of one or more neighbouring dissemination blocks and is the smallest standard geographic area for which all census data are disseminated.'](https://www150.statcan.gc.ca/n1/en/catalogue/92-169-X)
#'
#' - Neighbourhood-level income: qnatippe, qnbtippe, qaatippe, qabtippe, atippe, btippe. While qnatippe and qnbtippe are quintiles calculated based on national income distribution, qaatippe and qabtippe are quintiles constructed separately for each census metropolitan area (CMA), census agglomeration (CA) or residual area within each province. Atippe, btippe are numeric.
#' - [Visible minority](https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/pop127-eng.cfm): vismin_pct
#' - [Immigration status](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?LANG=E&GENDERlist=1,2,3&STATISTIClist=4&HEADERlist=23&SearchText=Canada&DGUIDlist=2021A000011124): immsta_pct
#' - [Post-secondary education (>15)](https://www150.statcan.gc.ca/n1/pub/81-004-x/2010001/def/posteducation-educpost-eng.htm): ed_15over_postsec_pct
#' - Post-secondary education (25-64): ed_25to64_postsec_pct
#' - Ontario Marginalization Index (numeric): (instability_da16, deprivation_da16, dependency_da16, ethniccon_da16) or (households_dwellings_DA21, material_resources_DA21, age_labourforce_DA21, racialized_NC_pop_DA21)
#' - Ontario Marginalization Index (quintile): (instability_q_da16, deprivation_q_da16, dependency_q_da16, ethniccon_q_da16) or (households_dwellings_q_DA21, material_resources_q_DA21, age_labourforce_q_DA21, racialized_NC_pop_q_DA21)
#'
#' @references
#' [ON-Marg guide 2016](https://www.publichealthontario.ca/-/media/documents/U/2018/userguide-on-marg.pdf)
#' [ON-Marg guide 2021](https://www.publichealthontario.ca/-/media/Event-Presentations/2023/09/ontario-marginalization-index-updates-products.pdf?rev=07baae2569164c17abaa18464075aa20&sc_lang=en)
#' [Statistics Canada Census.](https://www12.statcan.gc.ca/census-recensement/2021/ref/98-304/2021001/chap1-eng.cfm)
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
#' neighborhood_ses_table <- neighborhood_ses(dbcon, cohort, 2016)
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
