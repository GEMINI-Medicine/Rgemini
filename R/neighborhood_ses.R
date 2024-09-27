#' @title
#' Obtain commonly used neighbourhood-level socioeconomic (SES) variables
#'
#' @description
#' The `neighborhood_ses()` function merges SES variables based on dissemination area ID. Function descriptions contains the necessary understanding of variable concepts and corresponding application recommendations.
#'
#' @section Data source
#' All SES variables are sourced from [Statistics Canada Census.](https://www12.statcan.gc.ca/census-recensement/2021/ref/98-304/2021001/chap1-eng.cfm)
#'
#' The Census of Population provides a detailed and comprehensive statistical portrait of Canada. It’s collected by Statistics Canada conducts once every 5 years, aiming to provide population and dwelling counts for communities of all sizes across Canada.
#'
#' On-Marg is a neighborhood-level index showing marginalization differences between areas. It’s created using a portion of Statistics Canada Census variables.
#' On-Marg has 4 dimensions. There was a name change in 2021 to avoid deficit-based language and better reflect the census measures associated with each dimension
#' Summary score can be used if all dimensions are in the same direction. If >1 dimensions are in opposite direction, it’s recommended to report those dimensions separately.
#'
#' @section Income additional details
#' Based on theoretical framework, 42 Census variables are selected. After principal component factor analysis, 18 Census variables are used to create 4 ON-Marg dimensions. Each dimension is available in quintiles – Q1 represents the least marginalization, while Q5 represents the most marginalization.
#'
#' @section Missing data
#' This function automatically excludes encounters with missing data and report both the reason and percentage of missingness exclusion
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param census_year (`numeric` | `character`)\cr
#' Version of census_year chosen. Only 2016 or 2021 are valid inputs.
#'
#' @param cohort\cr
#' Table with all relevant encounters of interest, where each row corresponds to
#' a single encounter. Must contain GEMINI Encounter ID (`genc_id`).
#'
#' @return (`data.frame` | `data.table`)\cr
#' This function returns a data.table where each row corresponds to a genc_id from the user-provided cohort input, together with the following columns:
#' - DA (dissemination area) ID from the corresponding census year: da16uid or da21uid. DA is ['a small area composed of one or more neighbouring dissemination blocks and is the smallest standard geographic area for which all census data are disseminated.'](https://www150.statcan.gc.ca/n1/en/catalogue/92-169-X)
#' - [Visible minority](https://www12.statcan.gc.ca/census-recensement/2016/ref/dict/pop127-eng.cfm): c16_vismin_pct or c21_vismin_pct
#' - [Neighbourhood-level income](https://www.cihi.ca/sites/default/files/document/toolkit-area-level-measurement-pccf-en.pdf): qnatippe, qnbtippe, qaatippe, qabtippe, atippe, btippe. While qnatippe and qnbtippe are quintiles calculated based on national income distribution, while qaatippe and qabtippe are quintiles constructed separately for each census metropolitan area (CMA), census agglomeration (CA) or residual area within each province.
#' - [Immigration status](https://www12.statcan.gc.ca/census-recensement/2021/dp-pd/prof/details/page.cfm?LANG=E&GENDERlist=1,2,3&STATISTIClist=4&HEADERlist=23&SearchText=Canada&DGUIDlist=2021A000011124): c16_immsta_pct or c21_immsta_pct.
#' - [Post-secondary education (>15)](https://www150.statcan.gc.ca/n1/pub/81-004-x/2010001/def/posteducation-educpost-eng.htm): c16_ed_15over_postsec_pct or c21_ed_15over_postsec_pct.
#' - Post-secondary education (25-64): c16_ed_25to64_postsec_pct or c21_ed_25to64_postsec_pct
#' - Ontario Marginalization Index (numeric): (instability_da16, deprivation_da16, dependency_da16, ethniccon_da16) or (households_dwellings_DA21, material_resources_DA21, age_labourforce_DA21, racialized_NC_pop_DA21)
#' - Ontario Marginalization Index (quintile): (instability_q_da16, deprivation_q_da16, dependency_q_da16, ethniccon_q_da16) or (households_dwellings_q_DA21, material_resources_q_DA21, age_labourforce_q_DA21, racialized_NC_pop_q_DA21)
#'
#' @references
#' [ON-Marg guide 2016](https://www.publichealthontario.ca/-/media/documents/U/2018/userguide-on-marg.pdf)
#' [ON-Marg guide 2021](https://www.publichealthontario.ca/-/media/Event-Presentations/2023/09/ontario-marginalization-index-updates-products.pdf?rev=07baae2569164c17abaa18464075aa20&sc_lang=en)
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
  check_input(dbcon, argtype = "DBI")
  ## write a temp table to improve querying efficiency
  DBI::dbSendQuery(dbcon, "Drop table if exists temp_data;")
  DBI::dbWriteTable(dbcon, c("pg_temp", "temp_data"), cohort[, .(genc_id)], row.names = F, overwrite = T)

  ## only da=2016, 2021 are allowed values
  check_input(census_year, c("numeric", "character"), categories = c(2016, 2021))

  ### check which DB
  tn <- dbListTables(dbcon)
  tnl <- grepl("lookup_statcan_v", tn) %>%
    unique() %>%
    length()
  if (census_year == 2016 & tnl > 1) {
    var_tbl <- DBI::dbGetQuery(dbcon, "select tmp.genc_id, l.da16uid, s.c16_vismin, s.c16_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c16_immsta, s.c16_immsta_imm, s.c16_ed_15over_postsec, s.c16_ed_15over, s.c16_ed_25to64_postsec, s.c16_ed_25to64, s.instability_da16, s.deprivation_da16, s.dependency_da16, s.ethniccon_da16, s.instability_q_da16, s.deprivation_q_da16, s.dependency_q_da16, s.ethniccon_q_da16

                                          from temp_data tmp

                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan_v2016 s
                                            on l.da16uid = s.da16uid; ") %>% as.data.table()
  }

  if (census_year == 2016 & tnl == 1) {
    var_tbl <- DBI::dbGetQuery(dbcon, "select tmp.genc_id, l.da16uid, s.c16_vismin, s.c16_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c16_immsta, s.c16_immsta_imm, s.c16_ed_15over_postsec, s.c16_ed_15over, s.c16_ed_25to64_postsec, s.c16_ed_25to64, s.instability_da16, s.deprivation_da16, s.dependency_da16, s.ethniccon_da16, s.instability_q_da16, s.deprivation_q_da16, s.dependency_q_da16, s.ethniccon_q_da16

                                          from temp_data tmp

                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan s
                                            on l.da16uid = s.da16uid; ") %>% as.data.table()
  }

  if (census_year == 2021 & tnl > 1) {
    var_tbl <- DBI::dbGetQuery(dbcon, 'select tmp.genc_id, l.da21uid, s.c21_vismin, s.c21_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c21_immsta, s.c21_immsta_imm, s.c21_ed_15over_postsec, s.c21_ed_15over, s.c21_ed_25to64_postsec, s.c21_ed_25to64, "households_dwellings_DA21", "material_resources_DA21", "age_labourforce_DA21", "racialized_NC_pop_DA21", "households_dwellings_q_DA21", "material_resources_q_DA21", "age_labourforce_q_DA21", "racialized_NC_pop_q_DA21"

                                          from temp_data tmp

                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan_v2021 s
                                            on l.da21uid = s.da21uid; ') %>% as.data.table()
  }

  if (census_year != 2016 & tnl == 1) {
    warning("Your DB version only contains 2016 census data, please make census_year as 2016")
  }

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

  ## clean up colns
  coln_exclude <- c(
    paste0("c", yr, "_vismin"), paste0("c", yr, "_vismin_not"),
    paste0("c", yr, "_immsta_imm"), paste0("c", yr, "_immsta"),
    paste0("c", yr, "_ed_15over_postsec"), paste0("c", yr, "_ed_15over"),
    paste0("c", yr, "_ed_25to64_postsec"), paste0("c", yr, "_ed_25to64")
  )

  coln_keep <- setdiff(names(var_tbl), coln_exclude)

  rec <- var_tbl %>%
    select(all_of(coln_keep))

  # change to final names
  setnames(rec, "vm", paste0("c", yr, "_vismin_pct"))
  setnames(rec, "im", paste0("c", yr, "_immsta_pct"))
  setnames(rec, "ed15", paste0("c", yr, "_ed_15over_postsec_pct"))
  setnames(rec, "ed25", paste0("c", yr, "_ed_25to64_postsec_pct"))

  if (census_year == 2016) {
    # cal % exclusion for 2016 census data
    p1 <- nrow(rec[qnatippe == 9]) / nrow(rec)
    p2 <- nrow(rec[is.na(get(paste0("da", yr, "uid")))]) / nrow(rec)
    p3 <- nrow(rec[!is.na(get(paste0("da", yr, "uid"))) & (
      is.na(qnatippe) |
        is.na(instability_da16) |
        is.na(get(paste0("c", yr, "_vismin_pct"))) |
        is.na(get(paste0("c", yr, "_immsta_pct"))) |
        is.na(get(paste0("c", yr, "_ed_15over_postsec_pct"))) |
        is.na(get(paste0("c", yr, "_ed_25to64_postsec_pct"))))]) / nrow(rec)

    message(paste0(round(p1, 4), "% encounters are excluded due to missing income information during census data collection"))
    message(paste0(round(p2, 4), "% encounters are excluded due to missing dissemination area"))
    message(paste0(round(p3, 4), "% encounters are excluded due to no census data linkage with dissemination area"))
    # delete missing data
    rec <- na.omit(rec)
    rec <- rec[qnatippe != 9]
  }


  if (census_year == 2021) {
    # cal % exclusion for 2021 census data
    p1 <- nrow(rec[qnatippe == 999]) / nrow(rec)
    p2 <- nrow(rec[is.na(get(paste0("da", yr, "uid")))]) / nrow(rec)
    p3 <- nrow(rec[!is.na(get(paste0("da", yr, "uid"))) & (
      is.na(qnatippe) |
        is.na(households_dwellings_DA21) |
        is.na(get(paste0("c", yr, "_vismin_pct"))) |
        is.na(get(paste0("c", yr, "_immsta_pct"))) |
        is.na(get(paste0("c", yr, "_ed_15over_postsec_pct"))) |
        is.na(get(paste0("c", yr, "_ed_25to64_postsec_pct"))))]) / nrow(rec)

    message(paste0(round(p1, 4), "% encounters are excluded due to missing income information during census data collection"))
    message(paste0(round(p2, 4), "% encounters are excluded due to missing dissemination area"))
    message(paste0(round(p3, 4), "% encounters are excluded due to no census data linkage with dissemination area"))
    # delete missing data
    rec <- na.omit(rec)
    rec <- rec[qnatippe != 999]
  }


  return(rec)
}
