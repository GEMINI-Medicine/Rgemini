#' @title
#' Obtain commonly used neighbourhood-level socioeconomic status (SES) variables
#'
#' @description
#' The `neighbourhood_ses()` function derives neighbourhood-level SES variables for
#' a given encounter based on the dissemination area they reside in. All variables
#' returned by this function are based on Statistics Canada Census data and the
#' Ontario Marginalization Index (ON-Marg; see below for details).
#'
#' For database versions since `drm_cleandb_v3` / `H4H_template_v4` users can
#' choose between 2016 vs. 2021 census/ON-Marg data. For earlier versions,
#' only 2016 census data are available. Note that the names of some
#' output variables vary by census year.
#'
#' @section Statistics Canada Census:
#' The Statistics Canada census is collected every 5 years and provides a detailed
#' statistical portrait of communities across Canada, including information about
#' income, education, ethnicity, and immigrant status.
#'
#' Census data are collected by dissemination area (DA), which typically covers
#' a population of 400-700 people. To enable linkage between GEMINI data and
#' DA-level location, the DA of a given encounter was derived from their postal
#' code using the [Postal Code Conversion File Plus (PCCF+)](https://library.carleton.ca/sites/default/files/2023-03/PCCF%2BUserguide-2021.pdf)
#' program.
#'
#' The `neighbourhood_ses()` function currently returns the following census
#' variables:
#'
#' - [**Income**](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=pop123):
#'  - Statistics Canada sources information about household income from the Canadian Revenue Agency
#'  - PCCF+ provides an income per person equivalent (IPPE) by adjusting household income by household size
#'  - Both continuous income and national/community quintiles are returned
#' - [**Education**](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=pop038):
#'  - Indicates a person's highest level of education: Based on the long-form census questionnaire,
#' which is only administered to 25% of households
#'  - The function returns the % of respondents with a post-secondary certificate, diploma, or degree
#' - [**Visible minorities**](https://www12.statcan.gc.ca/census-recensement/2021/ref/98-500/006/98-500-x2021006-eng.cfm):
#'  - Indicates whether a person identifies as a visible minority, defined as follows by
#' the Employment Equity Act: “persons, other than Aboriginal peoples, who are non-Caucasian in
#' race or non-white in colour” (e.g., Black, South Asian, Chinese, Latin American etc.)
#'  - Based on the long-form census questionnaire, which is only administered
#' to 25% of households
#' - [**Immigrant status**](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=pop148):
#'  - Indicates whether a person is, or has ever been, a landed immigrant or permanent resident
#' in Canada. This indludes those who have obtained Canadian citizenship by naturalization.
#'  - In 2021 census: Sourced from Immigration, Refugees and Citizenship Canada
#'  - In 2016 census: Based on the long-form questionnaire (25% of households)
#'
#' @section Ontario Marginalization Index (On-Marg):
#' On-Marg is a neighbourhood-level index measuring marginalization differences
#' between areas based on a subset of variables from the Statistics Canada census.
#'
#' The index was derived from a principal component factor analysis on 42
#' variables, which resulted in 18 indicators along the following 4 dimensions:
#'
#' 1) Households and dwellings: Measures housing density and characteristics of
#' family structure (e.g., living alone, % dwellings not owned)
#' 2) Material resources: Measures access to basic material needs (e.g., housing,
#' food, and clothing), education, and employment
#' 3) Age and labour force: Includes indicators such as the % of seniors (65+),
#' children, and those that are not part of the labour force
#' 4) Racialized and newcomer populations: Measures the % of people who are recent
#' immigrants (within last 5 years) or identify as a visible minority
#'
#' In the 2016 version of ON-Marg, the 4 dimensions were called "Residential
#' instability", "Material deprivation", "Dependency", and "Ethnic concentration"
#' respectively. The dimensions were renamed in 2021 to avoid deficit-based
#' language and better reflect the census measures associated with each dimension.
#'
#' All ON-Marg variables are available as continuous factor scores as well as
#' quintiles. Higher scores represent a higher degree of marginalization.
#'
#' @section Missing values:
#' Some encounters could not be linked to Statistics Canada data due to missing/
#' invalid postal codes, or due to the fact that they reside in an area not
#' covereded by the census. These encounters will be returned with `da_uid = NA`.
#'
#' Additionally, Statistics Canada suppresses results from certain DAs due to
#' low response rates or data quality issues. The corresponding census/ON-Marg
#' variables will be returned as `NA` for all `genc_ids` in those DAs.
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param census_year (`numeric` | `character`)\cr
#' Statistics Canada census year. Only 2016 or 2021 are valid inputs.
#'
#' @param cohort (`data.table` | `data.frame`)\cr
#' Table with all relevant encounters of interest, where each row corresponds
#' to a single encounter. Must contain GEMINI Encounter ID (`genc_id`).
#'
#' @return (`data.frame` | `data.table`)\cr
#' This function returns a `data.table` where each row corresponds to a
#' `genc_id` from the user-provided cohort input, together with the following
#' columns:
#'
#' - The user-provided census year: `census_year` (2016 or 2021)
#' - DA the encounter resides in (based on PCCF+): `da_uid`
#' - Neighbourhood-level income (continuous):
#'  - `atippe` (neighbourhood after tax income per single person equivalent)
#'  - `btippe` (neighbourhood before tax income per single person equivalent)
#' - Neighbourhood-level income (quintiles from PCCF+):
#'  - `qnatippe` and `qnbtippe`: Quintiles of `atippe` and `btippe` calculated
#' based on *national* income distribution
#'  - `qaatippe` and `qabtippe`: Quintiles of `atippe` and `btippe` calculated
#' based on distribution within a given community (based on census metropolitan
#' area, census agglomeration, or residual area within each province).
#' - % visible minorities: `vismin_pct`
#' - % with immigrant status: `immsta_pct`
#' - % with post-secondary education:
#'  - Including all respondents > 15 years of age: `ed_15over_postsec_pct`
#'  - Only including respondents between 25-64 years: `ed_25to64_postsec_pct`
#' - Ontario Marginalization Index (continuous):
#'  - If `census_year` = 2021: `households_dwellings`, `material_resources`,
#' `age_labourforce`, `racialized_NC_pop`
#'  - If `census_year` = 2016: `instability`, `deprivation`, `dependency`,
#' `ethniccon`
#' - Ontario Marginalization Index (quintiles):
#'  - All ON-Marg variables are additionally returned as quintiles, as indicated
#' by the suffix `_q` (e.g., `households_dwellings_q`)
#'
#' @references
#' - **Statistics Canada Census**
#'  - 2021 Census: https://www12.statcan.gc.ca/census-recensement/2021/ref/index-eng.cfm
#'  - 2016 Census: https://www12.statcan.gc.ca/census-recensement/2016/ref/index-eng.cfm
#' - **Ontario Maginalization Index**
#'  - ON-Marg 2021: https://www.publichealthontario.ca/-/media/documents/o/2017/on-marg-userguide.pdf
#'  - ON-Marg 2016: https://www.publichealthontario.ca/-/media/documents/U/2018/userguide-on-marg.pdf
#'  - Additional information from Public Health Ontario: https://www.publichealthontario.ca/-/media/Event-Presentations/2023/09/ontario-marginalization-index-updates-products.pdf?rev=07baae2569164c17abaa18464075aa20&sc_lang=en
#' - **PCCF+**
#'  - PCCF+ Reference Guide: https://library.carleton.ca/sites/default/files/2023-03/PCCF%2BUserguide-2021.pdf
#'  - Measuring Health Inequalities - A Toolkit: https://www.cihi.ca/sites/default/files/document/toolkit-area-level-measurement-pccf-en.pdf
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
#' cohort <- dbGetQuery(dbcon, "SELECT genc_id from admdad LIMIT 100;")
#'
#' neighbourhood_ses_table <- neighbourhood_ses(dbcon, cohort, 2021)
#' }
#'
neighbourhood_ses <- function(dbcon, cohort, census_year) {
  ## check user inputs
  check_input(cohort, c("data.table", "data.frame"), colnames = "genc_id")
  cohort <- coerce_to_datatable(cohort)
  check_input(dbcon, argtype = "DBI")
  # only 2016 & 2021 census are currently available
  check_input(census_year, c("numeric", "character"), categories = c(2016, 2021))

  ## write a temp table to improve querying efficiency
  dbExecute(dbcon, "SET client_min_messages TO WARNING;") # suppress SQL notices
  DBI::dbSendQuery(dbcon, "Drop table if exists temp_data;")
  DBI::dbWriteTable(
    dbcon, c("pg_temp", "temp_data"), cohort[, .(genc_id)],
    row.names = FALSE, overwrite = TRUE
  )

  ## query relevant DB table based on census year
  if (census_year == 2021) {
    # return error if table does not exist
    table_name <- tryCatch(
      {
        find_db_tablename(dbcon, "lookup_statcan_v2021")
      },
      error = function(e) {
        stop(paste(
          "Your database version does not contain 2021 census data.",
          "Please try running this function with `census_year = 2016`, to obtain 2016 census data instead."
        ))
      }
    )

    # query relevant columns
    # note: columns with capital letters need "" to maintain column name
    nbhd_data <- DBI::dbGetQuery(
      dbcon,
      "SELECT tmp.genc_id, l.da21uid, s.c21_vismin, s.c21_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe,
      s.qabtippe, s.atippe, s.btippe, s.c21_immsta, s.c21_immsta_imm, s.c21_ed_15over_postsec,
      s.c21_ed_15over, s.c21_ed_25to64_postsec, s.c21_ed_25to64, \"households_dwellings_DA21\",
      \"material_resources_DA21\", \"age_labourforce_DA21\", \"racialized_NC_pop_DA21\",
      \"households_dwellings_q_DA21\", \"material_resources_q_DA21\", \"age_labourforce_q_DA21\",
      \"racialized_NC_pop_q_DA21\"
      FROM temp_data tmp
      left join locality_variables l on l.genc_id = tmp.genc_id
      left join lookup_statcan_v2021 s on l.da21uid = s.da21uid;"
    ) %>%
      as.data.table()
  } else if (census_year == 2016) {
    # in DB versions < drm_cleandb_v3/H4H_template_v4, the 2016 statcan table was simply called
    # 'lookup_statcan'; for later versions, it's called 'lookup_statcan_v2016'
    # either version should exist in most DBs, but if not, an error will be returned
    table_name <- tryCatch(
      {
        find_db_tablename(dbcon, "lookup_statcan")
      },
      error = function(e) {
        try(find_db_tablename(dbcon, "lookup_statcan_v2016"))
      }
    )

    # query relevant columns
    nbhd_data <- DBI::dbGetQuery(
      dbcon, paste("SELECT tmp.genc_id, l.da16uid, s.c16_vismin, s.c16_vismin_not, s.qnatippe,
      s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c16_immsta, s.c16_immsta_imm,
      s.c16_ed_15over_postsec, s.c16_ed_15over, s.c16_ed_25to64_postsec, s.c16_ed_25to64,
      s.instability_da16, s.deprivation_da16, s.dependency_da16, s.ethniccon_da16,
      s.instability_q_da16, s.deprivation_q_da16, s.dependency_q_da16, s.ethniccon_q_da16
      FROM temp_data tmp
      left join locality_variables l on l.genc_id = tmp.genc_id
      left join ", table_name, " s on l.da16uid = s.da16uid; ")
    ) %>%
      as.data.table()
  }
  nbhd_data[nbhd_data == ""] <- NA

  ## Standardize variable names
  # remove year prefixes/suffixes from column names
  setnames(nbhd_data,
    c("da16uid", "da21uid"), c("da_uid", "da_uid"),
    skip_absent = TRUE
  )
  setnames(nbhd_data, gsub(paste0(
    "c16_|c21_|_da16|_da21" # e.g., c16_immsta -> immsta
  ), "", colnames(nbhd_data), ignore.case = TRUE))

  ## Income
  # set all invalid income values to NA
  # note: in 2016 invalid quintiles are 9, in 2021 invalid quintiles are 999
  # -> set all income variables ("tippe") to NA if `qnatippe` starts with 9
  # this also overwrites invalid raw income (e.g., atippe = 9999999) with NA
  nbhd_data[
    grepl("^9", qnatippe), (
      grep("tippe", names(nbhd_data), value = TRUE)
    ) := lapply(.SD, function(x) NA),
    .SDcols = patterns("tippe")
  ]

  ## % visible minority
  nbhd_data[, vismin_pct := round(100 * vismin / (vismin + vismin_not), 2)]

  ## % immigration status
  nbhd_data[, immsta_pct := round(100 * immsta_imm / immsta, 2)]

  ## % post-secondary education (>15)
  nbhd_data[, ed_15over_postsec_pct := round(100 * ed_15over_postsec / ed_15over, 2)]

  ## % post-secondary education (25-64)
  nbhd_data[, ed_25to64_postsec_pct := round(100 * ed_25to64_postsec / ed_25to64, 2)]

  ## warning about % missing/invalid DA
  # all valid DAs should be 8-digit numerical codes
  # according to PCCF+ documentation, DAs ending with 9999 are invalid (see page 21:
  # https://library.carleton.ca/sites/default/files/2023-03/PCCF%2BUserguide-2021.pdf)
  # census/ON-Marg variables should all be NA in those cases
  nbhd_data[grepl("9999$", da_uid) | nchar(da_uid) < 8, da_uid := NA]
  message(paste0(
    round(sum(is.na(nbhd_data$da_uid)) / nrow(nbhd_data) * 100, 2),
    "% of `genc_ids` in the cohort could not be linked to census data due to ",
    "missing/invalid postal code information. These encounters are returned with `da_uid = NA`.\n"
  ))

  ## warning about % missing (suppressed) census/ON-Marg variables
  # only include encounters with valid DA in denominator
  message(paste0(
    "Among `genc_ids` with valid DA, ",
    round(100 * sum(rowSums(is.na(nbhd_data[!is.na(da_uid)])) > 0) / sum(!is.na(nbhd_data$da_uid)), 2),
    "% have at least 1 missing (suppressed) census or ON-Marg variable. ",
    "These variables are returned as NA. ",
    "Please carefully check missingness for each variable of interest."
  ))

  ## remove raw numerator/denominator columns from output
  nbhd_data <- nbhd_data[, -c(
    "vismin", "vismin_not", "immsta_imm", "immsta", "ed_15over_postsec",
    "ed_15over", "ed_25to64_postsec", "ed_25to64"
  )]

  ## Add census year to output & make sure all encounters are returned
  cohort[, census_year := census_year]
  output <- merge(
    cohort[, .(genc_id, census_year)], nbhd_data,
    by = "genc_id", all.x = TRUE
  )

  return(output)
}
