#' @title
#' Obtain commonly used neighbourhood-level socioeconomic (SES) variables
#'
#' @description
#' The `nbhd_ses_variables()` function merges SES variables based on dissemination area ID.
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param dissemination_area\cr
#' Version of dissemination_area chosen. Only 2016 or 2021 are valid inputs. 
#' 
#' @param cohort\cr
#' User specified cohort
#' 
#' @return (`data.frame` | `data.table`)\cr
#' This function returns a `data.table` merged with dissemination area ID and SES variables.
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'                         dbname = "db",
#'                         host = "domain_name.ca",
#'                         port = 1234,
#'                         user = getPass("Enter user:"),
#'                         password = getPass("password"))
#'
#' nbhd_ses_table <- nbhd_ses_variables(dbcon, 2016, cohort)
#' }
#'

nbhd_ses_variables <- function (dbcon, dissemination_area, cohort) {
  if (!RPostgreSQL::isPostgresqlIdCurrent(dbcon)) {
    stop("\n Please input a valid database connection")
  }
  
  ## write a temp table to improve querying efficiency
  DBI::dbSendQuery(dbcon,"Drop table if exists temp_data;")
  DBI::dbWriteTable(dbcon, c("pg_temp","temp_data"), cohort[,.(genc_id)], row.names = F, overwrite = T)
  
  ## only da=2016, 2021 are allowed values
  if (!dissemination_area %in% c(2016, 2021)) {
    stop("Only 2016 and 2021 are valid versions of dissemination area")
  }
  
  ## message to show invalid meanings of null values
  message('Null values or 9 or 999 represent unknown or missing estimates')
  
  if  (dissemination_area==2016) {
    var_tbl <- DBI::dbGetQuery(dbcon, "select tmp.genc_id, l.da16uid, s.c16_vismin, s.c16_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c16_immsta, s.c16_immsta_imm, s.c16_ed_15over_postsec, s.c16_ed_15over, s.c16_ed_25to64_postsec, s.c16_ed_25to64, s.instability_da16, s.deprivation_da16, s.dependency_da16, s.ethniccon_da16, s.instability_q_da16, s.deprivation_q_da16, s.dependency_q_da16, s.ethniccon_q_da16

                                          from temp_data tmp
                                          
                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan_v2016 s
                                            on l.da16uid = s.da16uid; " 
                               
    ) %>% as.data.table()
    # cal % visible minority
    var_tbl[, c16_vismin_pct := round(c16_vismin / (c16_vismin + c16_vismin_not) * 100, 2 )]
    
    # cal % immigration status
    var_tbl[, c16_immsta_pct := round(c16_immsta_imm / c16_immsta * 100, 2 )]
    
    # cal % post-secondary education (>15)
    var_tbl[, c16_ed_15over_postsec_pct := round(c16_ed_15over_postsec / c16_ed_15over * 100, 2 )]
    
    # cal % post-secondary education (25-64)
    var_tbl[, c16_ed_25to64_postsec_pct := round(c16_ed_25to64_postsec / c16_ed_25to64 * 100, 2 )]
    
    var_tbl <- var_tbl[, !c('c16_vismin', 'c16_vismin_not', 'c16_immsta_imm', 'c16_immsta', 'c16_ed_15over_postsec', 'c16_ed_15over', 'c16_ed_25to64_postsec', 'c16_ed_25to64'
    )]  
    
    
  }
  
  if  (dissemination_area==2021) {
    var_tbl <- DBI::dbGetQuery(dbcon, 'select tmp.genc_id, l.da21uid, s.c21_vismin, s.c21_vismin_not, s.qnatippe, s.qnbtippe, s.qaatippe, s.qabtippe, s.atippe, s.btippe, s.c21_immsta, s.c21_immsta_imm, s.c21_ed_15over_postsec, s.c21_ed_15over, s.c21_ed_25to64_postsec, s.c21_ed_25to64, "households_dwellings_DA21", "material_resources_DA21", "age_labourforce_DA21", "racialized_NC_pop_DA21", "households_dwellings_q_DA21", "material_resources_q_DA21", "age_labourforce_q_DA21", "racialized_NC_pop_q_DA21"

                                          from temp_data tmp
                                          
                                          left join locality_variables l
                                            on l.genc_id = tmp.genc_id
                                          left join lookup_statcan_v2021 s
                                            on l.da21uid = s.da21uid; ' 
                               
    ) %>% as.data.table()
    
    # cal % visible minority
    var_tbl[, c21_vismin_pct := round(c21_vismin / (c21_vismin + c21_vismin_not) * 100, 2 )]
    
    # cal % immigration status
    var_tbl[, c21_immsta_pct := round(c21_immsta_imm / c21_immsta * 100, 2 )]
    
    # cal % post-secondary education (>15)
    var_tbl[, c21_ed_15over_postsec_pct := round(c21_ed_15over_postsec / c21_ed_15over * 100, 2 )]
    
    # cal % post-secondary education (25-64)
    var_tbl[, c21_ed_25to64_postsec_pct := round(c21_ed_25to64_postsec / c21_ed_25to64 * 100, 2 )]
    
    var_tbl <- var_tbl[, !c('c21_vismin', 'c21_vismin_not', 'c21_immsta_imm', 'c21_immsta', 'c21_ed_15over_postsec', 'c21_ed_15over', 'c21_ed_25to64_postsec', 'c21_ed_25to64'
    )] 
    
  }  
  
  return(var_tbl)
  
}







