#' @title
#' Count the number Red Blood Cell (RBC) Transfusions per encounter
#'
#' @description
#' This function calculates the number of appropriate transfusions and total of
#' all transfusions per encounter.
#' The definition of an appropriate transfusion can be found on the sample
#' [My Practice Report](https://www.hqontario.ca/Portals/0/documents/qi/practice-reports/general-medicine-sample-report.html#transfusions-qi).
#' A transfusion is defined by procedure in the below codes:
#' 1.  [4144461](https://athena.ohdsi.org/search-terms/terms/4144461).
#' 2.  [4137859](https://athena.ohdsi.org/search-terms/terms/4137859)
#' 3.  [4022173](https://athena.ohdsi.org/search-terms/terms/4022173)
#'
#' An appropriate transfusion also requires that the patients pre-transfusion
#' hemoglobin levels are  below 80 g/L. The Athena definition of a hemoglobin
#' test:
#'  [3000963](https://athena.ohdsi.org/search-terms/terms/3000963)
#'
#' Red blood cell transfusions with no hemoglobin measurement within 48 hours
#' prior to the transfusion are excluded. These scenarios are rare, typically
#' occurring in approximately 2% of blood transfusions in GEMINI data.
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest where each row
#' corresponds to a single encounter. Must contain the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `hospital_num` (`integer`): Hospital number
#'
#' @return (`data.table`)\cr
#' Table with three columns: `genc_id`, `n_app_rbc_transfusion_derived` (number
#' of appropriate RBC transfusions), and `n_rbc_transfusion_derived` (total of
#' all RBC transfusions). Encounters without any transfusion will get a 0.
#'
#' @note
#' Transfusion data from two hospitals with known data quality issues are
#' automatically removed by this function. Any `genc_ids` from those sites are
#' not included in the returned output. When merging the output of this function
#' with another table, those `genc_ids` should have a value of `NA`.
#'
#' @import RPostgreSQL lubridate
#'
#' @export
#'

n_rbc_transfusions <-function(dbcon,
                            cohort) {


  mapping_message("RBC transfusions")

  cat("\nThis function may take a few minutes to run...\n\n")

  cohort <- coerce_to_datatable(cohort)

  ## Cohort table MUST contain hospital_num (not hospital_id) for exclusion below to work correctly
  if ("hospital_num" %ni% names(cohort)) {
    stop("Input table 'cohort' is missing variable 'hospital_num'.
    Please refer to the function documentation for more details.")
  }

  ## If relevant: Show warning notifying user of hospital exclusion
  if (any(c(105,106) %in% unique(cohort$hospital_num))) {
    warning("Excluding hospitals with known transfusion data quality issues.
            Please refer to the function documentation for more details.", immediate. = TRUE)

  }
  cohort_subset <- cohort[hospital_num %ni% c(105, 106)]

  temp_d_glist <- cohort_subset[,.(genc_id)][["genc_id"]]


  # find table names for all relevant tables (admdad/lab/transfusion)
  admdad_table <- find_db_tablename(dbcon, "admdad", verbose = FALSE)
  lab_table <- find_db_tablename(dbcon, "lab", verbose = FALSE)
  transfusion_table <- find_db_tablename(dbcon, "transfusion", verbose = FALSE)

  dbGetQuery(
    dbcon ,
    paste0(
      "drop table if exists lab_bb;
    create temp table lab_bb as
    select genc_id,
    result_value,
    collection_date_time
    from ", lab_table, " l
    where l.test_type_mapped_omop = '3000963' and
      genc_id in (",
      paste(temp_d_glist, collapse = ","),
      ")")
  )

  dbGetQuery(dbcon,
             paste0("drop table if exists transfusion_temp;
             create temp table transfusion_temp as
             select * from ", transfusion_table, " a
             where blood_product_mapped_omop in ('4022173','4137859','4144461')
             and genc_id in (",
                    paste(temp_d_glist, collapse = ","),
                    ")")
             )

  bb <- dbGetQuery(
    dbcon,paste0("select t.genc_id,
    issue_date_time ,
    l.collection_date_time,
    a.admission_date_time,
    l.result_value
    from
    transfusion_temp t
    left join lab_bb l on t.genc_id = l.genc_id
    left join ", admdad_table, " a on t.genc_id = a.genc_id
    where
    t.issue_date_time >= a.admission_date_time and
    t.issue_date_time <= a.discharge_date_time ;")) %>%
    data.table()

  bb_2 <- bb %>%
    .[, c("time_diff", "result_value")
      :=.(
        as.numeric(difftime(ymd_hm(issue_date_time),
                            ymd_hm(collection_date_time),
                            units = "hours")),
        as.numeric(
          stringr::str_split(result_value,pattern = "@|<|>",
                             simplify = TRUE)[,1]))] %>%
    .[!is.na(result_value) & time_diff>=0 & time_diff<=48]
  ### Replaced this:
  ### .[order(genc_id,issue_date_time, time_diff, decreasing =FALSE),]

  setorderv(bb_2, c("genc_id",
                    "issue_date_time",
                    "time_diff",
                    "result_value"),
            c(1, 1,1, 1))

  bb_3 <- unique(bb_2, by = c("genc_id",
                              "issue_date_time"))


  # new code
  rbc <- unique(bb_3)

  res <- rbc[,':='(with.pre.hgb = TRUE, pre.hgb = result_value)]

  prehgbless80 <- res[as.numeric(pre.hgb) < 80, .N, genc_id]

  ntrans <- res[!is.na(as.numeric(pre.hgb)), .N, genc_id]

  ipadmdad_preghg80 <- merge(
    cohort_subset[,.(genc_id)],
    prehgbless80[, .(genc_id, N.Trans.with.pre.HGBLess80P = N)],
    by = "genc_id",
    all.x = T)# check duplicated  genc_id

  ipadmdad_rbc_all <- merge(ipadmdad_preghg80,
                            ntrans[, .(genc_id, N.Trans = N)],
                            by = "genc_id",
                            all.x = T)

  ipadmdad_rbc_all[is.na(ipadmdad_rbc_all)] <- 0

  setnames(ipadmdad_rbc_all,
           old = c("N.Trans.with.pre.HGBLess80P",
                   "N.Trans"),
           new = c("n_app_rbc_transfusion_derived",
                   "n_rbc_transfusion_derived"))

  return(ipadmdad_rbc_all)

}
