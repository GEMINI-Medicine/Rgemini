#' @title
#' Count the number Red Blood Cell Transfusions per encounter
#'
#' @description
#' This function calculates appropriate transfusions and any transfusions.
#' The definition of an appropriate transfusion can be found on the sample
#' [My Practice Report](https://www.hqontario.ca/Portals/0/documents/qi/practice-reports/general-medicine-sample-report.html#transfusions-qi).
#' A transfusion is defined by procedure in the below codes:
#' 1.  [4144461](https://athena.ohdsi.org/search-terms/terms/4144461).
#' 2.  [4137859](https://athena.ohdsi.org/search-terms/terms/4137859)
#' 3.  [4022173](https://athena.ohdsi.org/search-terms/terms/4022173)
#'
#' An appropriate transfusion also requires that the patients pre-transfusion hemoglobin levels are  below 80 g/L.
#' The Athena definition of a hemoglobin test:
#'  [3000963](https://athena.ohdsi.org/search-terms/terms/3000963)
#'
#' @param con \cr
#' Database connection to any GEMINI database
#'
#' @param ipadmdad (`data.table`)\cr
#' `ipadmdad` table as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' @return
#' `data.table`
#' It includes three columns. It includes genc_id,
#' n_app_rbc_transfusion_derived, and n_rbc_transfusion_derived.
#' It will include all rows from the ipadmdad parameter. Any encounter without a
#' transfusion will get a 0.
#'
#' @import RPostgreSQL
#'
#' @export
#'

rbc_transfusions <-function(con,
                            ipadmdad
                            ) {

  
  cat("\n***Note:***
  The output of this function is based on manual mapping of RBC transfusions by a GEMINI Subject Matter Expert.
  Please carefully check mapping coverage for your cohort of interest, or contact the GEMINI team if you require additional support.\n")
  
  ipadmdad <- coerce_to_datatable(ipadmdad)


  ipadmdad_subset <- ipadmdad[hospital_num %ni% c(105, 106)]

  temp_d_glist <- ipadmdad_subset[,.(genc_id)][["genc_id"]]

  dbGetQuery(
    con ,
    paste0(
      "drop table if exists lab_bb;
    create temp table lab_bb as
    select genc_id,
    result_value,
    collection_date_time,
    hospital_id
    from lab l
    where l.test_type_mapped_omop = '3000963' and
      genc_id in (",
      paste(temp_d_glist, collapse = ","),
      ")")
    )

  dbGetQuery(con,
             paste0("drop table if exists transfusion_temp;
             create temp table transfusion_temp as
             select * from transfusion a
             where blood_product_mapped_omop in ('4022173','4137859','4144461')
             and genc_id in (",
                    paste(temp_d_glist, collapse = ","),
                    ")")
             )

  bb <- dbGetQuery(
    con,"select t.genc_id,
    issue_date_time ,
    l.collection_date_time,
    a.admission_date_time,
    l.result_value
    from
    transfusion_temp t
    left join lab_bb l on t.genc_id = l.genc_id
    left join admdad a on t.genc_id = a.genc_id
    where
    t.issue_date_time >= a.admission_date_time and
    t.issue_date_time <= a.discharge_date_time ;") %>%
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
    ipadmdad_subset[,.(genc_id)],
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
