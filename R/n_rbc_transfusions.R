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
#' A database connection to any GEMINI database. `DBI` connection is recommended
#' as `odbc` connection may cause connection issues in certain environment.
#'
#' @param cohort (`data.frame` or `data.table`)
#' Cohort table with all relevant encounters of interest where each row
#' corresponds to a single encounter. Must contain the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `hospital_num` (`integer`): Hospital number
#'
#' @param exclude_ed (`logical`)
#' Whether to exclude transfusions in emergency department. When set to `TRUE`, only
#' transfusions performed in-inpatient settings are counted. When set to `FALSE`,
#' transfusions in both ED and in-patient settings will be counted.
#' Transfusions in ED are defined as `issue_date_time` earlier
#' than `admission_date_time`. Transfusions with missing `issue_date_time` will be
#' excluded when `exclude_ed` is set to `TRUE`.
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
                              cohort,
                              exclude_ed = FALSE) {

  # warning messages
  mapping_message("RBC transfusions")
  availability_message("transfusion/lab")

  cat("\nThis function may take a few minutes to run...\n\n")

  # input check
  check_input(dbcon, argtype = "dbcon")
  check_input(cohort,
              argtype = c("data.table", "data.frame"),
              colnames = c("genc_id", "hospital_num"))
  check_input(exclude_ed, argtype = "logical")
  cohort <- coerce_to_datatable(cohort)

  ## If relevant: Show warning notifying user of hospital exclusion
  if (any(c(105,106) %in% unique(cohort$hospital_num))) {
    warning("Excluding hospitals with known transfusion data quality issues.
            Please refer to the function documentation for more details.", immediate. = TRUE)

  }
  cohort_subset <- cohort[hospital_num %ni% c(105, 106)]

  # find table names for all relevant tables (admdad/lab/transfusion)
  admdad_table <- find_db_tablename(dbcon, "admdad", verbose = FALSE)
  lab_table <- find_db_tablename(dbcon, "lab", verbose = FALSE)
  transfusion_table <- find_db_tablename(dbcon, "transfusion", verbose = FALSE)

  # load transfusion from db
  transfusion <- dbGetQuery(
    dbcon,
    paste0(
      ifelse(
        exclude_ed == TRUE,
        # filter by admission date time and exclude tests before admission
        paste(
          "select t.genc_id, t.issue_date_time, a.admission_date_time
           from", transfusion_table, "t
           left join admdad a
           on t.genc_id = a.genc_id
           where t.blood_product_mapped_omop in ('4022173','4137859','4144461') and
           t.issue_date_time >= a.admission_date_time and a.genc_id in ("
        ),
        # no filter on collection date time
        paste(
          "select genc_id, issue_date_time, blood_product_mapped_omop
           from", transfusion_table,
          "where blood_product_mapped_omop in ('4022173','4137859','4144461') and
           genc_id in ("
        )
      ),
      # IN method is used instead of temp table method to pull based on genc_id list,
      # to ensure function works in HPC environment
      paste(cohort$genc_id, collapse = ", "), ")"
    )
  ) %>% as.data.table

  # load hemoglobin from db
  hemoglobin <- dbGetQuery(
    dbcon,
    paste0(
      "select genc_id, collection_date_time, result_value
      from lab
      where test_type_mapped_omop = '3000963' and genc_id in (",
      paste(unique(transfusion$genc_id), collapse = ", "), ");"
    )
  ) %>% as.data.table()

  # format date time
  transfusion[, issue_date_time := ymd_hm(issue_date_time)]
  transfusion[, issue_date_time_pre48 := issue_date_time - hours(48)]
  hemoglobin[, collection_date_time := ymd_hm(collection_date_time)]

  # keep numeric values or those starting with <, >, @
  # exclude other non-numeric hgb result values
  hemoglobin[, result_value := as.numeric(
    stringr::str_split(result_value,pattern = "@|<|>",
                       simplify = TRUE)[,1])]
  # non-equi merge to match transfusion with hgb value in 48 hours
  trans_with_hgb <- transfusion[hemoglobin[!is.na(result_value)],
                                .(genc_id,
                                  issue_date_time = x.issue_date_time,
                                  issue_date_time_pre48 = x.issue_date_time_pre48,
                                  result_value = i.result_value,
                                  collection_date_time = i.collection_date_time),
                                on = .(
                                  genc_id,
                                  issue_date_time >= collection_date_time,
                                  issue_date_time_pre48 <= collection_date_time
                                )] %>%
    .[!is.na(issue_date_time)]

  # keep only latest hemoglobin value before transfusion
  trans_with_hgb[, time_diff := as.numeric(difftime(issue_date_time,
                                                    collection_date_time,
                                                    units = "hours"))]
  setorderv(trans_with_hgb,
            c("genc_id",
              "issue_date_time",
              "time_diff",
              "result_value"),
            c(1, 1, 1, 1))
  trans_with_hgb <- unique(trans_with_hgb,
                           by = c("genc_id", "issue_date_time"))

  # calculate transfusion and appropriate transfusion number
  n_trans <- trans_with_hgb[, .(n_rbc_transfusion_derived = .N), genc_id]
  prehgbless80 <- trans_with_hgb[result_value < 80, .(n_app_rbc_transfusion_derived = .N), genc_id]

  res <- Reduce(function(x, y) merge(x,y, by = "genc_id", all.x= T),
                x = list(cohort[, .(genc_id)],
                         n_trans,
                         prehgbless80))
  res[is.na(res)] <- 0

  return(res)

}

