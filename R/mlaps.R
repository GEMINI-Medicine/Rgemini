LAPS_OMOP_CONCEPTS <- c(
  3019550, # Sodium
  3024641, # Blood Urea Nitrogen (BUN)
  3020564, # Creatinine
  3024561, # Albumin
  3009542, # Hematocrit
  3010813, # White Blood Cell Count
  3019977, # Arterial pH
  3027946, # Arterial paCO2
  3027801, # Arterial paO2
  3013826, # Glucose Random
  3040151, # Glucose Random
  3018251, # Glucose Random
  3006140 # Bilirubin
)


#' @title
#' Assign score to LAPS component
#'
#' @param x (`character`)\cr
#' A vector of result values for a particular lab test.
#'
#' @param breaks (`numeric`)\cr
#' A vector of intervals for a particular test which correspond to a different number of points.
#'
#' @param points (`numeric`)\cr
#' A vector of scores for each interval in `breaks`.
#'
#' @return (`numeric`)\cr
#' A vector of scores for each lab result value provided in `x`.
#'
#' @importFrom stringr str_sub
#' @export
#'
#' @examples
#' x <- c("0.3", "0.7")
#' breaks <- c(0, 0.2, 0.4, 0.8, Inf)
#' points <- c(1, 2, 3, 4)
#'
#' laps_assign_test(x, breaks, points)
#'
laps_assign_test <- function(x, breaks, points) {
  # casting as character then back to numeric because default output for `cut` with `labels` is `factor`
  pts <- as.character(
    cut(
      suppressWarnings(as.numeric(x)),
      breaks,
      labels = points,
      include.lowest = TRUE,
      right = FALSE
    )
  )

  if (sum(stringr::str_sub(x, 1, 1) %in% c("<", ">")) > 0) {
    lower_limit <- breaks[2]
    upper_limit <- breaks[length(breaks) - 1]
    lower_points <- points[1]
    upper_points <- points[length(points)]
    pts[is.na(pts) & startsWith(x, "<") & suppressWarnings(as.numeric(stringr::str_sub(x, 2))) <= lower_limit] <- lower_points
    pts[is.na(pts) & startsWith(x, ">") & suppressWarnings(as.numeric(stringr::str_sub(x, 2))) >= upper_limit] <- upper_points
  }

  return(as.numeric(pts))
}


#' @title
#' Loop mLAPS
#'
#' @description
#' A wrapper around the `mlaps()` function which breaks down the calculation of mLAPS on a
#' large number of encounters by hospital-year. This avoids memory issues that can be caused
#' by loading large chunks of the lab table.
#'
#' @param db (`DBIConnection`)\cr
#' RPostgres DB connection.
#'
#' @param cohort (`data.frame` or `data.table`)\cr
#' Containing a column of `genc_ids` to restrict the calculation to.
#'
#' @param hours_after_admission (`numeric`)\cr
#' Consider lab tests collected **up to** `hours_after_admission` hours after inpatient admission in the calculation.
#' Default `hours_after_admission` is set to 0, where only lab tests collected at Emergency Department (before inpatient admission) are considered in mLAPS calculation.
#' Since not all encounters are admitted through Emergency Department, depending on research question, it can be relevant to consider lab tests collected in early inpatient admission.
#' Typically, `hours_after_admission` can be set to 24 to consider any lab tests collected at Emergency Department and 24 hours after inpatient admission.
#'
#' @param component_wise (`logical`)\cr
#' Does not aggregate the score and instead outputs for each LAPS component (test) its contribution
#' to the overall score.
#'
#' @return (`data.frame`)\cr
#' If `output_laps_components == TRUE`:
#'     `genc_id` (`numeric`),\cr
#'     `test_type_mapped_omop` (`character`),\cr
#'     `mlaps` (`numeric`) max score for this test.
#'
#' If `output_laps_components == FALSE`:
#'     `genc_id` (`numeric`),\cr
#'     `mlaps` (`numeric`) sum of max scores for each relevant test for this encounter.
#'
#' @import DBI RPostgreSQL
#' @importFrom purrr map2_df
#' @export
#'
#' @examples
#' \dontrun{
#' drv <- DBI::dbDriver("PostgreSQL")
#' db <- DBI::dbConnect(
#'   drv,
#'   dbname = "db_name",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass::getPass("Enter Username"),
#'   password = getPass::getPass("Enter Password")
#' )
#'
#' cohort <- DBI::dbGetQuery(db, "SELECT genc_id FROM public.admdad LIMIT 200;")
#'
#' laps <- loop_laps(db, cohort = cohort)
#' }
#'
#' @references
#' When the function is used, please cite the following:
#' https://doi.org/10.1097/MLR.0b013e3181589bb6
#' https://doi.org/10.1007/s11606-023-08245-w
#' https://doi.org/10.1101/2023.01.06.23284273
#'
loop_mlaps <- function(db, cohort = NULL, hours_after_admission = 0, component_wise = FALSE) {
  hospital_field <- return_hospital_field(db)
  # find table corresponding to admdad
  admdad_table <- find_db_tablename(db, "admdad", verbose = FALSE)

  # Ensure cohort is a data.table/data.frame with genc_id and
  # write a temp table if cohort is not NULL
  if (!is.null(cohort)) {
    check_input(cohort, c("data.table", "data.frame"),
      colnames = "genc_id"
    )
    DBI::dbSendQuery(db, "Drop table if exists cohort_data;")
    DBI::dbWriteTable(db, c("pg_temp", "cohort_data"), cohort[, .(genc_id)], row.names = F, overwrite = T)
    # Analyze speed up the use of temp table
    DBI::dbSendQuery(db, "Analyze cohort_data")
  }

  admdad <- DBI::dbGetQuery(
    db,
    paste(
      "SELECT
        genc_id,
        admission_date_time,
        EXTRACT(YEAR FROM discharge_date_time::DATE) AS year,",
      hospital_field, "AS hospital_id",
      "FROM ", admdad_table,
      if (!is.null(cohort)) {
        paste("a WHERE exists (select 1 from cohort_data c where c.genc_id=a.genc_id) ")
      }
    )
  ) %>%
    as.data.table()

  hospital_years <- admdad %>%
    select(hospital_id, year) %>%
    unique()

  # find table corresponding to lab
  lab_table <- find_db_tablename(db, "lab", verbose = FALSE)

  res <- purrr::map2_df(
    hospital_years$hospital_id,
    hospital_years$year,
    function(hospital_id, year) {
      lab <- DBI::dbGetQuery(
        db,
        paste(
          "SELECT
            l.genc_id,
            l.collection_date_time,
            l.test_type_mapped_omop,
            l.result_value,
            l.result_unit
          FROM ", lab_table, "l
          INNER JOIN ", admdad_table, " a
            ON l.genc_id = a.genc_id
          WHERE l.test_type_mapped_omop IN (", paste(LAPS_OMOP_CONCEPTS, collapse = ", "), ")",
          paste0("AND l.", hospital_field, " = '", hospital_id, "'"),
          "AND EXTRACT(YEAR FROM a.discharge_date_time::DATE) = ", year,
          if (!is.null(cohort)) {
            paste("and exists (select 1 from cohort_data c where c.genc_id=l.genc_id)")
          }
        )
      ) %>%
        as.data.table()

      res <- mlaps(
        admdad %>% dplyr::filter(hospital_id == hospital_id, year == year),
        lab,
        hours_after_admission = hours_after_admission,
        component_wise = component_wise
      )
    }
  )

  return(res)
}


#' @title
#' mLAPS
#'
#' @details
#' Modified Laboratory based Acute Physiology Score (mLAPS) uses 12 lab test values from 11 unique lab tests.
#' In this modified version, troponin tests are not considered in the mLAPS calculation.
#'
#' @param ipadmdad (`data.frame`)\cr
#' Table equivalent to a subset of the `admdad` table defined in the
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/wp-content/uploads/2023/12/GEMINI-Data-Repository-Data-Dictionary-v3.0.2.html).
#'
#' @param lab (`data.table`, `data.frame`)\cr
#' Table equivalent to a subset of the `lab` table defined in the
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/wp-content/uploads/2023/12/GEMINI-Data-Repository-Data-Dictionary-v3.0.2.html).
#'
#' @param hours_after_admission (`numeric`)\cr
#' Consider lab tests collected **up to** `hours_after_admission` hours after inpatient admission in the calculation.
#' Default `hours_after_admission` is set to 0, where only lab tests collected at Emergency Department (before inpatient admission) are considered in mLAPS calculation.
#' Since not all encounters are admitted through Emergency Department, depending on research question, it can be relevant to consider lab tests collected in early inpatient admission.
#' Typically, `hours_after_admission` can be set to 24 to consider any lab tests collected at Emergency Department and 24 hours after inpatient admission.
#'
#' @param component_wise (`logical`)\cr
#' Does not aggregate the score and instead outputs for each LAPS component (test) its contribution
#' to the overall score.
#'
#' @return (`data.frame`)\cr
#' If `componentwise == TRUE`:
#'     `genc_id` (`numeric`),\cr
#'     `test_type_mapped_omop` (`character`),\cr
#'     `mlaps` (`numeric`) max score for this test.
#'
#' If `componentwise == FALSE`:
#'     `genc_id` (`numeric`),\cr
#'     `mlaps` (`numeric`) sum of max scores for each relevant test for this encounter.
#'
#' @note
#' When an encounter has multiple recorded lab results for a given "LAPS-relevant" test, the test result which results in the
#' *worst* possible LAPS contribution is taken for a conservative estimate.
#'
#' Patients without entries in the lab table within `hours_of_admission` are not returned.
#' For those encounters which were not returned, it may be reasonable to impute their LAPS score with zero
#' if lab data was in principle available for their site and time period.
#' If lab data was unavailable, it might be more accurate to assign the LAPS score for these encounters as `NA`.
#' In general it is recommended to take care and be intentional when imputing LAPS scores.
#'
#' @importFrom lubridate hours
#' @export
#'
#' @references
#' When the function is used, please cite the following:
#' \itemize{
#'  \item{Escobar G, et al. Med Care, 2008. https://doi.org/10.1097/MLR.0b013e3181589bb6}
#'  \item{Roberts SB, et al. J Gen Intern Med, 2023. https://doi.org/10.1007/s11606-023-08245-w}
#'  \item{Roberts SB, et al. medRxiv (preprint), 2023. https://doi.org/10.1101/2023.01.06.23284273}
#' }
#'
mlaps <- function(ipadmdad, lab, hours_after_admission = 0, component_wise = FALSE) {
  mapping_message("lab tests")

  lab <- lab %>%
    select(test_type_mapped_omop, genc_id, result_value, result_unit, collection_date_time) %>%
    filter(test_type_mapped_omop %in% LAPS_OMOP_CONCEPTS) %>%
    left_join(
      ipadmdad %>% select(genc_id, admission_date_time),
      by = "genc_id"
    ) %>%
    filter(convert_dt(collection_date_time) < (convert_dt(admission_date_time) + lubridate::hours(hours_after_admission)))

  laps <- lab %>%
    mutate(result_value = ifelse(
      # special treatment at some sites where Hematocrit results are expressed as a %. Hematocrit results are fractions, thus always <=1. Values > 1, if not converted, can contribute 23pts to the score.
      !is.na(result_unit) & result_unit == "%" & test_type_mapped_omop == 3009542 | is.na(result_unit) & result_value > 1 & test_type_mapped_omop == 3009542,
      as.numeric(result_value) / 100,
      result_value
    )) %>%
    mutate(
      score = case_when(
        test_type_mapped_omop == 3019550 ~ # Sodium
          laps_assign_test(
            result_value,
            breaks = c(0, 129, 132, 135, 146, 149, 155, Inf),
            points = c(10, 7, 6, 0, 6, 7, 10)
          ),
        test_type_mapped_omop == 3024641 ~ # Blood Urea Nitrogen (BUN)
          laps_assign_test(
            result_value,
            breaks = c(0, 6.43, 7.14, 14.28, 28.57, Inf),
            points = c(0, 4, 12, 19, 24)
          ),
        test_type_mapped_omop == 3020564 ~ # Creatinine
          laps_assign_test(
            result_value,
            breaks = c(0, 88.5, 176.7, 353.7, Inf),
            points = c(0, 1, 7, 5)
          ),
        test_type_mapped_omop == 3024561 ~ # Albumin
          laps_assign_test(
            result_value,
            breaks = c(0, 20, 25, Inf),
            points = c(23, 18, 0)
          ),
        test_type_mapped_omop == 3009542 ~ # Hematocrit
          laps_assign_test(
            result_value,
            breaks = c(0, 0.2, 0.4, 0.5, 0.6, Inf),
            points = c(7, 5, 0, 6, 23)
          ),
        test_type_mapped_omop == 3010813 ~ # White Blood Cell Count
          laps_assign_test(
            result_value,
            breaks = c(0, 2, 5, 13, Inf),
            points = c(29, 6, 0, 15)
          ),
        test_type_mapped_omop == 3019977 ~ # Arterial pH
          laps_assign_test(
            result_value,
            breaks = c(0, 7.15, 7.25, 7.35, 7.45, 7.55, Inf),
            points = c(30, 23, 16, 0, 11, 14)
          ),
        test_type_mapped_omop == 3027946 ~ # Arterial paCO2
          laps_assign_test(
            result_value,
            breaks = c(0, 25, 35, 45, 55, 65, Inf),
            points = c(5, 12, 0, 10, 9, 13)
          ),
        test_type_mapped_omop == 3027801 ~ # Arterial paO2
          laps_assign_test(
            result_value,
            breaks = c(0, 50, 120, Inf),
            points = c(13, 0, 18)
          ),
        test_type_mapped_omop %in% c(3013826, 3040151, 3018251) ~ # Glucose Random
          laps_assign_test(
            result_value,
            breaks = c(0, 2.22, 3.33, 11.1, Inf),
            points = c(16, 12, 0, 3)
          ),
        test_type_mapped_omop == 3006140 ~ # Bilirubin
          laps_assign_test(
            result_value,
            breaks = c(0, 34.21, 51.31, 85.52, 136.83, Inf),
            points = c(0, 10, 16, 22, 32)
          )
        # ,.default = NA # requires a particular version of dplyr, using default behaviour (ignore cases without match)
      )
    ) %>%
    mutate(
      test_type_mapped_omop = ifelse(
        # recode 3 types of glucose tests as one type of test
        test_type_mapped_omop %in% c(3013826, 3040151, 3018251),
        "GlucoseRandom",
        test_type_mapped_omop
      )
    ) %>%
    group_by(genc_id, test_type_mapped_omop) %>%
    summarise(score = max_result_value(score)) %>%
    mutate(score = ifelse(is.infinite(score), NA, score))

  bun <- lab %>%
    dplyr::filter(test_type_mapped_omop == 3024641) %>%
    mutate(result_value = gsub("<|>", "", result_value) %>% trimws() %>% as.numeric()) %>%
    group_by(genc_id) %>%
    summarise(max_bun = max_result_value(result_value)) %>%
    mutate(max_bun = ifelse(is.infinite(max_bun), NA, max_bun))

  creatinine <- lab %>%
    dplyr::filter(test_type_mapped_omop == 3020564) %>%
    mutate(result_value = gsub("<|>", "", result_value) %>% trimws() %>% as.numeric()) %>%
    group_by(genc_id) %>%
    summarise(min_creatinine = min_result_value(result_value)) %>%
    mutate(min_creatinine = ifelse(is.infinite(min_creatinine), NA, min_creatinine))

  bun_creatinine <- bun %>%
    full_join(creatinine, by = c("genc_id" = "genc_id")) %>%
    mutate(
      score = ifelse(max_bun / min_creatinine < 0.1, 0, 6),
      test_type_mapped_omop = "BUN/Creatinine"
    ) %>%
    select(genc_id, test_type_mapped_omop, score)


  laps <- bind_rows(
    laps %>% mutate(test_type_mapped_omop = as.character(test_type_mapped_omop)),
    bun_creatinine
  )


  if (component_wise) {
    return(laps)
  }

  res <- laps %>%
    group_by(genc_id) %>%
    summarise(mlaps = sum(score, na.rm = TRUE))

  return(res)
}


#' @title
#' Calculates minima when input vector is not empty, else returns NA.
#'
#' @details
#' This is a helper function to suppress default warning message from `base::min()`` function
#' when all elements in the input vector is NA, which can be problematic for unit testing.
#' Default to remove NA values in minima calculation.
#'
#' @param x (`numeric`)\cr
#' A vecctor of numerical values.
#'
#' @return (`numeric`)\cr
#' A numerical value or NA
#'
min_result_value <- function(x) {
  ifelse(all(is.na(x)), NA, min(x, na.rm = TRUE))
}

#' @title
#' Calculates maxima when input vector is not empty, else returns NA.
#'
#' @details
#' This is a helper function to suppress default warning message from `base::max()`` function
#' when all elements in the input vector is NA, which can be problematic for unit testing.
#' Default to remove NA values in maxima calculation.
#'
#' @param x (`numeric`)\cr
#' A vecctor of numerical values.
#'
#' @return (`numeric`)\cr
#' A numerical value or NA
#'
max_result_value <- function(x) {
  ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
}
