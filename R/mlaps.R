
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
  3006140  # Bilirubin
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
#' A wrapper around the `mlaps()` function which breaks down the calculation of LAPS on a
#' large number of encounters by hospital-year. This avoids memory issues that can be caused
#' by loading large chunks of the lab table.
#'
#' @param db (`DBIConnection`)\cr
#' RPostgres DB connection.
#'
#' @param cohort (`numeric`)\cr
#' A numeric vector of `genc_ids` to restrict the calculation to.
#'
#' @param hours_after_admission (`numeric`)\cr
#' Consider lab tests collected **up to** `hours_after_admission` hours after admission in the
#' calculation.
#'
#' @param output_laps_components (`logical`)\cr
#' Does not aggregate the score and instead outputs for each LAPS component (test), its contribution
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
#' @import DBI RPostgreSQL data.table dplyr
#' @export
#'
loop_mlaps <- function(db, cohort = NULL, hours_after_admission = 0, output_laps_components = FALSE) {

  admdad <- DBI::dbGetQuery(
    db,
    paste(
      "SELECT
        genc_id,
        admission_date_time,
        EXTRACT(YEAR FROM discharge_date_time::DATE) AS year,
        hospital_id
      FROM admdad",
      if (!is.null(cohort)) {
        paste("WHERE genc_id IN (", paste(cohort, collapse = ", "), ")")
      }
    )
  ) %>%
    as.data.table()

  hospital_years <- admdad %>%
    select(hospital_id, year) %>%
    unique()

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
          FROM lab l
          INNER JOIN admdad a
            ON l.genc_id = a.genc_id
          WHERE l.test_type_mapped_omop IN (", paste(LAPS_OMOP_CONCEPTS, collapse = ", "), ")",
          paste0("AND a.hospital_id = '", hospital_id, "'"),
          "AND EXTRACT(YEAR FROM a.discharge_date_time::DATE) = ", year,
          if (!is.null(cohort)) {
            paste("AND l.genc_id IN (", paste(cohort, collapse = ", "), ")")
          }
        )
      ) %>%
        as.data.table()

      res <- mlaps(
        admdad %>% dplyr::filter(hospital_id == hospital_id, year == year),
        lab,
        hours_offset = hours_after_admission,
        componentwise = output_laps_components
      )
    }
  )

  return(res)
}


#' @title
#' mLAPS
#'
#' @details
#' Laboratory based Acute Physiology Score (LAPS) uses 14 lab test values.
#' In this modified version, High senstive Troponin tests are ignored and treated as normal.
#'
#' @param admdad (`data.frame`)\cr
#' Table equivalent to a subset of the `admdad` table defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' @param lab (`data.table`, `data.frame`)\cr
#' Table equivalent to a subset of the `lab` table defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' @param hours_offset (`numeric`)\cr
#' Consider lab tests collected **up to** `hours_after_admission` hours after admission in the
#'
#' @param componentwise (`logical`)\cr
#' Does not aggregate the score and instead outputs for each LAPS component (test), its contribution
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
#' @import dplyr
#' @importFrom lubridate ymd_hm hours
#' @export
#'
#' @references
#' https://pubmed.ncbi.nlm.nih.gov/18388836/
#'
#' @examples
#'
mlaps <- function(admdad, lab, hours_offset = 0, componentwise = FALSE) {

  lab <- lab %>%
    select(test_type_mapped_omop, genc_id, result_value, result_unit, collection_date_time) %>%
    filter(test_type_mapped_omop %in% LAPS_OMOP_CONCEPTS) %>%
    left_join(
      admdad %>% select(genc_id, admission_date_time),
      by = "genc_id"
    ) %>%
    filter(lubridate::ymd_hm(collection_date_time) < (lubridate::ymd_hm(admission_date_time) + lubridate::hours(hours_offset)))

  laps <- lab %>%
    mutate(result_value = ifelse(
      # special treatment at some sites where Hematocrit results are expressed as a %. Hematocrit results are fractions, thus always <=1. Values > 1, if not converted, can contribute 23pts to the score.
      !is.na(result_unit) & result_unit == "%" & test_type_mapped_omop == 3009542 | is.na(result_unit) & result_value > 1 & test_type_mapped_omop == 3009542,
      as.numeric(result_value) / 100,
      result_value)
    ) %>%
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
        #,.default = NA # requires a particular version of dplyr, using default behaviour (ignore cases without match)
      )) %>%
    mutate(
      test_type_mapped_omop = ifelse(
        # recode 3 types of glucose tests as one type of test
        test_type_mapped_omop %in% c(3013826, 3040151, 3018251),
        "GlucoseRandom",
        test_type_mapped_omop
      )
    ) %>%
    group_by(genc_id, test_type_mapped_omop) %>%
    summarise(score = max(score, na.rm = TRUE) %>% suppressWarnings()) %>%
    mutate(score = ifelse(is.infinite(score), NA, score))

  bun <- lab %>%
    dplyr::filter(test_type_mapped_omop == 3024641) %>%
    mutate(result_value = gsub("<|>", "", result_value) %>% trimws() %>% as.numeric()) %>%
    group_by(genc_id) %>%
    summarise(max_bun = max(result_value, na.rm = TRUE) %>% suppressWarnings()) %>%
    mutate(max_bun = ifelse(is.infinite(max_bun), NA, max_bun))

  creatinine <- lab %>%
    dplyr::filter(test_type_mapped_omop == 3020564) %>%
    mutate(result_value = gsub("<|>", "", result_value) %>% trimws() %>% as.numeric()) %>%
    group_by(genc_id) %>%
    summarise(min_creatinine = min(result_value, na.rm = TRUE) %>% suppressWarnings()) %>%
    mutate(min_creatinine = ifelse(is.infinite(min_creatinine), NA, min_creatinine))

  bun_creatinine <- bun %>%
    full_join(creatinine, by = c("genc_id" = "genc_id")) %>%
    mutate(
      score = ifelse(max_bun/min_creatinine < 0.1, 0, 6),
      test_type_mapped_omop = "BUN/Creatinine"
    ) %>%
    select(genc_id, test_type_mapped_omop, score)


  laps <- bind_rows(
    laps %>% mutate(test_type_mapped_omop = as.character(test_type_mapped_omop)),
    bun_creatinine
  )


  if (componentwise) {
    return(laps)
  }

  res <- laps %>%
    group_by(genc_id) %>%
    summarise(mlaps = sum(score, na.rm = TRUE))

  return(res)
}
