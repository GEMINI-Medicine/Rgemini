####### test 1
testthat::test_that("Scoring scheme for each test is correct", {
  id <- 1
  omop <- c(3006140, 3009542, 3010813, 3019550, 3019977, 3020564, 3024561, 3024641, 3027801, 3027946, 3013826)
  value <- c(70, 0.5, 6, 128,  8, 353.7, 19, 6.4, 120.1, 45, 3)
  unit <- "mmol/L"
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop, value, unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, c(16, 6, 0, 10, 14, 5, 23, 0, 18, 10, 12, 0))
})

####### test 2
testthat::test_that("Only the max value within specified time window is taken", {
  id <- 1
  omop <- 3024641
  value <- c(7,8,15,30)
  unit <- "mmol/L"
  mintime <- "2023-01-02 08:00" # some taken within some after 24 hours
  admtime <-"2023-01-02 00:00"
  set.seed(1) #ensure reproducibility

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop,value,unit, mintime)

  #pre-admission: hours_offset=0
  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(nrow(res), 0)

  #within 24 hours: hours_offset=24
  res <- mlaps(admdad, lab, hours_after_admission = 24, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 19)

  #within 36 hours: hours_offset=36
  res <- mlaps(admdad, lab, hours_after_admission = 36, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 24)
})

####### test 3
testthat::test_that("Only the max is taken for multiple glucose random tests", {
  id <- 1
  omop <- c(3013826, 3040151, 3018251)
  value <- c(3, 1, 12)
  unit <- "mmol/L"
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop, value, unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, 16)
})

####### test 4
testthat::test_that("BUN/creatinine is added", {
  id <- 1
  omop <- c(3024641, 3020564)
  value <- c(21, 200)
  unit <- "mmol/L"
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop,value,unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, c(7, 19, 6))

  ## only one of the two tests is present
  id <- 1
  omop <- 3024641
  value <- 21
  unit <- "mmol/L"
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop,value,unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 19)

  ##
  id <- 1
  omop <- 3020564
  value <- 200
  unit <- "mmol/L"
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop,value,unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 7)
})

####### test 5
testthat::test_that("Special unit for Hematocrit is converted into percentages", {
  id <- 1
  omop <- c(3009542)
  value <- c(10, 0.5)
  unit <- c(NA,"L/L")
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop,value,unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, 7)

  id <- 1
  omop <- c(3009542)
  value <- c(55, 0.3)
  unit <- c("%","L/L")
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop,value,unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, 6)
})

####### test 6
testthat::test_that("Special cases in result_value are properly handled", {
  # special cases being tested: NAs, </> signs in results, non-numeric
  id <- 1
  omop <- c(3019550, 3020564, 3024561, 3024641, 3040151)
  value <- c(NA, ">400", "FINAL", "<2", "")
  unit <- "mmol/L"
  mintime <- "2023-01-01 00:00"
  admtime <-"2023-01-02 00:00"

  admdad <- dummy_admdad(id, admtime)
  lab <- dummy_lab(id, omop,value,unit, mintime)

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, c(NA, 5, NA, 0, NA, 0))

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 5)
})
