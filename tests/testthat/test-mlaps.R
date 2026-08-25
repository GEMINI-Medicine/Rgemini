####### test 1
testthat::test_that("Scoring scheme for each test is correct", {
  admdad <- data.table(
    genc_id = 1,
    admission_date_time = ymd_hm("2023-01-02 00:00")
  )
  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3006140, 3009542, 3010813, 3019550, 3019977, 3020564, 3024561, 3024641, 3027801, 3027946, 3013826),
    result_value = c(70, 0.5, 6, 128, 8, 353.7, 19, 6.4, 120.1, 45, 3),
    result_unit = "mmol/L",
    collection_date_time = "2023-01-01 00:00"
  )

  res <- mlaps(
    admdad, lab,
    hours_after_admission = 0, component_wise = TRUE
  )
  testthat::expect_equal(
    res$score,
    c(16, 6, 0, 10, 14, 5, 23, 0, 18, 10, 12, 0)
  )
})

####### test 2
testthat::test_that("Only the max value within specified time window is taken", {
  set.seed(1) # ensure reproducibility

  admdad <- data.table(
    genc_id = 1,
    admission_date_time = ymd_hm("2023-01-02 00:00")
  )

  .table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = 3024641,
    result_value = c(7, 8, 15, 30),
    result_unit = "mmol/L",
    collection_date_time = ymd_hm("2023-01-02 08:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 4, replace = TRUE) # all tests taken after admission
  )

  # pre-admission: hours_offset=0 [no tests -> mlaps = 0]
  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(nrow(res), 0)

  # within 24 hours: hours_offset=24
  res <- mlaps(admdad, lab, hours_after_admission = 24, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 19)

  # within 36 hours: hours_offset=36
  res <- mlaps(admdad, lab, hours_after_admission = 36, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 24)
})


####### test 3
testthat::test_that("Only the max is taken for multiple glucose random tests", {
  admdad <- data.table(
    genc_id = 1,
    admission_date_time = ymd_hm("2023-01-02 00:00")
  )

  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3013826, 3040151, 3018251),
    result_value = c(3, 1, 12),
    result_unit = "mmol/L",
    collection_date_time = ymd_hm("2023-01-01 00:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 3, replace = TRUE) # all tests taken before admission
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, 16)
})

####### test 4
testthat::test_that("BUN/creatinine is added", {
  admdad <- data.table(
    genc_id = 1,
    admission_date_time = ymd_hm("2023-01-02 00:00")
  )

  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3024641, 3020564),
    result_value = c(21, 200),
    result_unit = "mmol/L",
    collection_date_time = ymd_hm("2023-01-01 00:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 2, replace = TRUE) # all tests taken before admission
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, c(7, 19, 6))

  ## only one of the two tests is present
  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3024641),
    result_value = c(21),
    result_unit = "mmol/L",
    collection_date_time = ymd_hm("2023-01-01 00:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 1, replace = TRUE) # all tests taken before admission
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 19)

  ##
  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3020564),
    result_value = c(200),
    result_unit = "mmol/L",
    collection_date_time = ymd_hm("2023-01-01 00:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 1, replace = TRUE) # all tests taken before admission
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 7)
})

####### test 5
testthat::test_that("Special unit for Hematocrit is converted into percentages", {
  admdad <- data.table(
    genc_id = 1,
    admission_date_time = ymd_hm("2023-01-02 00:00")
  )

  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3009542),
    result_value = c(10, 0.5),
    result_unit = c(NA, "L/L"),
    collection_date_time = ymd_hm("2023-01-01 00:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 2, replace = TRUE) # all tests taken before admission
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, 7)

  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3009542),
    result_value = c(55, 0.3),
    result_unit = c("%", "L/L"),
    collection_date_time = ymd_hm("2023-01-01 00:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 2, replace = TRUE) # all tests taken before admission
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, 6)
})

####### test 6
testthat::test_that("Special cases in result_value are properly handled", {
  admdad <- data.table(
    genc_id = 1,
    admission_date_time = ymd_hm("2023-01-02 00:00")
  )

  lab <- data.table(
    genc_id = 1,
    test_name_raw = "lab_test_name",
    test_type_mapped_omop = c(3019550, 3020564, 3024561, 3024641, 3040151),
    result_value = c(NA, ">400", "FINAL", "<2", ""), # special cases being tested: NAs, </> signs in results, non-numeric
    result_unit = "mmol/L",
    collection_date_time = ymd_hm("2023-01-01 00:00", tz = "UTC") + sample(0:(24 * 60 * 60 - 1), size = 5, replace = TRUE) # all tests taken before admission
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_equal(res$score, c(NA, 5, NA, 0, NA, 0))

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 5)
})

####### test 7
testthat::test_that("POC names are filtered out", {
  admdad <- data.table(
    genc_id = 1,
    admission_date_time = ymd_hm("2023-01-02 00:00")
  )

  lab <- data.table(
    genc_id = 1,
    test_name_raw = c(rep("arterial_blood_gas", 8), rep("POC arterial_blood_gas", 3)),
    test_type_mapped_omop = c(3006140, 3009542, 3010813, 3019550, 3013826, 3020564, 3024561, 3024641, 3027801, 3019977, 3027946),
    result_value = c(70, 0.5, 6, 128, 8, 353.7, 19, 6.4, 120.1, 45, 3),
    result_unit = "mmol/L",
    collection_date_time = "2023-01-01 00:00"
  )

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = TRUE)
  testthat::expect_false(any(c("3019977", "3027946", "3027801") %in% res$test_type_mapped_omop))

  res <- mlaps(admdad, lab, hours_after_admission = 0, component_wise = FALSE)
  testthat::expect_equal(res$mlaps, 60)
})
