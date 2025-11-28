test_that("cohort_creation inclusions/exclusions are applied correctly", {
  set.seed(1)
  dummy_data <- dummy_ipadmdad(300, n_hospitals = 2, seed = 1) %>%
    data.table()

  cohort <- cohort_creation(
    cohort = list(
      dummy_data,
      dummy_data[dummy_data$gender == "F"],
      dummy_data[dummy_data$age > 65],
      dummy_data[grepl("^7", dummy_data$discharge_disposition)]
    ),
    labels = c(
      "All GEMINI encounters",
      "Gender = Female",
      "Age > 65",
      "In-hospital death"
    ),
    exclusion_flag = c(FALSE, FALSE, FALSE, TRUE)
  )

  expect_equal(nrow(cohort[[1]]), 107)

  expected_output <- data.table(`N (%)` = c("300", "163 (54.3%)", "121 (74.2%)", "-14 (-11.6%)", "107"))
  expect_equal(cohort[[2]][, 3], expected_output)
})

test_that("grouping works as expected", {
  set.seed(1)
  dummy_data <- dummy_ipadmdad(300, n_hospitals = 2, seed = 1) %>%
    data.table()

  # apply grouping by hospital_num
  cohort <- cohort_creation(
    cohort = list(
      dummy_data,
      dummy_data[dummy_data$gender == "F"],
      dummy_data[dummy_data$age > 65],
      dummy_data[!grepl("^7", dummy_data$discharge_disposition)]
    ),
    labels = c(
      "All GEMINI encounters",
      "Gender = Female",
      "Age > 65",
      "In-hospital death"
    ),
    exclusion_flag = c(FALSE, FALSE, TRUE, FALSE),
    group_var = "hospital_num"
  )

  expect_equal(nrow(cohort[[1]]), 38)

  expected_output <- data.table(
    `Overall N (%)` = c("300", "163 (54.3%)", "-121 (-74.2%)", "38 (90.5%)"),
    `1` = c("146", "84 (57.5%)", "-60 (-71.4%)", "22 (91.7%)"),
    `2` = c("154", "79 (51.3%)", "-61 (-77.2%)", "16 (88.9%)")
  )
  expect_equal(cohort[[2]][, 3:5], expected_output)
})


test_that("cell suppression works as expected", {
  set.seed(1)
  dummy_data <- dummy_ipadmdad(300, n_hospitals = 2, seed = 1) %>%
    data.table()

  expect_warning( # should produce warning
    cohort <- cohort_creation(
      cohort = list(
        dummy_data,
        dummy_data[dummy_data$gender == "F"],
        dummy_data[genc_id < 250, ],
        dummy_data[dummy_data$age > 65],
        dummy_data[grepl("^7", dummy_data$discharge_disposition)]
      ),
      labels = c(
        "All GEMINI encounters",
        "Gender = Female",
        "Subset of `genc_ids`",
        "Age > 65",
        "In-hospital death"
      ),
      exclusion_flag = c(FALSE, FALSE, TRUE, FALSE, TRUE),
    )
  )

  expected_output <- data.table(`N (%)` = c(
    "300", "163 (54.3%)", "-134 (-82.2%)", "24 (82.8%)", "N < 6", "21"
  ))
  expect_equal(cohort[[2]][, 3], expected_output)
})

test_that("show_prct works as expected", {
  set.seed(1)
  dummy_data <- dummy_ipadmdad(300, n_hospitals = 2, seed = 1) %>%
    data.table()

  cohort <- cohort_creation(
    cohort = list(
      dummy_data,
      dummy_data[dummy_data$gender == "F"],
      dummy_data[dummy_data$age > 65],
      dummy_data[grepl("^7", dummy_data$discharge_disposition)]
    ),
    labels = c(
      "All GEMINI encounters",
      "Gender = Female",
      "Age > 65",
      "In-hospital death"
    ),
    exclusion_flag = c(FALSE, FALSE, FALSE, TRUE),
    show_prct = FALSE
  )

  expected_output <- data.table(N = c(
    "300", "163", "121", "-14", "107"
  ))
  expect_equal(cohort[[2]][, 3], expected_output)
})
