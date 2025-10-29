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

  expect_equal(nrow(cohort[[1]]), 83)

  expected_output <- data.table(`N (%)` = c("300", "150 (50%)", "95 (63.3%)", "-12 (-12.6%)", "83"))
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

  expect_equal(nrow(cohort[[1]]), 51)

  expected_output <- data.table(
    `Overall N (%)` = c("300", "150 (50%)", "-95 (-63.3%)", "51 (92.7%)"),
    `1` = c("155", "75 (48.4%)", "-47 (-62.7%)", "26 (92.9%)"),
    `2` = c("145", "75 (51.7%)", "-48 (-64%)", "25 (92.6%)")
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
    "300", "150 (50%)", "-124 (-82.7%)", "15 (57.7%)", "N < 6", "13"
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
    "300", "150", "95", "-12", "83"
  ))
  expect_equal(cohort[[2]][, 3], expected_output)
})
