test_that("cohort_creation inclusions/exclusions are applied correctly", {
  set.seed(1)
  dummy_data <- gemSim::dummy_admdad(300, n_hospitals = 2) %>%
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

  expect_equal(nrow(cohort[[1]]), 82)

  expected_output <- data.table(
    `N (%)` = c("300", "156 (52%)", "93 (59.6%)", "-11 (-11.8%)", "82")
  )
  expect_equal(cohort[[2]][, 3], expected_output)
})

test_that("grouping works as expected", {
  set.seed(1)
  dummy_data <- gemSim::dummy_admdad(300, n_hospitals = 2) %>%
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

  expect_equal(nrow(cohort[[1]]), 58)

  expected_output <- data.table(
    `Overall N (%)` = c("300", "156 (52%)", "-93 (-59.6%)", "58 (92.1%)"),
    `1` = c("146", "84 (57.5%)", "-60 (-71.4%)", "22 (91.7%)"),
    `2` = c("154", "72 (46.8%)", "-33 (-45.8%)", "36 (92.3%)")
  )
  expect_equal(cohort[[2]][, 3:5], expected_output)
})


test_that("cell suppression works as expected", {
  set.seed(1)
  dummy_data <- gemSim::dummy_admdad(300, n_hospitals = 2) %>%
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
    "300", "156 (52%)", "-135 (-86.5%)", "11 (52.4%)", "N < 6", "9"
  ))
  expect_equal(cohort[[2]][, 3], expected_output)
})

test_that("show_prct works as expected", {
  set.seed(1)
  dummy_data <- gemSim::dummy_admdad(300, n_hospitals = 2) %>%
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
    "300", "156", "93", "-11", "82"
  ))
  expect_equal(cohort[[2]][, 3], expected_output)
})
