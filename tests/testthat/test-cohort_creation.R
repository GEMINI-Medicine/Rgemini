test_that("cohort_creation inclusions/exclusions are applied correctly", {
  set.seed(1)
  dummy_data <- Rgemini::dummy_ipadmdad(300, n_hospitals = 2) %>%
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

  expect_equal(nrow(cohort[[1]]), 96)

  expected_output <- data.table(`N (%)` = c("300", "156 (52%)", "103 (66%)", "-7 (-6.8%)", "96"))
  expect_equal(cohort[[2]][, 3], expected_output)
})

test_that("grouping works as expected", {
  set.seed(1)
  dummy_data <- Rgemini::dummy_ipadmdad(300, n_hospitals = 2) %>%
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

  expect_equal(nrow(cohort[[1]]), 49)

  expected_output <- data.table(
    `Overall N (%)` = c("300", "156 (52%)", "-103 (-66%)", "49 (92.5%)"),
    `1` = c("155", "77 (49.7%)", "-41 (-53.2%)", "34 (94.4%)"),
    `2` = c("145", "79 (54.5%)", "-62 (-78.5%)", "15 (88.2%)")
  )
  expect_equal(cohort[[2]][, 3:5], expected_output)
})


test_that("cell suppression works as expected", {
  set.seed(1)
  dummy_data <- Rgemini::dummy_ipadmdad(300, n_hospitals = 2) %>%
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
    "300", "156 (52%)", "-135 (-86.5%)", "12 (57.1%)", "N < 6", "11"
  ))
  expect_equal(cohort[[2]][, 3], expected_output)
})

test_that("show_prct works as expected", {
  set.seed(1)
  dummy_data <- Rgemini::dummy_ipadmdad(300, n_hospitals = 2) %>%
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
    "300", "156", "103", "-7", "96"
  ))
  expect_equal(cohort[[2]][, 3], expected_output)
})
