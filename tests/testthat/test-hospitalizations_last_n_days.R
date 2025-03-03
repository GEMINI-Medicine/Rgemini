test_that("function returns correct values", {
  # create dummy admdad - 10 unique genc_id's, 4 unique patient id's. Will have
  # patient with 1 previous hospitalization, patient with 2 previous, patient with 3 previous
  set.seed(1)
  cohort <- data.table(
    genc_id = sample(c(1:10)),
    patient_id_hashed = paste0("hash_", c(1, 1, 2, 2, 2, 3, 3, 3, 3, 4)),
    admission_date_time = c(
      "2022-01-01 00:01", "2023-01-05 12:34", "2023-04-23 09:38",
      "2023-08-21 05:30", "2023-10-04 23:23", "2022-12-12 00:30",
      "2022-07-24 05:30", "2022-07-26 13:23", "2022-12-30 22:21",
      "2023-06-11 15:30"
    ),
    discharge_date_time = c(
      "2022-01-11 20:11", "2023-01-06 12:00", "2023-04-26 06:31",
      "2023-09-21 12:30", "2023-10-06 15:13", "2022-12-16 16:46",
      "2022-07-24 05:30", "2022-07-27 15:53", "2022-12-31 13:41",
      "2023-06-12 12:37"
    )
  )

  ### Set window to ~10 years, have maximum number of hospitalizations
  check_part1 <- hospitalizations_last_n_days(cohort, n_days = 3650)

  ## Check 1: have 4 unique patient ids, there should be 4 rows with 0 previous hospitalizations
  expect_equal(nrow(check_part1[n_hospitalizations == "0"]), 4)

  ## Check 2: have 3 patient ids with 2+ previous hospitalizations, function should return
  # 3 encounters with ONE previous hospitalization
  expect_equal(nrow(check_part1[n_hospitalizations == "1"]), 3)

  ## Check 3: 2 patients ids with 3+ previous hospitalizations. One has 3 total, one has 4,
  # expect 3 rows with 2+ previous hospitalizations
  expect_equal(nrow(check_part1[n_hospitalizations == "2+"]), 3)

  ### Set window to be really small (0 days), should have no rows with n_hospitalizations > 0
  check_part2 <- hospitalizations_last_n_days(cohort, n_days = 0)

  ## Check 4: Expect the function to only have 0 in n_hospitalizations column
  expect_equal(nrow(check_part2[n_hospitalizations == 0]), 10)
})

test_that("function quits when missing important column", {
  ## Check 1: Make sure function quits if it is missing genc_id or patient_id_hashed
  # create dummy admdad
  set.seed(1)
  cohort <- data.table(dummy_ipadmdad())

  # add in patient_id_hashed
  set.seed(1)
  cohort[, patient_id_hashed := paste0("hash_", sample(1:800, size = .N, replace = T))]

  ## try removing genc_id from cohort, should receive an error
  expect_error(hospitalizations_last_n_days(cohort[, .(patient_id_hashed)]))

  ## Check 2: Make sure function quits when missing inputted column (typo in input)
  expect_error(hospitalizations_last_n_days(cohort, admit_date = "admission_date_tim"))

  ## Check 3: Make sure function quits when missing date column (discharge dt column not in table)
  expect_error(hospitalizations_last_n_days(cohort[, .(genc_id, admission_date_time, patient_id_hashed)]))

  ## Check 4: Function quits when admission_date is given instead of admission_date_time
  cohort[, admission_date := substr(admission_date_time, 1, 10)]

  expect_error(hospitalizations_last_n_days(cohort, admit_dt = "admission_date"))

  ## Check 5: Function quits when discharge_date is given instead of discharge_date_time
  cohort[, discharge_date := substr(discharge_date_time, 1, 10)]

  expect_error(hospitalizations_last_n_days(cohort, discharge_dt = "discharge_date"))
})


test_that("function returns same number of rows as input cohort", {
  # create dummy admdad
  set.seed(1)
  cohort <- data.table(dummy_ipadmdad())

  # add in patient_id_hashed
  set.seed(1)
  cohort[, patient_id_hashed := paste0("hash_", sample(1:800, size = .N, replace = T))]


  ## run function
  check <- hospitalizations_last_n_days(cohort)

  ## Check: Number of rows in function output should equal number of rows in cohort
  expect_equal(nrow(check), nrow(cohort))
})


test_that("function runs as expected when input date is not named admission_date_time or discharge_date_time", {
  # create dummy admdad
  set.seed(1)
  cohort <- data.table(dummy_ipadmdad())

  # add in patient_id_hashed
  set.seed(1)
  cohort[, patient_id_hashed := paste0("hash_", sample(1:800, size = .N, replace = T))]

  # rename admission_date_time to admission_date
  setnames(cohort, "admission_date_time", "admission_dt")

  ## run function - should not throw any errors
  expect_silent(hospitalizations_last_n_days(cohort, admit_dt = "admission_dt"))
})
