test_that("derived COVID flags are accurate", {

  # create dummy data
  set.seed(1)
  cohort <- data.table(genc_id = c(1, 2, 3, 4, 5, 6))
  ipdiagnosis <- data.table(genc_id = c(1, 2, 3, 4, 5),
                            diagnosis_code = c("U071", "U072", "Z53", "A13", "C53"))
  erdiagnosis <- data.table(genc_id = c(3, 4, 5),
                            er_diagnosis_code = c("U071", "U072", "U072"))

  # derive COVID flags
  check <- covid_flag(cohort, ipdiagnosis, erdiagnosis)


  ## Check genc_ids with COVID == TRUE
  expect_equal(sum(check$covid_icd_confirmed_flag, na.rm = TRUE), 2)
  expect_equal(sum(check$covid_icd_suspected_flag, na.rm = TRUE), 3)

  ## Check NA for genc_ids without diagnosis code
  expect_equal(c(check[genc_id == 6, covid_icd_confirmed_flag],
                 check[genc_id == 6, covid_icd_suspected_flag]),
               c(NA, NA))

  ## check without ER diagnoses
  # Not specifying erdiag = NULL should result in error
  expect_error(covid_flag(cohort = ipadmdad, ipdiag = ipdiagnosis))

  # Not including ER diagnoses should result in lower count
  check2 <- covid_flag(cohort, ipdiag = ipdiagnosis, erdiag = NULL)
  expect_equal(sum(check2$covid_icd_confirmed_flag, na.rm = TRUE), 1)
  expect_equal(sum(check2$covid_icd_suspected_flag, na.rm = TRUE), 1)

})

