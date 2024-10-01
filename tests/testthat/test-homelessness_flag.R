test_that("derived homelessness flag is accurate", {

  # set seed for reproducibility
  set.seed(1624)

  # create dummy data
  cohort <- data.table(genc_id = c(1, 2, 3, 4, 5, 6))
  ipdiagnosis <- data.table(genc_id = c(1, 2, 3, 4, 5),
                            diagnosis_code = c("Z590", "Z591", "Z53", "A13", "C53"))
  erdiagnosis <- data.table(genc_id = c(3, 4, 5),
                            er_diagnosis_code = c("Z590", "Z591", "U072"))

  # check derived homelessness flags match
  check <- homelessness_flag(cohort, ipdiagnosis, erdiagnosis)

  # check genc_ids experiencing homelessness matches expected num
  expect_equal(sum(check$homelessness_icd_flag, na.rm = TRUE), 4)

  # Check NA for genc_ids without diagnosis code
  expect_equal(check[genc_id == 6, homelessness_icd_flag], NA)

  # check without ER diagnoses
  # Not specifying erdiag = NULL should result in error
  expect_error(homelessness_flag(cohort = ipadmdad, ipdiag = ipdiagnosis))

  # Not including ER diagnoses should result in lower count
  check2 <- homelessness_flag(cohort, ipdiag = ipdiagnosis, erdiag = NULL)
  expect_equal(sum(check2$homelessness_icd_flag, na.rm = TRUE), 2)

  # check handling of duplicates
  duplicated_ipdiag <- data.table(genc_id = c(1, 1, 2), diagnosis_code = c("Z590", "Z590", "Z591"))
  check3 <- homelessness_flag(cohort, duplicated_ipdiag, erdiag = NULL)
  expect_equal(sum(check3$homelessness_icd_flag, na.rm = TRUE), 2)

})

