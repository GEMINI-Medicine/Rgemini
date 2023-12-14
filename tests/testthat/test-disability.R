test_that("global and component-wise outputs align", {

  ## Check 1: Make sure that global & component-wise flags don't contradict each other
  # create dummy data
  set.seed(1)
  ipdiagnosis <- dummy_diag(nid = 100, nrow = 400)
  erdiagnosis <- dummy_diag(nid = 80, nrow = 200, ipdiagnosis = FALSE)
  cohort <- data.table(genc_id = c(unique(ipdiagnosis$genc_id), 888, 999)) # add some genc_ids with no diagnosis entries

  check1_no_cat <- disability(cohort, ipdiag = ipdiagnosis, erdiag = erdiagnosis, component_wise = FALSE)
  check1_cat <- disability(cohort, ipdiag = ipdiagnosis, erdiag = erdiagnosis, component_wise = TRUE)

  ## All genc_ids where global disability flag = TRUE should exist in component-wise output (and v.v.)
  expect_true(all(check1_no_cat[disability == TRUE]$genc_id %in% check1_cat$genc_id == TRUE))
  expect_true(all(check1_no_cat[disability == FALSE]$genc_id %in% check1_cat$genc_id == FALSE))
  expect_true(all(check1_cat$genc_id %in% check1_no_cat[disability == TRUE]$genc_id == TRUE))

  ## If no entry in diagnosis table, disability should be NA in global disability output
  expect_equal(unique(check1_no_cat[genc_id == 888 | genc_id == 999, disability]), NA)

})


test_that("returned with disability = TRUE", {

  ## Check 2: Unit test for some diagnosis codes that should have disability = TRUE
  check2 <- disability(cohort = data.table(genc_id = c(1,2,3,4,5)),
                       ipdiag = data.table(genc_id = c(1,1,2,2,2,3,4,5,5,5,5,5),
                                           diagnosis_code = c('F7001', 'Q8723','Q66', 'E2204', 'M23836', 'G60', 'G242','S029', 'Z998', 'H90132', 'Q150', 'E10352')),
                       erdiag = NULL, component_wise = TRUE)

  expect_true(nrow(check2) == 12) # all rows from cohort input should be returned

  # check number of returned rows per genc_id
  expect_true(nrow(check2[genc_id == 1,]) == 2)
  expect_true(nrow(check2[genc_id == 2,]) == 3)
  expect_true(nrow(check2[genc_id == 3,]) == 1)
  expect_true(nrow(check2[genc_id == 4,]) == 1)
  expect_true(nrow(check2[genc_id == 5,]) == 5)

  # check disability categories
  expect_true(unique(check2[diagnosis_code %in% c("F7001", "Q8723"), disability_category]) == "Developmental Disabilities")
  expect_true(check2[diagnosis_code %in% c("Q66"), disability_category] == "Physical disability - Congenital Anomalies")
  expect_true(unique(check2[diagnosis_code %in% c("E2204", "M23836"), disability_category]) == "Physical disability - Musculoskeletal disorders")
  expect_true(unique(check2[diagnosis_code %in% c("G60", "G242"), disability_category]) == "Physical disability - Neurological disorders")
  expect_true(unique(check2[diagnosis_code %in% c("S029", "Z998"), disability_category]) == "Physical disability - Permanent Injuries")
  expect_true(check2[diagnosis_code %in% c("H90132"), disability_category] == "Sensory disabilities - Hearing impairments")
  expect_true(unique(check2[diagnosis_code %in% c("Q150", "E10352"), disability_category]) == "Sensory disabilities - Vision impairments")

  })


test_that("returned with disability = FALSE", {
  ## Check 3: Unit test for some diagnosis codes that should have disability = FALSE
  check3 <- disability(cohort = data.table(genc_id = c(1,2,3,4,5,6,7,8,9,10,11,12)),
                       ipdiag = data.table(genc_id = c(1,2,3,4,5,6,7,8,9,10,11,12),
                                           diagnosis_code = c('I4891', 'A021','B962', 'K8308', 'Q68', 'G65', 'H91','F83', 'Z99', 'H92', 'E12', 'Z89')),
                       erdiag = NULL, component_wise = TRUE)

  expect_true(all(check3$disability == FALSE))
})


test_that("missing erdiag input results in error", {

  ## Check 4: Not specifying erdiag = NULL should result in error
  expect_error(disability(cohort = ipadmdad, ipdiag = ipdiagnosis))

})





