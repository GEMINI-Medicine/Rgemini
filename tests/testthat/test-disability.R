test_that("global and component-wise outputs align", {

  ## Check 1: Make sure that global & component-wise flags don't contradict each other
  # create dummy data
  set.seed(1)
  ipdiagnosis <- dummy_diag(nid = 100, nrow = 400)
  erdiagnosis <- dummy_diag(nid = 80, nrow = 200, ipdiagnosis = FALSE)
  cohort <- data.table(genc_id = c(unique(ipdiagnosis$genc_id), 888, 999)) # add some genc_ids with no diagnosis entries

  check1_no_cat <- disability(cohort, ipdiag = ipdiagnosis, erdiag = erdiagnosis, component_wise = FALSE)
  check1_cat <- disability(cohort, ipdiag = ipdiagnosis, erdiag = erdiagnosis, component_wise = TRUE)

  ## Check genc_ids with disability == TRUE
  expect_equal(c(sum(check1_no_cat$disability, na.rm = TRUE), length(unique(check1_cat$genc_id))), c(27, 27))

  ## All genc_ids where global disability flag = TRUE should exist in component-wise output (and v.v.)
  expect_true(all(check1_no_cat[disability == TRUE]$genc_id %in% check1_cat$genc_id == TRUE))
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
  expect_equal(check2$genc_id, c(1,1,2,2,2,3,4,5,5,5,5,5))
  expect_equal(check2$disability_category, c("Developmental Disabilities",
                                             "Developmental Disabilities",
                                             "Physical disability - Musculoskeletal disorders",
                                             "Physical disability - Musculoskeletal disorders",
                                             "Physical disability - Congenital Anomalies",
                                             "Physical disability - Neurological disorders",
                                             "Physical disability - Neurological disorders",
                                             "Sensory disabilities - Vision impairments",
                                             "Sensory disabilities - Hearing impairments",
                                             "Sensory disabilities - Vision impairments",
                                             "Physical disability - Permanent Injuries",
                                             "Physical disability - Permanent Injuries"))

  })


test_that("returned with disability = FALSE", {

  ## Check 3: Unit test for some diagnosis codes that should have disability = FALSE
  set.seed(10)
  ipdiag <- dummy_diag(nid=5, nrow=5, ipdiagnosis=T, pattern ="^I4|^A0|^B92|^K8|^Q2|^G65|^H92|^F83|^Z994|^H92|^E12|^Z88")

  check3 <- disability(cohort = data.table(genc_id = unique(ipdiag$genc_id)),
                       ipdiag,
                       erdiag = NULL, component_wise = FALSE)

  expect_true(all(check3$disability == FALSE))

})


test_that("missing erdiag input results in error", {

  ## Check 4: Not specifying erdiag = NULL should result in error
  expect_error(disability(cohort = ipadmdad, ipdiag = ipdiagnosis))

})





