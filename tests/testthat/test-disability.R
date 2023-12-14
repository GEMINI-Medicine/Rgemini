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
  set.seed(2)
  ipdiag <- dummy_diag(nid=5, nrow=20, ipdiagnosis=T, pattern ="^F840|^S07|^M05|^Q66|^H90|^H30|^G25")
  check2 <- disability(cohort = data.table(genc_id = unique(ipdiag$genc_id)),
                       ipdiag,
                       NULL, component_wise = TRUE)

  expect_true(nrow(check2) == nrow(ipdiag)) # all rows from cohort input should be returned

  # check number of returned rows per genc_id
  expect_equal(check2$genc_id, c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 5, 5, 5))

  # check subcategories
  expect_equal(unique(check2[grepl('^F840', diagnosis_code), disability_category]), "Developmental Disabilities")
  expect_equal(unique(check2[grepl('^S07', diagnosis_code), disability_category]), "Physical disability - Permanent Injuries")
  expect_equal(unique(check2[grepl('^M05', diagnosis_code), disability_category]), "Physical disability - Musculoskeletal disorders")
  expect_equal(unique(check2[grepl('^Q66', diagnosis_code), disability_category]), "Physical disability - Congenital Anomalies")
  expect_equal(unique(check2[grepl('^H90', diagnosis_code), disability_category]), "Sensory disabilities - Hearing impairments")
  expect_equal(unique(check2[grepl('^H30', diagnosis_code), disability_category]), "Sensory disabilities - Vision impairments")
  expect_equal(unique(check2[grepl('^G25', diagnosis_code), disability_category]), "Physical disability - Neurological disorders")

  })


test_that("returned with disability = FALSE", {

  ## Check 3: Unit test for some diagnosis codes that should have disability = FALSE
  set.seed(3)
  ipdiag <- dummy_diag(nid=5, nrow=5, ipdiagnosis=T, pattern ="^I4|^A0|^B92|^K8|^Q2|^G65|^H92|^F83|^Z994|^H92|^E12|^Z88")

  check3 <- disability(cohort = data.table(genc_id = unique(ipdiag$genc_id)),
                       ipdiag,
                       NULL, component_wise = FALSE)

  expect_true(all(check3$disability == FALSE))

})


test_that("missing erdiag input results in error", {

  ## Check 4: Not specifying erdiag = NULL should result in error
  expect_error(disability(cohort = ipadmdad, ipdiag = ipdiagnosis))

})





