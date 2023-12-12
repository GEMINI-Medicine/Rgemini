test_that("global and component-wise flags align", {

  ## Check 1: Make sure that global & component-wise flags don't contradict each other
  # create dummy data
  set.seed(1)
  ipdiagnosis <- dummy_diag(nid = 100, nrow = 400)
  erdiagnosis <- dummy_diag(nid = 80, nrow = 200, ipdiagnosis = FALSE)
  cohort <- data.table(genc_id = c(unique(ipdiagnosis$genc_id), 888, 999)) # add some genc_ids with no diagnosis entries

  check1 <- disability(cohort, ipdiag = ipdiagnosis, erdiag = erdiagnosis, component_wise = TRUE)


  ## For each row where disability = TRUE, at least one disability component should be TRUE
  expect_true(unique(check1[disability==TRUE, rowSums(.SD, na.rm = TRUE) > 0, .SDcols = setdiff(names(check1), c('genc_id', 'disability'))]))

  ## For each row where disability = FALSE, none of the disability components should be TRUE
  expect_false(unique(check1[disability==FALSE, rowSums(.SD, na.rm = TRUE) > 0, .SDcols = setdiff(names(check1), c('genc_id', 'disability'))]))

  ## For each row where disability = NA, all disability components should be NA
  expect_equal(unique(check1[is.na(disability), rowSums(!is.na(.SD), na.rm = TRUE), .SDcols = setdiff(names(check1), c('genc_id', 'disability'))]), 0)

  ## If no entry in diagnosis table, disability should be NA
  expect_equal(unique(check1[genc_id == 888 | genc_id == 999, disability]), NA)

})


test_that("returned with disability = TRUE", {

  ## Check 2: Unit test for some diagnosis codes that should have disability = TRUE
  check2 <- disability(cohort = data.table(genc_id = c(1,2,3,4,5)),
                       ipdiag = data.table(genc_id = c(1,1,2,2,2,3,4,5,5,5,5,5),
                                           diagnosis_code = c('F7001', 'Q8723','Q66', 'E2204', 'M23836', 'G60', 'G242','S029', 'Z998', 'H90132', 'Q150', 'E10352')),
                       erdiag = NULL, component_wise = TRUE)

  expect_true(all(check2$disability == TRUE))
  expect_true(check2[genc_id == 1, Developmental_disability])
  expect_true(check2[genc_id == 2, Physical_disability.Musculoskeletal_disorder])
  expect_true(check2[genc_id == 2, Physical_disability.Congenital_anomaly])
  expect_true(check2[genc_id == 3, Physical_disability.Neurological_disorder])
  expect_true(check2[genc_id == 4, Physical_disability.Neurological_disorder])
  expect_true(check2[genc_id == 5, Physical_disability.Permanent_injury])
  expect_true(check2[genc_id == 5, Sensory_disability.Hearing_impairment])
  expect_true(check2[genc_id == 5, Sensory_disability.Vision_impairment])

  expect_equal(rowSums(check2[genc_id == 1, -c('genc_id', 'disability')]), 1)
  expect_equal(rowSums(check2[genc_id == 2, -c('genc_id', 'disability')]), 2)
  expect_equal(rowSums(check2[genc_id == 3, -c('genc_id', 'disability')]), 1)
  expect_equal(rowSums(check2[genc_id == 4, -c('genc_id', 'disability')]), 1)
  expect_equal(rowSums(check2[genc_id == 5, -c('genc_id', 'disability')]), 3)

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





