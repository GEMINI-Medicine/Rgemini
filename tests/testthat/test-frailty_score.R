
testthat::test_that("Correct score calculated for frailty conditions and sublevels codes", {
  set.seed(2)
  cohort_dum <- data.table(genc_id=c(1), age=c(70))
  ipdiag_dum <- dummy_diag(nid=1, nrow=5, ipdiagnosis=T, pattern ="I509$|M12") # 2 unique frailty conditions in map and their sub-codes
  erdiag_dum <- dummy_diag(nid=1, nrow=4, ipdiagnosis=F, pattern ="I50$|M083") # not frailty conditions

  res <- frailty_score(cohort_dum, ipdiag_dum, erdiag_dum, component_wise = F)
  testthat::expect_equal(res$frailty_score_derived, 2)

  res <- frailty_score(cohort_dum, ipdiag_dum, erdiag_dum, component_wise = T)
  testthat::expect_equal(res$diagnosis_code, c("M128", "I509", "M120", "M123"))
})

testthat::test_that("Setting erdiag to NULL removes erdiagosis codes with a warning message", {
  set.seed(2)
  cohort_dum <- data.table(genc_id=c(1) , age=c(70))
  ipdiag_dum <- dummy_diag(nid=1, nrow=3, ipdiagnosis=T, pattern ="M12") # frailty conditions
  erdiag_dum <- dummy_diag(nid=1, nrow=3, ipdiagnosis=F, pattern ="C20$|E209$") # frailty conditions

  testthat::expect_warning( res <- frailty_score(cohort_dum, ipdiag_dum, NULL, component_wise = F) )
  testthat::expect_equal(res$frailty_score_derived, 1)

  testthat::expect_error(frailty_score(cohort_dum, ipdiag_dum, component_wise = F)) #expect error when no value is passed
})

testthat::test_that("Encounters not qualify for frailty assessment are excluded from results", {
  set.seed(2)
  cohort_dum <- data.table(genc_id=c(1, 2, 3, 4), age=c(89, 64, 65, 70)) #id=3 below age cutoff, id=4 doesn't have diagnosis data
  ipdiag_dum <- dummy_diag(nid=3, nrow=10, ipdiagnosis=T, pattern ="C20$|R460$") # frailty conditions
  erdiag_dum <- dummy_diag(nid=3, nrow=3, ipdiagnosis=F, pattern ="C20$|E209$") # frailty conditions

  testthat::expect_warning(res <- frailty_score(cohort_dum, ipdiag_dum, erdiag_dum, component_wise = F))
  testthat::expect_equal(res$frailty_score_derived, c(2, 3))
  testthat::expect_equal(res$genc_id, c(1, 3))
})
