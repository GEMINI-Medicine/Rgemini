testthat::test_that("Correct score calculated for frailty conditions and sublevels codes", {
  set.seed(2)
  cohort_dum <- data.table(genc_id = 1, age = 70)
  ipdiag_dum <- gemSim::dummy_diag(
    nid = 1, ipdiagnosis = TRUE, pattern = "I509$|M12"
  ) # 2 unique frailty conditions in map and their sub-codes
  erdiag_dum <- gemSim::dummy_diag(
    nid = 1, ipdiagnosis = FALSE, pattern = "I50$|M083"
  ) # not frailty conditions

  res <- frailty_score(
    cohort_dum, ipdiag_dum, erdiag_dum,
    component_wise = FALSE
  )
  testthat::expect_equal(res$frailty_score_derived, 2)

  res <- frailty_score(
    cohort_dum, ipdiag_dum, erdiag_dum,
    component_wise = TRUE
  )
  testthat::expect_equal(
    res, data.table(
      genc_id = 1,
      diagnosis_code = c("I509", "M120", "M120", "M123", "M125", "M128"),
      frailty_categories = c("Cardiac and vascular", "Arthritis and inflammation", "Arthritis and inflammation", "Arthritis and inflammation", "Arthritis and inflammation", "Arthritis and inflammation")
    )
  )
})

testthat::test_that("Setting erdiag to NULL removes erdiagosis codes with a warning message", {
  set.seed(2)
  cohort_dum <- data.table(genc_id = 1, age = 70)
  ipdiag_dum <- gemSim::dummy_diag(
    nid = 1, ipdiagnosis = TRUE, pattern = "M12"
  ) # frailty conditions
  erdiag_dum <- gemSim::dummy_diag(
    nid = 1, ipdiagnosis = FALSE, pattern = "C20$|E209$"
  ) # frailty conditions

  testthat::expect_warning(
    res <- frailty_score(
      cohort_dum, ipdiag_dum, NULL,
      component_wise = FALSE
    )
  )
  testthat::expect_equal(res$frailty_score_derived, 1)

  testthat::expect_error(
    frailty_score(cohort_dum, ipdiag_dum, component_wise = FALSE)
  ) # expect error when no value is passed
})

testthat::test_that("Encounters not qualify for frailty assessment are excluded from results", {
  set.seed(2)
  cohort_dum <- data.table(
    genc_id = c(1, 2, 3, 4), age = c(89, 64, 65, 70)
  ) # id=3 below age cutoff, id=4 doesn't have diagnosis data
  ipdiag_dum <- gemSim::dummy_diag(
    nid = 3, ipdiagnosis = TRUE, pattern = "C20$|R460$"
  ) # frailty conditions
  erdiag_dum <- gemSim::dummy_diag(
    nid = 3, ipdiagnosis = FALSE, pattern = "C20$|E209$"
  ) # frailty conditions

  suppressWarnings(
    testthat::expect_warning(
      res <- frailty_score(
        cohort_dum, ipdiag_dum, erdiag_dum,
        component_wise = FALSE
      )
    )
  )
  print(res)
  testthat::expect_equal(res$frailty_score_derived, c(3, 3))
  testthat::expect_equal(res$genc_id, c(1, 3))
})

testthat::test_that("Encounters no frailty condition are returned with a frailty score of 0", {
  set.seed(2)
  cohort_dum <- data.table(genc_id = 1, age = 89)
  ipdiag_dum <- gemSim::dummy_diag(
    nid = 1, ipdiagnosis = TRUE, pattern = "A05$"
  ) # no frailty condition

  testthat::expect_warning(
    res <- frailty_score(cohort_dum, ipdiag_dum, NULL)
  )
  testthat::expect_equal(res$frailty_score_derived, c(0))
})
