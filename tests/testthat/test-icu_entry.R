
testthat::test_that("ICU admissions are properly identify with default settings", {
  cohort_dum <- data.table(genc_id=c(1, 2) , admission_date_time=c("2020-07-15 02:00", "2020-07-15 15:00"))
  ipscu_dum <- data.table(genc_id=c(1), scu_admit_date_time=c("2020-07-14 16:00"),
                    scu_unit_number=c(30))
  res <- icu_entry(cohort_dum, ipscu_dum)
  testthat::expect_equal(res$icu_entry_derived, c(TRUE, FALSE))
})


testthat::test_that("Admissions to Step-Down Units are not considered as ICU admissions", {
  cohort_dum <- data.table(genc_id=c(1, 2) , admission_date_time=c("2020-07-15 02:00", "2020-07-15 15:00"))
  ipscu_dum <- data.table(genc_id=c(1, 2), scu_admit_date_time=c("2020-07-14 16:00", "2020-07-15 18:00"),
                          scu_unit_number=c(90, 99))
  res <- icu_entry(cohort_dum, ipscu_dum)
  testthat::expect_equal(res$icu_entry_derived, c(FALSE, FALSE))
})


testthat::test_that("ICU admissions as a clinical outcome excludes entries prior to IP admission time by default", {
  cohort_dum <- data.table(genc_id=c(1, 2) , admission_date_time=c("2020-07-15 02:00", "2020-07-15 15:00"))
  ipscu_dum <- data.table(genc_id=c(1, 2), scu_admit_date_time=c("2020-07-14 16:00", "2020-07-15 18:00"),
                          scu_unit_number=c(30, 45))
  res <- icu_entry(cohort_dum, ipscu_dum, as_outcome=T)
  testthat::expect_equal(res$icu_entry_derived , c(FALSE, TRUE))
})


testthat::test_that("ICU admissions within the user specified time window are properly identified", {
  cohort_dum <- data.table(genc_id=c(1, 2) , admission_date_time=c("2020-07-15 02:00", "2020-07-15 15:00"))
  ipscu_dum <- data.table(genc_id=c(1, 2), scu_admit_date_time=c("2020-07-15 12:00", "2020-07-16 06:00"),
                          scu_unit_number=c(30, 45))
  res <- icu_entry(cohort_dum, ipscu_dum, entry_since_cutoff=12)
  testthat::expect_equal(res$icu_entry_derived , c(TRUE, TRUE))
  testthat::expect_equal(res$icu_entry_in_12hr_derived, c(TRUE, FALSE))
})


testthat::test_that("When both exclude_xhr_post_ipadmit and entry_window are customized, ICU admissions within the customized time interval are properly identified", {
  cohort_dum <- data.table(genc_id=c(1, 2) , admission_date_time=c("2020-07-15 02:00", "2020-07-15 15:00"))
  ipscu_dum <- data.table(genc_id=c(1, 2), scu_admit_date_time=c("2020-07-15 12:00", "2020-07-16 06:00"),
                          scu_unit_number=c(30, 45))
  res <- icu_entry(cohort_dum, ipscu_dum, as_outcome=T, exclude_cutoff=10,  entry_since_cutoff=12)
  testthat::expect_equal(res$icu_entry_derived , c(FALSE, TRUE))
  testthat::expect_equal(res$icu_entry_in_12hr_derived, c(FALSE, TRUE))
  
  res <- icu_entry(cohort_dum, ipscu_dum, as_outcome=T, exclude_cutoff=10,  entry_since_cutoff=3)
  testthat::expect_equal(res$icu_entry_derived , c(FALSE, TRUE))
  testthat::expect_equal(res$icu_entry_in_3hr_derived, c(FALSE, FALSE))
})
