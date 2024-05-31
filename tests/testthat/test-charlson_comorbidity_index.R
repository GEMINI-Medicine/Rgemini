test_that("diagnosis type 1/w/x/y/3 and MRDx/2 is not an admission diagnosis", {
  ipdiag <- data.table(
    genc_id = rep(11111111, times = 2),
    diagnosis_code = c("K766", "K766"),
    diagnosis_type = c("M", 1)
  )

  ercols <- c("genc_id", "er_diagnosis_code", "er_diagnosis_type")
  erdiag <- data.table(matrix(nrow = 0, ncol = length(ercols)))
  colnames(erdiag) <- ercols

  res <- diagnoses_at_admission(ipdiag, erdiag)

  expect_equal(nrow(res), 0)

  ipdiag <- data.table(
    genc_id = rep(11111111, times = 2),
    diagnosis_code = c("E102", "E102"),
    diagnosis_type = c(2, 3)
  )

  res <- diagnoses_at_admission(ipdiag, erdiag)

  expect_equal(nrow(res), 0)
})


test_that("exclude type 6 and include MRDx if not also type 2", {
  ipdiag <- data.table(
    genc_id = rep(11111111, times = 2),
    diagnosis_code = c("K766", "D529"),
    diagnosis_type = c("M", 6)
  )

  erdiag <- NULL

  res <- diagnoses_at_admission(ipdiag, erdiag)

  expect_equal(
    data.table(
      genc_id = c(11111111),
      diagnosis_code = c("K766"),
      diagnosis_type = c("M")
    ),
    res
  )

  ipdiag <- data.table(
    genc_id = rep(11111111, times = 3),
    diagnosis_code = c("K766", "D529", "K766"),
    diagnosis_type = c("M", 6, 2)
  )

  res <- diagnoses_at_admission(ipdiag, erdiag)

  expect_equal(nrow(res), 0)
})


test_that("exclude type 1/w/x/y that is also type 6/2", {
  ipdiag5 <- data.table(
    genc_id = rep(11111111, times = 2),
    diagnosis_code = c("K766", "K766"),
    diagnosis_type = c(1, 6)
  )

  ercols <- c("genc_id", "er_diagnosis_code", "er_diagnosis_type")
  erdiag5 <- data.table(matrix(nrow = 0, ncol = length(ercols)))
  colnames(erdiag5) <- ercols

  res5 <- diagnoses_at_admission(ipdiag5, erdiag5)

  expect_equal(nrow(res5), 0)

  ipdiag5 <- data.table(
    genc_id = rep(11111111, times = 2),
    diagnosis_code = c("K766", "K766"),
    diagnosis_type = c("W", 2)
  )

  ercols <- c("genc_id", "er_diagnosis_code", "er_diagnosis_type")
  erdiag5 <- data.table(matrix(nrow = 0, ncol = length(ercols)))
  colnames(erdiag5) <- ercols

  res5 <- diagnoses_at_admission(ipdiag5, erdiag5)

  expect_equal(nrow(res5), 0)
})
