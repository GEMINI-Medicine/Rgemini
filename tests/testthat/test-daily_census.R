## create dummy data for unit tests
ipadmdad <- data.table(
  hospital_num = c(1, 1, 1, 1, 1, 2, 2, 3, 3, 3),
  hospital_id = c("A", "A", "A", "A", "A", "B", "B", "C", "C", "C"),
  genc_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  admission_date_time = c(
    "2020-01-01 08:00", "2020-01-01 11:00", "2020-01-01 15:00", "2020-01-02 15:00", "2020-01-01 11:00",
    "2020-01-01 07:00", "2020-01-01 03:00", "2020-01-01 07:00", "2020-01-04 07:00", "2020-01-04 06:00"
  ),
  discharge_date_time = c(
    "2020-01-02 11:00", "2020-01-03 07:00", "2020-01-04 08:00", "2020-01-04 08:00", "2020-01-02 19:00",
    "2020-01-03 07:00", "2020-01-01 07:00", "2020-01-01 19:00", "2020-01-04 20:00", "2020-01-04 12:00"
  )
)

test_that("default census counts are returned correctly (include_zero = TRUE)", {
  # calculate census
  census <- daily_census(ipadmdad, buffer = 0)

  # check output
  expect_equal(round(mean(census$census, na.rm = TRUE), digits = 2), 1.30)
  expect_equal(round(mean(census$capacity_ratio, na.rm = TRUE), digits = 2), 1.20)
  expect_true(sum(census$census == 0, na.rm = TRUE) == 3) # should include 0s
  # make sure all relevant columns are included in output
  expect_equal(colnames(census), c("hospital_id", "hospital_num", "date_time", "census", "capacity_ratio"))
})


test_that("census counts are returned correctly when include_zero = FALSE", {
  # calculate census
  census <- daily_census(ipadmdad, include_zero = FALSE, buffer = 0)

  # check output (counts should now be higher overall due to exclusion of 0s)
  expect_equal(round(mean(census$census, na.rm = TRUE), digits = 2), 1.86)
  expect_equal(round(mean(census$capacity_ratio, na.rm = TRUE), digits = 2), 1.14)
  expect_equal(sum(census$census == 0, na.rm = TRUE), 0) # should not contain 0s
})


test_that("buffer period correctly excludes days from census counts", {
  # calculate census
  census <- daily_census(ipadmdad, buffer = 1)
  expect_equal(sum(is.na(census$census)), 3) # 1-day buffer period for each of 3 hospitals = 3
})
