test_that("default census counts are returned correclty (include_zero = TRUE)", {

  # create dummy data for testing
  set.seed(2)
  ipadmdad <- dummy_ipadmdad(n = 1000, n_hospitals = 5, time_period = c(2020, 2021)) %>%
    data.table()

  # calculate census
  census <- daily_census(ipadmdad)

  # check output
  expect_equal(round(mean(census$census, na.rm = TRUE), digits = 2), 2.47)
  expect_equal(round(mean(census$capacity_ratio, na.rm = TRUE), digits = 2), 1.03)
  expect_equal(sum(is.na(census$census)), 150) # NAs due to buffer period
  expect_equal(sum(census$census == 0, na.rm = TRUE), 304) # should include 0s

})


test_that("census counts are returned correclty when include_zero = FALSE", {

  # create dummy data for testing
  set.seed(2)
  ipadmdad <- dummy_ipadmdad(n = 1000, n_hospitals = 5, time_period = c(2020, 2021)) %>%
    data.table()

  # calculate census
  census <- daily_census(ipadmdad, include_zero = FALSE)

  # check output (counts should now be higher overall due to exclusion of 0s)
  expect_equal(round(mean(census$census, na.rm = TRUE), digits = 2), 2.71)
  expect_equal(round(mean(census$capacity_ratio, na.rm = TRUE), digits = 2), 1.13)
  expect_equal(sum(is.na(census$census)), 454)
  expect_equal(sum(census$census == 0, na.rm = TRUE), 0)

})


test_that("buffer period correctly excludes days from census counts", {

  # create dummy data for testing
  set.seed(2)
  ipadmdad <- dummy_ipadmdad(n = 1000, n_hospitals = 5, time_period = c(2019, 2019)) %>%
    data.table()

  # calculate census
  census <- daily_census(ipadmdad, buffer = 90)

  expect_equal(sum(is.na(census$census)), 90*5) #90-day buffer period for each of 5 hospitals
  expect_equal(sum(is.na(census$capacity_ratio)), 90*5)

})
