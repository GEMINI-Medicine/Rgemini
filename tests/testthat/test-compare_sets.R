test_that("function works when comparing dates in different types", {
  ### create a vector of 3 random dates in YYYYMMDD and YYYY-MM-DD, set dates = T
  ### the function should indicate that the dates are the same
  # set up correct table
  result_table <- data.table(in_both = 3, x_only = 0, y_only = 0)

  # run the check that they're the same
  expect_equal(
    compare_sets(c("2024-01-01", "2024-06-30", "2024-08-01"),
      c(20240101, 20240630, 20240801),
      dates = TRUE
    ),
    result_table
  )
})


test_that("function works when vectors contain different date types", {
  ### create a vector containing multiple date types ("YYYY-MM-DD" & YYYYMMDD)
  ### compare with vector that only contains YYYY-MM-DD
  # set up correct table
  result_table <- data.table(in_both = 3, x_only = 0, y_only = 0)

  # run the check that they're the same
  expect_equal(
    compare_sets(c("2024-01-01", "2024-06-30", "2024-08-31"),
      c(20240101, 20240630, "2024-08-31"),
      dates = TRUE
    ),
    result_table
  )
})

test_that("function works as expected when not comparing dates", {
  ## set up correct table
  result_table <- data.table(in_both = 4, x_only = 20, y_only = 20)

  ## create vectors
  x <- c(1:24)
  y <- c(21:44)

  expect_equal(compare_sets(x, y), result_table)
})

test_that("function works when comparing date times", {
  ## set up correct table
  result_table <- data.table(in_both = 3, x_only = 0, y_only = 0)

  ## create vectors
  x <- c("2020-10-01 11:59", "2022-08-19 01:37", "2005-09-12 12:34")
  y <- c("20201001 11:59", "20220819 01:37", "20050912 12:34")

  expect_equal(compare_sets(x, y, dates = TRUE), result_table)
})

test_that("function performs as expected when there are no overlaps", {
  ## set up correct table
  result_table <- data.table(in_both = 0, x_only = 10, y_only = 10)

  ## create vectors to test
  x <- c(1:10)
  y <- c(21:30)

  expect_equal(compare_sets(x, y, dates = FALSE), result_table)
})

test_that("function works when supplying custom date formats", {
  ## set up correct table
  result_table <- data.table(in_both = 3, x_only = 0, y_only = 0)

  ## create vectors (using mdy rather than ymd)
  x <- c("08212025", "02-14-2024", "12-25-2022")
  y <- c("08-21-2025", "02142024", "12252022")

  expect_equal(compare_sets(x, y, dates = TRUE, orders = c("mdy")), result_table)
})

test_that("function works when supplying custom order but vectors have no overlap", {
  ## set up correct table
  result_table <- data.table(in_both = 0, x_only = 3, y_only = 3)

  ## create vectors (using mdy rather than ymd)
  x <- c("10212025", "02-15-2024", "01-29-2022")
  y <- c("06-21-2025", "12142024", "12272022")

  expect_equal(compare_sets(x, y, dates = TRUE, orders = c("mdy")), result_table)
})
