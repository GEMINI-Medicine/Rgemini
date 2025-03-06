test_that("function creates quartiles as expected", {
  ### set up correct result
  result_value <- cut(1:100,
    breaks = quantile(1:100, probs = c(0, 0.25, 0.5, 0.75, 1)),
    include.lowest = TRUE,
    labels = c(1:4)
  )

  # run the function
  expect_equal(
    create_ntiles(c(1:100), n = 4),
    result_value
  )
})


test_that("function creates deciles as expected", {
  ### set up correct result
  result_value <- cut(1:100,
    breaks = quantile(1:100,
      probs = seq(0, 1, by = 0.1)
    ),
    include.lowest = TRUE,
    labels = c(1:10)
  )

  # run the function
  expect_equal(
    create_ntiles(c(1:100), n = 10),
    result_value
  )
})

test_that("function quits when given non-numeric variable", {
  ## supply vector of letters rather than numbers
  expect_error(create_ntiles(c("A", "B", "C", "D", "E"), 5))
})

test_that("function quits when n < 2", {
  ## set n = -1
  expect_error(create_ntiles(rnorm(100), -1))
})

test_that("function quits when quartile breakpoints are identical", {
  ## create error by supplying vector with number repeated 20 times
  expect_error(create_ntiles(rep(20, 20), 5))
})

test_that("function returns percentiles that are the same length as input vector", {
  ## create sample vector
  set.seed(123)
  values <- rnorm(100)

  ## run the function
  n_tile_results <- create_ntiles(values, 10)

  ## confirm length of values == length of n_tile_results
  expect_equal(length(values), length(n_tile_results))
})

test_that("function has the same number of levels as n", {
  ## create sample vector
  set.seed(123)
  values <- rnorm(100)

  ## set n = 10
  n <- 10

  ## create ntiles
  ntile_result <- create_ntiles(x = values, n = n)

  ## check that length of levels(ntile_result) is 10
  expect_equal(length(levels(ntile_result)), n)
})
