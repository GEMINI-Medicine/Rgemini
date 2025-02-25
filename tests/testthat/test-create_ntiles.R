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

test_that("function quits when n < 0", {
  ## set n < 0
  expect_error(create_ntiles(rnorm(100), -1))
})

test_that("function quits when quartile breakpoints are identical", {
  ## create error by supplying vector with number repeated 20 times
  expect_error(create_ntiles(rep(20, 20), 5))
})
