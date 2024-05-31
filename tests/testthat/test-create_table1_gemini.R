test_that("categorical cell suppression works", {
  x <- factor(c(rep("a", times = nrow(mtcars)), "b"), levels = c("a", "b"))
  res <- render_cell_suppression.categorical(x)

  expect_equal(res, c("", "a" = "(suppressed)", "b" = "(suppressed)"))

  x2 <- factor(c(rep("a", times = nrow(mtcars))), levels = c("a", "b"))
  res2 <- render_cell_suppression.categorical(x2)

  expect_equal(res2, c("", "a" = "32 (100.0%)", "b" = "0 (0.0%)"))

  x3 <- factor(
    c(rep("a", times = nrow(mtcars)), "b", "c", "d", "e", "f", "g"),
    levels = c("a", "b", "c", "d", "e", "f", "g")
    )
  res3 <- render_cell_suppression.categorical(x3)

  expect_equal(
    res3,
    c(
      "",
      "a" = "32 (84.2%)",
      "b" = "&lt; 6 obs. (suppressed)",
      "c" = "&lt; 6 obs. (suppressed)",
      "d" = "&lt; 6 obs. (suppressed)",
      "e" = "&lt; 6 obs. (suppressed)",
      "f" = "&lt; 6 obs. (suppressed)",
      "g" = "&lt; 6 obs. (suppressed)"
      )
    )

  x4 <- factor(
    c(rep("a", times = 100),
      rep("b", times = 50),
      rep("c", times = 7),
      rep("d", times = 2)),
    levels = c("a", "b", "c", "d", "e")
    )

  res4 <- render_cell_suppression.categorical(x4)

  expect_equal(
    res4,
    c(
      "",
      "a" = "100 (62.9%)",
      "b" = "50 (31.4%)",
      "c" = "(suppressed)",
      "d"  = "(suppressed)",
      "e" = "0 (0.0%)"
      )
    )
})

test_that("bad input throws errors", {
  expect_error(
    render_default.continuous(letters)
  )
})

test_that("categorial cell suppression doesn't round to the nearest integer", {
  x <- factor(
    c(rep("a", times = 1000), rep("b", times = 1010)),
    levels = c("a", "b")
    )

  expect_equal(
    render_strict_cell_suppression.categorical(x),
    c("", "a" = "1000 (49.8%)", "b" = "1010 (50.2%)")
    )

  expect_equal(
    render_cell_suppression.categorical(x),
    c("", "a" = "1000 (49.8%)", "b" = "1010 (50.2%)")
  )
})

test_that("when a factor variable is missing levels, still can calculate SMD", {
  g <- rep(LETTERS[1:3], each = 30)
  x <- c(rep('d', 20), rep('e', 10), rep('d', 16), rep('e', 12), rep('f', 1), rep('g', 1), rep('d', 10), rep('e', 19), rep('g',1))

  df <- data.frame(hosp = g, gender = x)

  expect_equal(
    max_pairwise_smd(split(df$gender, df$hosp)),
    0.734
  )
})
