test_that("categorical cell suppression works", {
  x <- factor(c(rep("a", times = nrow(mtcars)), "b"), levels = c("a", "b"))
  res <- render_cell_suppression.categorical(x)

  expect_equal(res, c("", "a" = "(suppressed)", "b" = "(suppressed)"))

  x2 <- factor(c(rep("a", times = nrow(mtcars))), levels = c("a", "b"))
  res2 <- render_cell_suppression.categorical(x2)

  expect_equal(res2, c("", "a" = "32 (100 %)", "b" = "0 (0 %)"))

  x3 <- factor(
    c(rep("a", times = nrow(mtcars)), "b", "c", "d", "e", "f", "g"),
    levels = c("a", "b", "c", "d", "e", "f", "g")
    )
  res3 <- render_cell_suppression.categorical(x3)

  expect_equal(
    res3,
    c(
      "",
      "a" = "32 (84 %)",
      "b" = "< 6 obs. (suppressed)",
      "c" = "< 6 obs. (suppressed)",
      "d" = "< 6 obs. (suppressed)",
      "e" = "< 6 obs. (suppressed)",
      "f" = "< 6 obs. (suppressed)",
      "g" = "< 6 obs. (suppressed)"
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
      "a" = "100 (63 %)",
      "b" = "50 (31 %)",
      "c" = "(suppressed)",
      "d"  = "(suppressed)",
      "e" = "0 (0 %)"
      )
    )
})

test_that("bad input throws errors", {
  expect_error(
    render_default.continuous(letters)
  )
})
