test_that("normalizes drug names correctly", {
  expect_equal(normalize_text(" Ámoxícíllíns. ", lemma = FALSE), "amoxicillins")
})

test_that("singularizes plurals correctly", {
  expect_equal(normalize_text(" CHêcKs ", lemma = TRUE), "check")
})
