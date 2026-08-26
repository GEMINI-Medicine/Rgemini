# connect to dummy DB
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "gemini-db-dummy.j.aivencloud.com",
  port = 10571,
  dbname = "dummy_db_v1_0_0",
  user = "gemini_user",
  password = "gemini",
  sslmode = "require"
)

# pull dummy cohort
cohort <- DBI::dbGetQuery(con, "select * from public.admdad")
cohort <- data.table::as.data.table(cohort)

test_that("function returns correct routine bloodwork counts", {
  # run n_routine_bloodwork on dummy cohort
  result <- n_routine_bloodwork(con, cohort)

  expect_equal(
    sum(result$n_routine_bloodwork_derived, na.rm = TRUE),
    65822
  )

  result_3 <- result[result$genc_id == 3, ]

  expect_equal(result_3$n_routine_bloodwork_derived, 20)

  # number of rows in function output equals number of rows in cohort
  expect_equal(nrow(result), nrow(cohort))
})

test_that("ed tests are excluded", {
  # run n_routine_bloodwork on dummy cohort with ed excluded
  result <- n_routine_bloodwork(con, cohort, exclude_ed = TRUE)

  expect_equal(
    sum(result$n_routine_bloodwork_derived, na.rm = TRUE),
    56501
  )

  result_3 <- result[result$genc_id == 3, ]

  expect_equal(result_3$n_routine_bloodwork_derived, 16)
})
