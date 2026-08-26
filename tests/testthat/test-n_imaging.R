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
cohort <- DBI::dbGetQuery(con, "select * from public.admdad where genc_id <= 100")
cohort <- data.table::as.data.table(cohort)

test_that("function returns correct imaging counts", {
  # run n_imaging on dummy cohort
  result <- n_imaging(con, cohort)

  expect_equal(
    sum(result$n_img_ct_mri_us_derived, na.rm = TRUE),
    216
  )

  result_63 <- result[result$genc_id == 63, ]

  expect_equal(result_63$n_img_ct_derived, 4)
  expect_equal(result_63$n_img_mri_derived, 1)
  expect_equal(result_63$n_img_us_derived, 2)
  expect_equal(result_63$n_img_ct_mri_us_derived, 7)

  # number of rows in function output equals number of rows in cohort
  expect_equal(nrow(result), nrow(cohort))
})

test_that("ed tests are excluded", {
  # run n_imaging on dummy cohort with ed excluded
  result <- n_imaging(con, cohort, exclude_ed = TRUE)


  expect_equal(
    sum(result$n_img_ct_mri_us_derived, na.rm = TRUE),
    170
  )

  result_63 <- result[result$genc_id == 63, ]

  expect_equal(result_63$n_img_ct_derived, 3)
  expect_equal(result_63$n_img_mri_derived, 0)
  expect_equal(result_63$n_img_us_derived, 1)
  expect_equal(result_63$n_img_ct_mri_us_derived, 4)
})
