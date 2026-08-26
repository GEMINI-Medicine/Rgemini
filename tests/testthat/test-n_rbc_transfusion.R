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


test_that("function returns correct rbc transfusion counts", {
  
  # run n_rbc_transfusions on dummy cohort
  result <- n_rbc_transfusions(con, cohort)
  
  
  expect_equal(
    sum(result$n_rbc_transfusion_derived, na.rm = TRUE),
    1194
  )
  
  expect_equal(
    sum(result$n_app_rbc_transfusion_derived, na.rm = TRUE),
    93
  )
  
  result_35 <- result[result$genc_id == 35, ]
  
  expect_equal(result_35$n_rbc_transfusion_derived, 3)
  expect_equal(result_35$n_app_rbc_transfusion_derived, 1)
  
  # number of rows in function output equals number of rows in cohort
  expect_equal(nrow(result), nrow(cohort))
  

  
})

test_that("ed tests are excluded", {
  
  # run n_rbc_transfusions on dummy cohort with ed excluded
  result<- n_rbc_transfusions(con, cohort, exclude_ed =TRUE)
  
  
  expect_equal(
    sum(result$n_rbc_transfusion_derived, na.rm = TRUE),
    1150
  )
  
  expect_equal(
    sum(result$n_app_rbc_transfusion_derived, na.rm = TRUE),
    87
  )
  
  
  result_35 <- result[result$genc_id == 35, ]
  
  expect_equal(result_35$n_rbc_transfusion_derived, 1)
  expect_equal(result_35$n_app_rbc_transfusion_derived, 1)
  
  
})






