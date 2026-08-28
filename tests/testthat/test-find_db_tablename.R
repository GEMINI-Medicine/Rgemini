# connect to dummy DB
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = "gemini-db-dummy.j.aivencloud.com",
  port = 10571,
  dbname = "dummy_db_v1_0_0", # most recent version
  user = "gemini_user",
  password = "gemini",
  sslmode = "require"
)

# Unit test
test_that("db table names are returned correctly", {
  table_name <- Rgemini:::find_db_tablename(con, "admdad")
  expect_equal(table_name, "admdad")

  table_name2 <- Rgemini:::find_db_tablename(con, "lab")
  expect_equal(table_name2, "lab")
})
