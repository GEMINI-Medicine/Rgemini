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
test_that("hospital field is returned correctly", {
  
  hospital_field <- Rgemini:::return_hospital_field(con)
  expect_equal(hospital_field, "hospital_num") # dummy DB only contains hospital_num variable
  
})
