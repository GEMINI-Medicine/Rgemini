test_that("correct inputs pass checks", {

  # example function with expected input types
  my_function_correct <- function(input1 = TRUE, # logical
                                  input2 = -1.5, # numeric
                                  input3 = 2, # integer
                                  input4 = "all", # character
                                  input5 = data.frame( # data.frame
                                    genc_id = as.integer(5),
                                    discharge_date_time = "2020-01-01"),
                                  input6 = data.table( # data.table
                                    genc_id = as.integer(5),
                                    discharge_date_time = '2020-01-01 00:00'),
                                  input7 = dbDriver("PostgreSQL") # DB connection
  ){

    expect_no_error(
      check_input(input1, "logical")
    )

    expect_no_error(
      check_input(input2, "numeric", interval = c(-Inf, 0))
    )

    expect_no_error(
      check_input(input3, "integer", interval = c(1, Inf))
    )

    expect_no_error(
      check_input(input4, "character", options = c("all", "none"))
    )

    expect_no_error(
      check_input(input5, c("data.frame", "data.table"),
                  colnames = c("genc_id", "discharge_date_time"),
                  coltypes = c("integer", "character"),
                  unique = TRUE)
    )

    expect_no_error(
      check_input(input6,
                  c("data.frame", "data.table"),
                  colnames = c("genc_id", "discharge_date_time"),
                  coltypes = c("integer", "character"),
                  unique = TRUE)
    )

    expect_no_error(
      check_input(input7, "dbi")
    )

  }

  my_function_correct()

})


## test incorrect input types
test_that("incorrect inputs fail check", {

  # example function with expected input types
  my_function_incorrect <- function(input1 = TRUE, # logical
                                  input2 = -1.5, # numeric
                                  input3 = 2, # integer
                                  input4 = "all", # character
                                  input5 = data.frame( # data.frame
                                    genc_id = as.integer(5),
                                    discharge_date_time = "2020-01-01"),
                                  input6 = data.table( # data.table
                                    genc_id = as.integer(5),
                                    discharge_date_time = '2020-01-01 00:00'),
                                  input7 = dbDriver("PostgreSQL") # DB connection
  ){

    # wrong type
    expect_error(
      check_input(input2, "integer")
    )

    # wrong interval for numeric/integer variable
    expect_error(
      check_input(input3, "numeric", interval = c(-Inf, 0))
    )

    # unacceptable option for character input
    expect_error(
      check_input(input4, "character", options = c("any", "none"))
    )

    # missing column in table input
    expect_error(
      check_input(input5, c("data.frame", "data.table"),
                  colnames = c("genc_id", "discharge_date_time", "hospital_num"))
    )

    # incorrect column type of genc_id
    expect_error(
      check_input(input6, c("data.frame", "data.table"),
                  colnames = c("genc_id", "discharge_date_time"),
                  coltypes = c("numeric", "character"))
    )

    # duplicates in input table
    input6 <- rbind(input6, input6)
    expect_error(
      check_input(input6, c("data.frame", "data.table"), unique = TRUE)
    )

  }

  my_function_incorrect()

})
