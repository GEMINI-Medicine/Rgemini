## create test data
test_data <- data.frame(
    genc_id = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
    hospital_num = c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3),
    age = c(40, 50, 60, 87, NA, 67, 95, 63, 79, 58, 91, 45),
    gender = c("F", "M", "F", "M", "F", "M", "F", "O", "F", "M", NA, "F"),
    discharge_date_time = c("2021-01-02 11:00", "2022-02-03 07:00", "2022-01-04 08:00", "2022-08-04 08:00", "2021-01-02 19:00", 
                            "2021-03-03 07:00", "2022-05-01 07:00", "2021-01-05 23:00", "2021-01-01 19:00", "2022-09-04 20:00",
                            "2021-01-04 12:00", "2021-01-03 19:00")
)

test_that("cell count (func = 'n') returns expected values", {

  # default grouping (by month * hospital_num)
  res <- plot_over_time(data = test_data, func = "n", return_data = TRUE)
  expect_equal(res$data_aggr$n, c(2, 1, 3, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1))
  expect_equal(res$data_aggr_overall$n, c(2, 0, 0, 0, 0, 0, 0)) # missing entries are returned as 0 for counts
  
  # remove grouping by hospital (= identical to "overall" results)
  res <- plot_over_time(data = test_data, func = "n", return_data = TRUE, line_group = NULL, facet_group = NULL)
  expect_equal(res$data_aggr$n, c(6, 1, 1, 1, 1, 1, 1))
  expect_equal(res$data_aggr$n, res$data_aggr_overall$n)
  
})

test_that("mean (default func) of variable is returned as expected", {
  
  # without cell suppression
  res <- plot_over_time(data = test_data, plot_var = "age", return_data = TRUE, time_int = "year")
  expect_equal(round(res$data_aggr$mean_age, 1), c(40.0, 65.0, 71.7, 65.7, 95.0, 58.0))
  expect_equal(round(res$data_aggr_overall$mean_age, 1), c(64.2, 70.0))
  
  # with cell suppression
  res <- suppressWarnings(plot_over_time(data = test_data, plot_var = "age", return_data = TRUE, min_n = 2))
  # should only return entries with n >= 2
  expect_true(min(res$data_aggr$n) >= 2)
  expect_true(min(res$data_aggr_overall$n) >= 2)
  # check returned values
  expect_equal(round(res$data_aggr$mean_age, 1), c(40.0, 71.7))
  expect_equal(round(res$data_aggr_overall$mean_age, 1), c(63.6))
  
})


test_that("percentage (func = `prct`) of variable is returned as expected", {
  
  # % for categorical variables
  res <- suppressWarnings(plot_over_time(data = test_data, plot_var = "gender", return_data = TRUE, time_int = "season"))
  expect_equal(round(res$data_aggr_overall$prct_gender_O, 1), c(0.0,  0.0,  0.0, 14.3))
  expect_equal(as.character(unique(res$data_aggr$discharge_season)), c("Spring", "Summer", "Fall", "Winter"))
  
  
  # % for numeric variables (% age <= 60)
  res <- plot_over_time(data = test_data, plot_var = "age", return_data = TRUE, plot_cat = c(seq(18, 60, 1)), time_int = "year")
  expect_equal(round(res$data_aggr$mean_age, 1), c(40.0, 65.0, 71.7, 65.7, 95.0, 58.0))
  expect_equal(round(res$data_aggr_overall$mean_age, 1), c(64.2, 70.0))
  
})
