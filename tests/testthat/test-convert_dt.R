test_that(
  "date-times provided as character input are parsed correctly with orders = 'ymd HM' (default)", {

    date_time <- c("2023-01-01 12:05", "2018-12-24 14:00",
                   "2021-02-20 08:10:00", "2025-02-05", NA, "", " ")

    # this should return warnings due to missing/invalid entries
    # warnings: 3 missing, 2 invalid, 1 invalid due to date-only
    suppressWarnings(expect_warning(res <- convert_dt(date_time)))

    # check outputs
    expect_equal(class(res), c("POSIXct", "POSIXt"))
    expect_equal(res, as.POSIXct(c("2023-01-01 12:05:00", "2018-12-24 14:00",
                                   NA, NA, NA, NA, NA), tz = "UTC"))

  }
)

test_that(
  "date-times provided as character input can be parsed according to multiple orders", {

    date_time <- c("2014-01-01 12:00", "2022-01-01 02:10", "2024-01-01 12:00:30",
                   "2017-01-01 13:12:20", "2021-02-05", "2019-12-05")

    # should not result in warning due to multiple acceptable formats
    expect_no_warning(res <- convert_dt(date_time, orders = c("ymd HM", "ymd HMS", "ymd")))

    expect_equal(class(res), c("POSIXct", "POSIXt"))
    expect_equal(
      res, as.POSIXct(c("2014-01-01 12:00:00", "2022-01-01 02:10:00", "2024-01-01 12:00:30",
                        "2017-01-01 13:12:20", "2021-02-05 00:00:00", "2019-12-05 00:00:00"), tz = "UTC")
    )
  }
)

test_that(
  "dates-times provided as POSIXct POSIXt input are returned correctly", {

    ## pre-processed with ymd_hms
    date_time <- ymd_hms(c("2016-01-01 12:00:33", "2022-01-01 00:00:23", "2020-01-01 12:00:54", "2014-01-01 00:00:14", "2021-02-05 10:00:46"))
    res <- convert_dt(date_time, orders = "ymd HMS")
    expect_equal(class(res), c("POSIXct", "POSIXt"))
    # output should be identical to input
    expect_equal(date_time, res)

    ## pre-processed with ymd_hm
    date_time <- ymd_hm(c("2016-01-01 12:00", "2022-01-01 00:00", "2020-01-01 12:00", "2014-01-01 00:00", "2021-02-05 10:00"))
    res <- convert_dt(date_time, orders = "ymd HM")
    expect_equal(class(res), c("POSIXct", "POSIXt"))
    # output should be identical to input
    expect_equal(date_time, res)

  }
)

test_that(
  "date-only formats are returned correctly", {

    ## dates pre-processed with ymd
    date_time <- ymd(c("2020-01-01", "2020-01-01", "2020-01-01", "2020-01-01", "2021-02-05"))
    res <- convert_dt(date_time, orders = "ymd")
    expect_equal(class(res), c("POSIXct", "POSIXt"))
    expect_equal(as.POSIXct(date_time, tz = "UTC"), res)
    # should result in warning when expecting timestamp information
    suppressWarnings(expect_warning(convert_dt(date_time, orders = c("ymd HM", "ymd HMS"))))

    ## dates pre-processed with as.Date input
    date_time <- as.Date(c("2020-01-01 00:22", "2020-01-01 04:23", "2020-01-01 00:87:12", "2020-01-01", "2021-02-05"))
    res <- convert_dt(date_time, orders = "ymd")
    expect_equal(class(res), c("POSIXct", "POSIXt"))
    expect_equal(as.POSIXct(date_time, tz = "UTC"), res)
    # should result in warning when expecting timestamp information
    suppressWarnings(expect_warning(convert_dt(date_time, orders = "ymd HM")))
    # ... unless truncation is applied
    expect_no_warning(convert_dt(date_time, orders = "ymd HM", truncated = 2))

  }
)

test_that(
  "warnings for 00:00 timestamps are returned correctly", {

    suppressWarnings(expect_warning(
      convert_dt("2020-01-01 00:00", orders = c("ymd HM", "ymd HMS"), check_ts_zeros = TRUE)
    ))
    suppressWarnings(expect_warning(
      convert_dt("2020-01-01 00:00:00", orders = c("ymd HM", "ymd HMS"), check_ts_zeros = TRUE)
    ))

    # check should still be performed even if not all date-times are expected to have
    # timestamp / if truncation is applied
    suppressWarnings(expect_warning(
      convert_dt("2020-01-01 00:00", orders = c("ymd"), check_ts_zeros = TRUE)
    ))
    suppressWarnings(expect_warning(
      convert_dt("2020-01-01 00:00:00", orders = c("ymd HM", "ymd HMS"), check_ts_zeros = TRUE, truncated = 3)
    ))

    # make sure this check runs if variable has NAs/""
    #convert_dt("", orders = c("ymd HM", "ymd HMS"), check_ts_zeros = TRUE)
    suppressWarnings(expect_warning(
      convert_dt(c("2021-04-05 00:00","", NA), orders = c("ymd HM", "ymd HMS"), check_ts_zeros = TRUE)
    ))
  }
)

test_that(
  "Truncation works as expected", {

    date_time <- c("2015-10-03 00:00", "2022-12-15 00:00:00", "2017-03-28")
    res <- convert_dt(date_time, orders = c("ymdHMS"), truncated = 3)

    expect_equal(class(res), c("POSIXct", "POSIXt"))
    expect_equal(
      res, as.POSIXct(c("2015-10-03", "2022-12-15", "2017-03-28"), tz = "UTC")
    )

  }
)
