#' Imports for the entire package
#' Doesn't require Depends or `@import` per function
#'
#' @rawNamespace
#' import(data.table, except = c("first", "last", "between", "month", "hour",
#' "quarter", "week", "year", "wday", "second", "minute", "mday", "yday",
#' "isoweek"))
#' @rawNamespace
#' import(dplyr, except = c("first", "last", "between", "matches"))
#'
NULL

#' @title
#' Sample a truncated log normal distribution
#'
#' @description
#' Sample from a log normal distribution using the `rlnorm` function
#' Truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param meanlog (`numeric`) The mean of the log normal distribution
#'
#' @param sdlog (`numeric`) The standard deviation of the log normal distribution
#'
#' @param min (`numeric`) The minimum value to truncate the data to.
#'
#' @param max (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the log normal distribution, truncated to the specified range.
#'
#' @export
#'
rlnorm_trunc <- function(n, meanlog, sdlog, min, max, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (any(min > max)) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  res <- rlnorm(n, meanlog, sdlog)
  # keep redrawing until all are in range
  # get out of range values
  oor <- (res < min) | (res > max)
  while (any(oor)) {
    res[oor] <- rlnorm(sum(oor, na.rm = TRUE), meanlog, sdlog)
    oor <- (res < min) | (res > max)
  }
  return(res)
}

#' @title
#' Sample a truncated normal distribution
#'
#' @description
#' Sample from a normal distribution using the `rnorm` function but truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param mean (`numeric`) The mean of the normal distribution
#'
#' @param sd (`numeric`) The standard deviation of the normal distribution
#'
#' @param min (`numeric`) The minimum value to truncate the data to.
#'
#' @param max (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the normal distribution, truncated to the specified range.
#'
#' @export
#'
rnorm_trunc <- function(n, mean, sd, min, max, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (any(min > max)) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  res <- rnorm(n, mean, sd)
  while (sum(res < min) + sum(res > max) > 0) {
    res[c(res < min | res > max)] <- rnorm(
      sum(res < min) + sum(res > max),
      mean,
      sd
    )
  }
  return(res)
}

#' @title
#' Sample a truncated skewed normal distribution
#'
#' @description
#' Sample from a skewed normal distribution using the `rsn` function
#' Truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param xi (`numeric`) The center of the skewed normal distribution
#'
#' @param omega (`numeric`) The spread of the skewed normal distribution
#'
#' @param alpha (`numeric`) The skewness of the skewed normal distribution
#'
#' @param min (`numeric`) The minimum value to truncate the data to.
#'
#' @param max (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the skewed normal distribution, truncated to the specified range.
#'
#' @importFrom sn rsn
#' @export
#'
rsn_trunc <- function(n, xi, omega, alpha, min, max, seed = NULL) {
  # checks for input validity
  if (any(min > max)) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # first sampling of results
  res <- rsn(n = n, xi = xi, omega = omega, alpha = alpha)
  if (n == 1) {
    # if only one number is sampled
    while (res[1] < min | res[1] > max) {
      res <- rsn(n = 1, xi = xi, omega = omega, alpha = alpha)
    }
    return(res[1])
  } else {
    # re-sample until all values are in the specified range
    while (sum(res < min) + sum(res > max) > 0) {
      res[c(res < min | res > max)] <- rsn(
        n = sum(res < min) + sum(res > max),
        xi = xi,
        omega = omega,
        alpha = alpha
      )
    }
  }
  return(res)
}

#' @title
#' Chopped, skewed normal distribution for time variables
#'
#' @description
#' The function samples from a skewed normal distribution using `rsn` to obtain time of day data in hours.
#' Values greater than 24 are subtracted by 24 (moved to the next day) so that a real time variable is observed.
#'
#' @param nrow (`integer`) The number of data points to sample
#'
#' @param xi (`numeric`) The center of the skewed normal distribution
#'
#' @param omega (`numeric`) The spread of the skewed normal distribution
#'
#' @param alpha (`numeric`) The skewness of the skewed normal distribution
#'
#' @param min (`numeric`) Optional, a minimum value to left truncate the value to; the default value is set to 0.
#'
#' @param max (`numeric`) Optional, a maximum value to right truncate the value to; the default value is set to 48.
#'
#' @param seed (`integer`) Optional, an integer for setting the seed for reproducible results.
#'
#'
#' @return A numeric vector following the specified distribution.
#'
#' @export
#'
sample_time_shifted <- function(nrow, xi, omega, alpha, min = 0, max = 48, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (min > max) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  # sampling of skewed normal distribution
  time_orig <- rsn_trunc(
    n = nrow,
    xi = xi,
    omega = omega,
    alpha = alpha,
    min = min,
    max = max,
  )
  # times greater than 24 hours are after 12am
  # subtract 25 to turn 12am into 00:00
  final_time <- ifelse(time_orig >= 24,
    time_orig - 24,
    time_orig
  )
  return(final_time)
}


#' @title
#' Chopped log normal distribution for time variables
#'
#' @description
#' The function samples from a log normal using `rlnorm` to obtain time of day data in hours.
#' Values greater than 24 are subtracted by 24 (moved to the next day) so that a real time variable is observed.
#'
#' @param nrow (`integer`) The number of data points to sample
#'
#' @param meanlog (`numeric`) The log mean for the distribution
#'
#' @param sdlog (`numeric`) The log standard deviation
#'
#' @param min (`numeric`) Optional, a minimum value to left truncate the value to; the default value is set to 0.
#'
#' @param max (`numeric`) Optional, a maximum value to right truncate the value to; the default value is set to 48.
#'
#' @param seed (`integer`) Optional, an integer for setting the seed for reproducible results.
#'
#'
#' @return A numeric vector following the specified distribution.
#'
#' @export
#'
sample_time_shifted_lnorm <- function(nrow, meanlog, sdlog, min = 0, max = 48, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (min > max) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  sample_dist <- function(nrow, meanlog, sdlog) {
    # sampling of skewed normal distribution
    time_orig <- rlnorm(
      n = nrow,
      meanlog = meanlog,
      sdlog = sdlog
    )
    # times greater than 24 hours are after 12am
    # subtract 25 to turn 12am into 00:00
    final_time <- ifelse(time_orig >= 24,
      time_orig - 24,
      time_orig
    )
    return(final_time)
  }
  res <- sample_dist(nrow, meanlog, sdlog)
  while (sum(res < min) + sum(res > max) > 0) {
    oor_sum <- sum(res < min) + sum(res > max)
    res[c(res < min | res > max)] <- sample_dist(oor_sum, meanlog, sdlog)
  }
  return(res)
}


#' @title
#' Generate a data table with basic inpatient stay information.
#' At the minimum, it will include an encounter and hospital ID,
#' along with other information if `cohort` is included in the input.
#'
#' @description
#' This function creates a data table of simulated encounter IDs and hospital IDs.
#' The creation is either based on user's desired number of encounters and unique hospitals,
#' or based on a given set of encounter IDs. It can be used to create long format data tables
#' where users have control over average repeat frequency.

#'
#' @param nid (`integer`)\cr Optional, number of unique encounter IDs to simulate
#'
#' @param n_hospitals (`integer`)\cr Optional, number of hospitals to simulate and assign to encounter IDs
#'
#' @param avg_repeats (`numeric`)\cr The average number of repeats per row in the final data table
#'
#' @param include_prop (`numeric`)\cr A number between 0 and 1,
#' for the proportion of unique rows in `cohort` to include in the final data table
#'
#' @param cohort (`data.table`)\cr Optional, resembling the GEMINI "admdad" table to build the returned data table from
#'
#' @param by_los (`logical`)\cr Optional, whether to assign more repeats to longer hospital stays or not.
#' Default to FALSE. When TRUE, two additional columns are required in the input `cohort` dataset -
#' `admission_date_time` and `discharge_date_time` for calculating length of stay.
#'
#' @param seed (`integer`)\cr Optional, a number for setting the seed for reproducible results
#'
#' @return (`data.table`)\cr A data.table object with the same columns as `cohort`,
#' but with some rows excluded and/or repeated based on user specifications.
#' If `cohort` is not included, then it will have the following fields:
#' - `genc_id` (`integer`): GEMINI encounter number, may be repeated in multiple rows based on avg_repeats
#' - `hospital_num` (`integer`): An integer identifying the hospital attached to the encounter
#'
#' @export
#'
#' @examples
#' sample_cohort <- data.table::data.table(genc_id = 1:100, hospital_num = rep(1:5, each = 20))
#' generate_id_hospital(cohort = sample_cohort, include_prop = 0.8, avg_repeats = 1.5, by_los = TRUE, seed = 1)
#' generate_id_hospital(nid = 1000, n_hospitals = 10, avg_repeats = 1)
#'
generate_id_hospital <- function(
  nid = 1000, n_hospitals = 10, avg_repeats = 1.5, include_prop = 1, cohort = NULL, by_los = FALSE, seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(cohort)) {
    # if cohort is not provided, create data.table out of parameters
    hosp_names <- seq(1:n_hospitals)
    hosp_assignment <- sample(hosp_names, nid, replace = TRUE) # this creates randomness in hospital size

    # simulate number of repeats for each id (from Poisson)
    if (avg_repeats != 1) {
      n_repeats <- rpois(nid, lambda = avg_repeats) # random sample number of repeats for each id
      n_repeats[n_repeats == 0] <- 1
    } else {
      n_repeats <- rep(1, nid)
    }
    # expand ids and sites
    id_list <- 1:nid
    id_vector <- rep(id_list, times = n_repeats)
    site_vector <- rep(hosp_assignment, times = n_repeats)

    res <- data.table(genc_id = id_vector, hospital_num = site_vector, stringsAsFactors = FALSE)
  } else {
    include_set <- cohort[sample(seq_len(nrow(cohort)), round(include_prop * nrow(cohort))), ]

    if (avg_repeats == 1) {
      n_repeats <- rep(1, nrow(include_set))
    } else {
      # sample rows with repeats
      n_repeats <- rpois(nrow(include_set), lambda = avg_repeats)
      n_repeats[n_repeats == 0] <- 1
    }

    # may sort by LOS to assign more repeats to longer stays
    if (by_los) {
      # convert date times to a useable format
      include_set$admission_date_time <- as.POSIXct(include_set$admission_date_time,
        format = "%Y-%m-%d %H:%M"
      )
      include_set$discharge_date_time <- as.POSIXct(include_set$discharge_date_time,
        format = "%Y-%m-%d %H:%M"
      )

      include_set$los <- as.numeric(difftime(
        include_set$discharge_date_time,
        include_set$admission_date_time,
        units = "hours"
      ))
      # order from shortest to longest
      include_set <- include_set[order(los)]
      n_repeats <- sort(n_repeats)

      res <- include_set[rep(seq_len(.N), times = n_repeats), ]
    } else {
      # if not sorting by LOS, just assign repeats randomly
      res <- include_set[rep(seq_len(.N), times = n_repeats), ]
    }
  }

  res[, genc_id := as.integer(genc_id)]
  res[, hospital_num := as.integer(hospital_num)]
  return(res)
}


#' @title
#' Checks a character input to verify that it is as valid date or date time format
#'
#' @description
#' This function checks the format of a `character` object so that it can be converted to a Date or POSIXct type.
#' The formats are:
#' - Date: "YYYY-mm-dd" or "YYYY"
#' - Date time (to convert to POSIXct): "YYYY-mm-dd hh:mm"
#'
#' @param x (`character`)\cr The string to be checked for format.
#'
#' @param check_time (`logical`)\cr Optional, a flag indicating whether the function will check for
#' a date or date time format. The default is `FALSE`, meaning it will check for a date only.
#'
#' @return (`logical`)\cr The function returns `TRUE` if the input was a valid date or date time format.
#' Otherwise, it returns `FALSE`.
#'
#' @export
#'
#' @examples
#' check_date_format("2020-01-01", check_time = FALSE)
#' check_date_format("2021-01-01 12:01", check_time = TRUE)
#' check_date_format(c("2015-12-31 01:01", "2016-01-01 01:01"), check_time = TRUE)
#' check_date_format("November 13th, 2025")
#'
check_date_format <- function(x, check_time = FALSE) {
  x <- as.character(x)
  x_trim <- trimws(x)
  if (check_time == FALSE) {
    return(
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x_trim) | grepl("^[0-9]{4}$", x_trim)
    )
  } else {
    x_trim <- substr(x_trim, 1, 16) # removes seconds from the date time object
    return(
      grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}$", x_trim)
    )
  }
}
