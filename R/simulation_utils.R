#' @title
#' Sample a truncated log normal distribution
#'
#' @description
#' Sample from a log normal distribution using the `rlnorm` function but truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param meanlog (`numeric`) The mean of the log normal distribution
#'
#' @param sdlog (`numeric`) The standard deviation of the log normal distribution
#'
#' @param min_n (`numeric`) The minimum value to truncate the data to.
#'
#' @param max_n (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the log normal distribution, truncated to the specified range.
rlnorm_trunc <- function(n, meanlog, sdlog, min_n, max_n, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  res <- rlnorm(n, meanlog, sdlog)
  # keep redrawing until all are in range
  bad <- (res < min_n) | (res > max_n)
  while (any(bad)) {
    res[bad] <- rlnorm(sum(bad, na.rm = TRUE), meanlog, sdlog)
    bad <- (res < min_n) | (res > max_n)
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
#' @param min_n (`numeric`) The minimum value to truncate the data to.
#'
#' @param max_n (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the normal distribution, truncated to the specified range.
rnorm_trunc <- function(n, mean, sd, min_n, max_n, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  res <- rnorm(n, mean, sd)
  while (sum(res < min_n) + sum(res > max_n) > 0) {
    res[c(res < min_n | res > max_n)] <- rnorm(
      sum(res < min_n) + sum(res > max_n),
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
#' Sample from a skewed normal distribution using the `rsn` function but truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param xi (`numeric`) The center of the skewed normal distribution
#'
#' @param omega (`numeric`) The spread of the skewed normal distribution
#'
#' @param alpha (`numeric`) The skewness of the skewed normal distribution
#'
#' @param min_n (`numeric`) The minimum value to truncate the data to.
#'
#' @param max_n (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the skewed normal distribution, truncated to the specified range.
rsn_trunc <- function(n, xi, omega, alpha, min_n, max_n, seed = NULL) {
  # checks for input validity
  if (min_n > max_n) {
    stop()
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # first sampling of results
  res <- rsn(n = n, xi = xi, omega = omega, alpha = alpha)
  if (n == 1) {
    # if only one number is sampled
    while (res[1] < min_n | res[1] > max_n) {
      res <- rsn(n = 1, xi = xi, omega = omega, alpha = alpha)
    }
    return(res[1])
  } else {
    # re-sample until all values are in the specified range
    while (sum(res < min_n) + sum(res > max_n) > 0) {
      res[c(res < min_n | res > max_n)] <- rsn(
        n = sum(res < min_n) + sum(res > max_n),
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
#' The function samples from a skewed normal distribution using `rsn` to obtain time of day data in hours. Values greater than 24 are subtracted by 24 (moved to the next day) so that a real time variable is observed.
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
sample_time_shifted <- function(nrow, xi, omega, alpha, min = 0, max = 48, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # sampling of skewed normal distribution
  time_orig <- rsn_trunc(
    n = nrow,
    xi = xi,
    omega = omega,
    alpha = alpha,
    min_n = min,
    max_n = max,
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
#' The function samples from a log normal using `rlnorm` to obtain time of day data in hours. Values greater than 24 are subtracted by 24 (moved to the next day) so that a real time variable is observed.
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
sample_time_shifted_lnorm <- function(nrow, meanlog, sdlog, min = 0, max = 48, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
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
    bad_sum <- sum(res < min) + sum(res > max)
    res[c(res < min | res > max)] <- sample_dist(bad_sum, meanlog, sdlog)
  }
  return(res)
}


#' @title
#' Generate a data table with basic inpatient stay information. At the minimum, it will include an encounter and hospital ID, along with other information if `cohort` is included in the input.
#'
#' @description
#' This function creates a data table based on the input, either based on a number of unique IDs or an existing inpatient data table. It can be used to create long format data tables based on inpatient data or to create a new data based on user specifications.
#'
#' @param nid (`integer`)\cr Optional, number of unique encounter IDs to simulate
#'
#' @param n_hospitals (`integer`)\cr Optional, number of hospitals to simulate and assign to encounter IDs
#'
#' @param cohort (`data.table`)\cr Optional, resembling the GEMINI "admdad" table to build the returned data table from
#'
#' @param include_prop (`numeric`)\cr A number between 0 and 1, for the proportion of unique rows in `cohort` to include in the final data table
#'
#' @param avg_repeats (`numeric`)\cr The average number of repeats per row in the final data table
#'
#' @param by_los (`logical`)\cr Optional, whether to assign more repeats to longer hospital stays or not
#'
#' @param seed (`integer`)\cr Optional, a number for setting the seed for reproducible results
#'
#' @return (`data.table`)\cr A data.table object with the same columns as `cohort`, but with some rows excluded and/or repeated based on user specifications. If `cohort` is not included, then it will only have the following fields:
#' - `genc_id` (`integer`): GEMINI encounter number, may be repeated in multiple rows based on avg_repeats
#' - `hospital_num` (`integer`): An integer identifying the hospital attached to the encounter
#'
#' @examples
#' generate_id_hospital(cohort = admdad, include_prop = 0.8, avg_repeats = 1.5, by_los = TRUE, seed = 1)
#' generate_id_hospital(nid = 1000, n_hospitals = 10, avg_repeats = 1)
#'
generate_id_hospital <- function(nid = 1000, n_hospitals = 10, avg_repeats = 1.5, include_prop = 1, cohort = NULL, by_los = FALSE, seed = NULL) {
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
    cohort$admission_date_time <- as.POSIXct(cohort$admission_date_time,
      format = "%Y-%m-%d %H:%M"
    )
    cohort$discharge_date_time <- as.POSIXct(cohort$discharge_date_time,
      format = "%Y-%m-%d %H:%M"
    )

    include_set <- cohort[sample(1:nrow(cohort), round(include_prop * nrow(cohort))), ]

    if (avg_repeats == 1) {
      n_repeats <- rep(1, nrow(include_set))
    } else {
      # sample rows with repeats
      n_repeats <- rpois(nrow(include_set), lambda = avg_repeats)
      n_repeats[n_repeats == 0] <- 1
    }

    # may sort by LOS to assign more repeats to longer stays
    if (by_los) {
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

  return(res)
}
