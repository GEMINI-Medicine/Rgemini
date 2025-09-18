#' @title
#' Maximum Pairwise Standardized Mean Difference
#'
#' @param x named (`list`)\cr
#' Where each list element is a vector corresponding to the observations of the variable of interest
#' for that particular strata (corresponding to the name). See example for how this can be constructed.
#'
#' @param name (`character`)\cr
#' Unused variable that is required to be supported by `extra.col` in [table1::table1()].
#'
#' @param round_to (`numeric`)\cr
#' How many digits to round the standardized mean difference to.
#'
#' @param ... \cr
#' Additional arguments passed on to [table1::table1()].
#'
#' @return (`numeric`)\cr
#' The maximum pairwise standardized mean difference between all strata for a particular variable.
#'
#' @note
#' The implementation with the `stddiff` package is more fragile, than with the `smd` package.
#' However, the `smd` package uses the *population variance* to calculate the SMD as opposed to the *sample variance*.
#' This can cause small inaccuracies in the final result. Therefore we elect to implement with `stddiff`.
#' Another consideration is that `stddiff` can be maximally precise to only 3 decimal places.
#'
#' Additionally, for very large cohorts, standardized mean differences calculated on categorical variables using the
#' `stddiff` package may throw `In n[1] * n[2] : NAs produced by integer overflow`. This is an implementation issue
#' in `stddiff` in the calculation of the standard errors for the standardized mean differences. However, since these
#' standard errors are not used in the final output, they can be safely ignored.
#'
#' @import stddiff
#' @export
#'
#' @examples
#' max_pairwise_smd(split(mtcars$disp, mtcars$am))
#'
max_pairwise_smd <- function(x, name, round_to = 3, ...) {
  x[["overall"]] <- NULL # remove overall category if exists

  x <- reshape2::melt(x)
  x$L1 <- as.numeric(as.factor(x$L1)) - 1 # needs to start at 0 for stddiff
  pairs <- unique(x$L1) %>% combn(2, simplify = FALSE)

  vartype <- class(x$value)
  vartype <- vartype[!vartype == "labelled"]

  fn <- if ((vartype == "numeric") || is.integer(x$value)) {
    stddiff.numeric
  } else if (vartype == "logical") {
    stddiff.binary
  } else if (vartype %in% c("factor", "character")) {
    stddiff.category
  }

  max_smd <- 0

  for (pair in pairs) {
    current_smd <- max(
      fn(
        x %>% dplyr::filter(L1 %in% pair) %>% droplevels(), # drop factor levels, otherwise singularity may arise for group(s) containing an empty level
        2, 1
      ) %>% .[[1, "stddiff"]] # alternate reference group through every group
    )

    if (is.na(current_smd)) {
      warning("Some pairwise SMDs could not be calculated. Please investigate.", .call = FALSE)
      max_smd <- current_smd
    } else if ((current_smd > max_smd) || is.na(max_smd)) {
      max_smd <- current_smd
    }
  }

  return(round(max_smd, round_to))
}


#' @title
#' Render Cell Suppression (Default)
#'
#' @description
#' This is a wrapper around the render functions for each type of variable.
#'
#' @param x (`vector`)\cr
#' A vector of numeric, factor, character or logical values.
#'
#' @param name (`character`)\cr
#' Name of the variable to be rendered (ignored).
#'
#' @param missing (`logical`)\cr
#' Should missing values be included?
#'
#' @param transpose (`logical`)\cr
#' Logical indicating whether on not the table is transposed.
#'
#' @param render.empty (`character`)\cr
#' A character to return when x is empty.
#'
#' @param render.continuous (`function`)\cr
#' A function to render continuous (i.e. numeric) values. Can also be a character string,
#' in which case it is passed to `table1:::parse.abbrev.render.code()`.
#'
#' @param render.categorical (`function`)\cr
#' A function to render categorical (i.e. factor, character or logical) values. Can also be a character string,
#' in which case it is passed to `table1:::parse.abbrev.render.code()`.
#'
#' @param render.missing (`function`)\cr
#' A function to render missing (i.e. NA) values. Can also be a character string,
#' in which case it is passed to `table1:::parse.abbrev.render.code()`. Set to `NULL` to ignore missing values.
#'
#' @param ... \cr
#' Further arguments, passed to `table1:::stats.apply.rounding()` or
#' `prettyNum()` for additional formatting (e.g., `big.mark = ","`).
#'
#' @return (`character`)\cr
#' Summary of variable as a character vector with cell suppression applied.
#'
#' @import table1
#' @export
#'
render_cell_suppression.default <- function(
    x,
    name,
    missing = any(is.na(x)),
    transpose = FALSE,
    render.empty = "NA",
    render.continuous = render_cell_suppression.continuous,
    render.categorical = render_cell_suppression.categorical,
    render.missing = render_cell_suppression.missing,
    ...) {
  args <- list(...)

  if (length(x) == 0) {
    return(render.empty)
  }

  if (is.logical(x)) x <- factor(x, levels = c(T, F), labels = c("Yes", "No"))

  if (is.factor(x) || is.character(x)) {
    r <- do.call(render.categorical, list(x = x, ...))
  } else if (is.numeric(x)) {
    r <- do.call(render.continuous, list(x = x, ...))
  } else {
    stop(paste("Unrecognized variable type:", class(x)))
  }

  if (missing && !is.null(render.missing)) {
    r <- c(r, do.call(render.missing, list(x = x, ...)))
  }

  if (transpose) {
    if (!is.null(names(r))) {
      r <- paste0(sprintf("%s: %s", names(r), r), collapse = "<br/>")
    } else {
      r <- paste0(r, collapse = "<br/>")
    }
  }

  return(r)
}


#' @title
#' Render Cell Suppression (Categorical)
#'
#' @description
#' This is a custom render for `table1` categorical variables which performs GEMINI
#' "cell suppression" for any variable levels which contain fewer than 6 observations.
#'
#' If the total number of these variable levels with fewer than 6 observations is less
#' than 6, *all* cells for all levels of the variable must be censored (because it is
#' possible to indirectly deduce the missing counts otherwise).
#'
#' @param x (`character` or `factor`)\cr
#' A categorical variable to summarize.
#'
#' @param ... \cr
#' Optionally accept a named `digits` (`integer`) or `single_level_binary` (`logical`) argument
#' which specifies the number of digits to round percentages to.
#' Also accepts additional inputs that are passed to `prettyNum()` to apply
#' additional formatting to shown results (e.g., `big.mark = ","`).
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @importFrom table1 stats.default
#' @export
#'
#' @examples
#' x <- factor(c(rep("a", times = nrow(mtcars)), "b"), levels = c("a", "b"))
#' render_cell_suppression.categorical(x)
#'
#' x2 <- factor(c(rep("a", times = nrow(mtcars))), levels = c("a", "b"))
#' render_cell_suppression.categorical(x2)
#'
#' y <- factor(
#'   c(rep("a", times = nrow(mtcars)), "b", "c", "d", "e", "f", "g"),
#'   levels = c("a", "b", "c", "d", "e", "f", "g")
#' )
#' render_cell_suppression.categorical(y)
#'
#' z <- factor(
#'   c(
#'     rep("a", times = 100),
#'     rep("b", times = 50),
#'     rep("c", times = 7),
#'     rep("d", times = 2)
#'   ),
#'   levels = c("a", "b", "c", "d", "e")
#' )
#'
#' render_cell_suppression.categorical(z)
#'
render_cell_suppression.categorical <- function(x, ...) {
  args <- list(...)

  if (!is.null(args$digits)) {
    output_format <- paste0("%d (%0.", args$digits, "f%%)")
  } else {
    output_format <- "%d (%0.1f%%)"
  }

  contents <- vapply(
    stats.default(x),
    function(y) with(y, c(frequency = FREQ, pct = PCT)),
    numeric(2)
  ) %>%
    t() %>%
    as.data.frame()
  # if there are > 6 observations between levels that are < 6 within levels,
  # these can be displayed
  levels_w_fewer_than_6_obs <- contents %>%
    dplyr::filter(frequency < 6 & frequency != 0) %>% # no need to suppress true 0s
    select(frequency) %>%
    nrow()

  if (levels_w_fewer_than_6_obs >= 6) {
    contents <- contents %>%
      transmute(
        summary = ifelse(
          frequency < 6 & frequency != 0, "&lt; 6 obs. (suppressed)",
          sprintf(output_format, frequency, pct)
        )
      )
  } else if (levels_w_fewer_than_6_obs > 0) {
    # suppress enough such that the suppressed values cannot be derived with reasonable precision (within 6)
    contents <- contents %>%
      mutate(id = row_number()) %>%
      arrange(-frequency) %>%
      mutate(remainder = sum(frequency) - cumsum(frequency)) %>%
      mutate(summary = ifelse(((frequency < 6) | (remainder < 6)) & frequency != 0, "(suppressed)", sprintf(output_format, frequency, pct))) %>%
      arrange(id) %>%
      select(summary)
  } else {
    contents <- contents %>%
      transmute(summary = sprintf(output_format, frequency, pct))
  }

  res <- prettyNum(t(contents), ...) %>% as.character()
  names(res) <- colnames(t(contents))

  if (
    !is.null(args$single_level_binary) &&
    args$single_level_binary &&
    length(res) == 2
  ) {
    res <- res[1]
  }

  return(c("", res))
}


#' @title
#' Render Strict Cell Suppression (Categorical)
#'
#' @description
#' Strictly suppress any counts that are less than 6 with message. This differs
#' from the more conservative logic of [Rgemini::render_cell_suppression.categorical()].
#'
#' @param x (`character` or `factor`)\cr
#' A categorical variable to summarize.
#'
#' @param ... \cr
#' Optionally accept a named `digits` (`integer`) or `single_level_binary` (`logical`) argument
#' which specifies the number of digits to round percentages to.
#' Also accepts additional inputs that are passed to `prettyNum()` to apply
#' additional formatting to shown results (e.g., `big.mark = ","`).
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @export
#'
#' @examples
#' x <- factor(c(rep("a", times = nrow(mtcars)), "b"), levels = c("a", "b"))
#' render_strict_cell_suppression.categorical(x)
#'
#' x2 <- factor(c(rep("a", times = nrow(mtcars))), levels = c("a", "b"))
#' render_strict_cell_suppression.categorical(x2)
#'
#' y <- factor(
#'   c(rep("a", times = nrow(mtcars)), "b", "c", "d", "e", "f", "g"),
#'   levels = c("a", "b", "c", "d", "e", "f", "g")
#' )
#' render_strict_cell_suppression.categorical(y)
#'
#' z <- factor(
#'   c(
#'     rep("a", times = 100),
#'     rep("b", times = 50),
#'     rep("c", times = 7),
#'     rep("d", times = 2)
#'   ),
#'   levels = c("a", "b", "c", "d", "e")
#' )
#'
#' render_strict_cell_suppression.categorical(z)
#'
render_strict_cell_suppression.categorical <- function(x, ...) {
  args <- list(...)

  if (!is.null(args$digits)) {
    output_format <- paste0("%d (%0.", args$digits, "f%%)")
  } else {
    output_format <- "%d (%0.1f%%)"
  }

  contents <- vapply(
    stats.default(x),
    function(y) with(y, c(frequency = FREQ, pct = PCT)),
    numeric(2)
  ) %>%
    t() %>%
    as.data.frame()

  contents <- contents %>%
    transmute(
      summary = ifelse(
        (frequency < 6) & (frequency != 0), "&lt; 6 obs. (suppressed)",
        sprintf(output_format, frequency, pct)
      )
    )

  res <- prettyNum(t(contents), ...) %>% as.character()
  names(res) <- colnames(t(contents))

  if (
    !is.null(args$single_level_binary) &&
    args$single_level_binary &&
    length(res) == 2
  ) {
    res <- res[1]
  }

  return(c("", res))
}


#' @title
#' Render Mean (Continuous)
#'
#' @description
#' This is the default renderer for continuous variables in the `table1` package. It will
#' generate a formatted mean and standard deviation for each level of the variable.
#'
#' @param x (`character` or `factor`)\cr
#' A continuous variable to summarize.
#'
#' @param ... \cr
#' Further arguments, passed to `table1:::stats.apply.rounding()`.
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @importFrom table1 stats.apply.rounding
#' @export
#'
#' @examples
#' render_mean.continuous(mtcars$disp)
#'
render_mean.continuous <- function(x, ...) {
  with(
    stats.apply.rounding(stats.default(x, ...), rounding.fn = round_pad, ...),
    c("", "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD))
  )
}


#' @title
#' Render Median (Continuous)
#'
#' @description
#' This is the default renderer for continuous variables in the `table1` package. It will
#' generate a formatted median with first and third quartiles for each level of the variable.
#'
#' @param x (`character` or `factor`)\cr
#' A continuous variable to summarize.
#'
#' @param ... \cr
#' Further arguments, passed to `table1:::stats.apply.rounding()`.
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @importFrom table1 stats.apply.rounding
#' @export
#'
#' @examples
#' render_median.continuous(mtcars$disp)
#'
render_median.continuous <- function(x, ...) {
  with(
    stats.apply.rounding(stats.default(x, ...), rounding.fn = round_pad, ...),
    c("", "Median [Q1, Q3]" = sprintf("%s [%s, %s]", MEDIAN, Q1, Q3))
  )
}


#' @title
#' Render Cell Suppression (Continuous)
#'
#' @description
#' This is a custom renderer for `table1` continuous variables which performs GEMINI
#' "cell suppression" for any variable levels which contain fewer than 6 observations.
#'
#' @param x (`numeric`)\cr
#' A continuous variable to summarize.
#'
#' @param ... \cr
#' Further arguments, such as `continuous_fn`, or those passed to `table1:::stats.apply.rounding()`.
#' Use `continuous_fn` to specify the summary statistics to display,
#' which accepts character string: "mean", "median", or c("mean", "median") to display both. Defaults to "mean".
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @export
#'
#' @examples
#' x <- 1:6
#' render_cell_suppression.continuous(x)
#'
#' y <- 1:2
#' render_cell_suppression.continuous(y)
#'
#' ## Use in `table1`:
#' \dontrun{
#' library(table1)
#' dat <- expand.grid(id = 1:10, treat = c("Treated", "Placebo"))
#' dat$age <- runif(nrow(dat), 10, 50)
#' label(dat$age) <- "Age"
#'
#' table1(~ age | treat,
#'   data = dat,
#'   render.continuous = render_cell_suppression.continuous,
#'   continuous_fn = c("mean", "median"), # to display mean and median simultaneously
#'   digits = 2
#' )
#' }
#'
render_cell_suppression.continuous <- function(x, ...) {
  args <- list(...)

  if (is.null(args$continuous_fn)) {
    args$continuous_fn <- "mean"
  }

  if (length(x) < 6) {
    mea <- c("", `Mean (SD)` = "&lt; 6 obs. (suppressed)")
    med <- c("", `Median [Q1, Q3]` = "&lt; 6 obs. (suppressed)")
  } else {
    mea <- render_mean.continuous(x, ...)
    med <- render_median.continuous(x, ...)
  }

  if (all(args$continuous_fn == "mean")) {
    res <- mea
  } else if (all(args$continuous_fn == "median")) {
    res <- med
  } else {
    res <- c(mea, med)
  }

  return(res)
}


#' @title
#' Render Cell Suppression (Strata)
#'
#' @description
#' This is a custom render for `table1` stratification variables which performs GEMINI
#' "cell suppression" for any levels which contain fewer than 6 observations.
#'
#' Note that even with strata variable cell suppression, it is possible to reverse-calculate
#' the total given the overall column. Therefore it is recommended to also hide the "Overall"
#' column in the call to [table1::table1()].
#'
#' @param label (`character`)\cr
#' For table1 versions up to 1.4.3: A character vector containing the labels.
#' For table1 versions >= 1.5.0: A list item with data for each strata.
#'
#' @param transpose (`logical`)\cr
#' Used internally by [table1::table1()].
#'
#' @param ... \cr
#' Optional additional arguments. Note that the current version expects this to
#' be n for each strata, mimicing the behavior of table1 version <= 1.4.3, where
#' n was explicitly passed to this function.
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @note
#' Arguments from this function should not be passed directly and are defined here
#' to work internally with [table1::table1()].
#'
#' @export
#'
render_cell_suppression.strat <- function(label, ..., transpose = FALSE) {

  # Since table1 version 1.5.0:
  # `label` is a list so we need to extract relevant info here
  if (is.list(label)) {
    n <- sapply(label, nrow)
    label <- names(n)
  } else {
    # For previous table1 versions (<= 1.4.3): n was explicitly passed; here
    # we check for implicit arguments to accommodate all versions of table1
    n <- list(...)[[1]]
  }

  sprintf(
    ifelse(
      is.na(n),
      "<span class='stratlabel'>%s</span>",
      ifelse(
        as.numeric(gsub("[^0-9.-]", "", n)) < 6, # need to remove any optional formatting here to check numeric value
        "<span class='stratlabel'>%s<br><span class='stratn'>(N&lt; 6 obs. (suppressed))</span></span>",
        "<span class='stratlabel'>%s<br><span class='stratn'>(N=%s)</span></span>"
      )
    ), label, n
  )
}


#' @title
#' Render Default (Discrete)
#'
#' @description
#' This is the default render for discrete variables in the `table1` package. It will
#' generate a sum for each level of the variable.
#'
#' @param x (`character` or `factor`)\cr
#' A discrete variable to summarize.
#'
#' @param ... \cr
#' Further arguments, passed to `prettyNum()` for additional formatting (e.g.,
#' `big.mark = ","`).
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @importFrom table1 stats.default
#' @export
#'
#' @examples
#' render_default.discrete(mtcars$vs)
#'
render_default.discrete <- function(x, ...) {
  with(
    stats.default(x),
    c("", Sum = prettyNum(sprintf("%s", SUM), ...))
  )
}

#' @title
#' Render Cell Suppression (Discrete)
#'
#' @description
#' This is a custom render for `table1` discrete variables which performs GEMINI
#' "cell suppression" for any variable levels which contain fewer than 6 observations.
#'
#' This is useful when you have an indicator variable for example, and you would like to
#' count the total number of events. `[table1::render.default.categorical()]` will break
#' down the indicator variable into its components first (0 and 1) and then give you individual
#' counts. This will simply count 1s (for example).
#'
#' @param x (`character` or `factor`)\cr
#' A discrete variable to summarize.
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @importFrom table1 stats.default
#' @export
#'
#' @examples
#' x <- 1:6
#' render_cell_suppression.discrete(x)
#'
#' y <- 1:2
#' render_cell_suppression.discrete(y)
#'
render_cell_suppression.discrete <- function(x) {
  if (sum(x) < 6) {
    c("", Sum = "&lt; 6 obs. (suppressed)")
  } else {
    render_default.discrete(x)
  }
}

#' @title
#' Render Cell Suppression (Missing)
#'
#' @description
#' This is a custom render for `table1` missing variables which performs GEMINI
#' "cell suppression" for any variable levels which contain fewer than 6 observations.
#'
#' This is useful when you have an indicator variable for example, and you would like to
#' count the total number of events. `[table1::render.default.categorical()]` will break
#' down the indicator variable into its components first (0 and 1) and then give you individual
#' counts. This will simply count 1s (for example).
#'
#' @param x (`character` or `factor`)\cr
#' A variable with missing values to summarize.
#'
#' @param ... \cr
#' Further arguments, passed to `table1:::stats.apply.rounding()` and
#' `prettyNum()` for additional formatting (e.g., `big.mark = ","`).
#'
#' @return named (`character`)\cr
#' Concatenated with `""` to shift values down one row for proper alignment.
#'
#' @importFrom table1 render.missing.default
#' @export
#'
#' @examples
#' x <- factor(sample(0:1, 99, replace = TRUE), labels = c("Female", "Male"))
#' x[1:3] <- NA
#' render_cell_suppression.missing(x)
#'
#' x[5:10] <- NA
#' render_cell_suppression.missing(x)
#'
render_cell_suppression.missing <- function(x, ...) {
  args <- list(...)
  if (sum(is.na(x)) < 6) {
    c("", Missing = "&lt; 6 obs. (suppressed)")
  } else {
    if (!is.null(args$digits)) {
      prettyNum(render.missing.default(x, digits.pct = args$digits), ...)
    } else {
      prettyNum(render.missing.default(x), ...)
    }
  }
}
