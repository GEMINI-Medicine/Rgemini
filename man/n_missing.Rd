% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_missing.R
\name{n_missing}
\alias{n_missing}
\alias{mi2}
\title{Number of missingness}
\usage{
n_missing(x, na_strings = c(""), index = FALSE)

mi2(x, na_strings = c(""), index = FALSE)
}
\arguments{
\item{x}{(\code{vector} or \code{data.frame} or \code{data.table})
the object to be checked}

\item{na_strings}{(\code{character})
a character vector of strings which are to be interpreted as \code{NA} values.
The default for \code{n_missing} is "", which will treat empty strings as \code{NA}.}

\item{index}{(\code{logical})
If true the function returns the index of the missing values instead of the
number (percentage). For \code{vector} input, a logical vector of same length will
be returned. For \code{data.frame} or \code{data.table} input, the function returns a
vector with length equal to the number of rows of input table and any
missingness in the row will result in a returned value of \code{TRUE} for the row.}
}
\value{
(\code{character} or \code{logical})
For \code{vector} input, a \code{character} vector the same length of \code{x} where
each value represents whether that particular element is missing.
For \code{data.frame} or \code{data.table} input, a \code{logical} vector of which the
length is the number of rows of input table and each value represents
whether any element of the row is missing.
}
\description{
This function checks number of missingness in a \code{vector}, or a \code{data.frame}
or \verb{data table}. It returns the results in exact number (percentage) or
returns the index of missingness
}
\examples{
df <- data.frame(
  x = c(1, 2, NA, 4, 5),
  y = as.POSIXct(c(
    "2023-12-01 15:00", "2021-07-09 19:30", "2020-01-02 09:00",
    "1998-08-23 15:23", NA
  )),
  z = c("NA", NA, "a", " ", ""),
  v = as.Date(c("2023-10-23", "2012-06-30", NA, "2023-08-12", "2009-01-01"))
)
n_missing(df)
n_missing(df$z)
n_missing(df, na_strings = c("", "NA", " "))
n_missing(df$z, index = TRUE)
n_missing(df, na_strings = c("", "NA", " "), index = TRUE)

}
