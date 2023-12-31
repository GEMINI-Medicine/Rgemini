% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell_suppression.R
\name{max_pairwise_smd}
\alias{max_pairwise_smd}
\title{Maximum Pairwise Standardized Mean Difference}
\usage{
max_pairwise_smd(x, name, round_to = 3, ...)
}
\arguments{
\item{x}{named (\code{list})\cr
Where each list element is a vector corresponding to the observations of the variable of interest
for that particular strata (corresponding to the name). See example for how this can be constructed.}

\item{name}{(\code{character})\cr
Unused variable that is required to be supported by \code{extra.col} in \code{\link[table1:table1]{table1::table1()}}.}

\item{round_to}{(\code{numeric})\cr
How many digits to round the standardized mean difference to.}

\item{...}{\cr
Additional arguments passed on to \code{\link[table1:table1]{table1::table1()}}.}
}
\value{
(\code{numeric})\cr
The maximum pairwise standardized mean difference between all strata for a particular variable.
}
\description{
Maximum Pairwise Standardized Mean Difference
}
\note{
The implementation with the \code{stddiff} package is more fragile, than with the \code{smd} package.
However, the \code{smd} package uses the \emph{population variance} to calculate the SMD as opposed to the \emph{sample variance}.
This can cause small inaccuracies in the final result. Therefore we elect to implement with \code{stddiff}.
Another consideration is that \code{stddiff} can be maximally precise to only 3 decimal places.

Additionally, for very large cohorts, standardized mean differences calculated on categorical variables using the
\code{stddiff} package may throw \verb{In n[1] * n[2] : NAs produced by integer overflow}. This is an implementation issue
in \code{stddiff} in the calculation of the standard errors for the standardized mean differences. However, since these
standard errors are not used in the final output, they can be safely ignored.
}
\examples{
max_pairwise_smd(split(mtcars$disp, mtcars$am))

}
