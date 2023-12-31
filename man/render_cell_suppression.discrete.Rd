% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cell_suppression.R
\name{render_cell_suppression.discrete}
\alias{render_cell_suppression.discrete}
\title{Render Cell Suppression (Discrete)}
\usage{
render_cell_suppression.discrete(x)
}
\arguments{
\item{x}{(\code{character} or \code{factor})\cr
A discrete variable to summarize.}
}
\value{
named (\code{character})\cr
Concatenated with \code{""} to shift values down one row for proper alignment.
}
\description{
This is a custom render for \code{table1} discrete variables which performs GEMINI
"cell suppression" for any variable levels which contain fewer than 6 observations.

This is useful when you have an indicator variable for example, and you would like to
count the total number of events. \verb{[table1::render.default.categorical()]} will break
down the indicator variable into its components first (0 and 1) and then give you individual
counts. This will simply count 1s (for example).
}
\examples{
x <- 1:6
render_cell_suppression.discrete(x)

y <- 1:2
render_cell_suppression.discrete(y)

}
