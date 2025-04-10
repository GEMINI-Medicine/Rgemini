% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlaps.R
\name{loop_mlaps}
\alias{loop_mlaps}
\title{Loop mLAPS}
\usage{
loop_mlaps(
  db,
  cohort = NULL,
  hours_after_admission = 0,
  component_wise = FALSE
)
}
\arguments{
\item{db}{(\code{DBIConnection})\cr
RPostgres DB connection.}

\item{cohort}{(\code{data.frame} or \code{data.table})\cr
Containing a column of \code{genc_ids} to restrict the calculation to.}

\item{hours_after_admission}{(\code{numeric})\cr
Consider lab tests collected \strong{up to} \code{hours_after_admission} hours after inpatient admission in the calculation.
Default \code{hours_after_admission} is set to 0, where only lab tests collected at Emergency Department (before inpatient admission) are considered in mLAPS calculation.
Since not all encounters are admitted through Emergency Department, depending on research question, it can be relevant to consider lab tests collected in early inpatient admission.
Typically, \code{hours_after_admission} can be set to 24 to consider any lab tests collected at Emergency Department and 24 hours after inpatient admission.}

\item{component_wise}{(\code{logical})\cr
Does not aggregate the score and instead outputs for each LAPS component (test) its contribution
to the overall score.}
}
\value{
(\code{data.frame})\cr
If \code{output_laps_components == TRUE}:
\code{genc_id} (\code{numeric}),\cr
\code{test_type_mapped_omop} (\code{character}),\cr
\code{mlaps} (\code{numeric}) max score for this test.

If \code{output_laps_components == FALSE}:
\code{genc_id} (\code{numeric}),\cr
\code{mlaps} (\code{numeric}) sum of max scores for each relevant test for this encounter.
}
\description{
A wrapper around the \code{mlaps()} function which breaks down the calculation of mLAPS on a
large number of encounters by hospital-year. This avoids memory issues that can be caused
by loading large chunks of the lab table.
}
\examples{
\dontrun{
drv <- DBI::dbDriver("PostgreSQL")
db <- DBI::dbConnect(
  drv,
  dbname = "db_name",
  host = "domain_name.ca",
  port = 1234,
  user = getPass::getPass("Enter Username"),
  password = getPass::getPass("Enter Password")
)

cohort <- DBI::dbGetQuery(db, "SELECT genc_id FROM public.admdad LIMIT 200;")

laps <- loop_laps(db, cohort = cohort)
}

}
\references{
When the function is used, please cite the following:
https://doi.org/10.1097/MLR.0b013e3181589bb6
https://doi.org/10.1007/s11606-023-08245-w
https://doi.org/10.1101/2023.01.06.23284273
}
