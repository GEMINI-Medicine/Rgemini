% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/er_los.R
\name{er_los}
\alias{er_los}
\title{Compute Length of Stay in Emergency Room}
\usage{
er_los(cohort, er)
}
\arguments{
\item{cohort}{(\code{data.table} or \code{data.frame})\cr
Table with all relevant encounters of interest, where each row corresponds to
a single encounter. Must contain GEMINI Encounter ID (\code{genc_id}).}

\item{er}{(\code{data.table} or \code{data.frame})\cr
Table equivalent to the \code{er} table defined in the
\href{https://geminimedicine.ca/the-gemini-database/}{GEMINI Data Repository Dictionary}.
Table must contain fields:
GEMINI Encounter ID (\code{genc_id}),
ER triage date-time (\code{triage_date_time} in "yyyy-mm-dd hh:mm" format),
Left ER date-time (\code{left_er_date_time} in "yyyy-mm-dd hh:mm" format).}
}
\value{
(\code{data.table})\cr
By default, for each encounter in input \code{cohort} returns the corresponding
derived numeric fields \code{er_los_hrs_derived} and \code{er_los_days_derived}.
}
\description{
Calculate the total duration in Emergency Room (ER) that an encounter spent
through a hospitalization using CIHI National Ambulatory Care Reporting System
(NACRS) fields
}
\details{
This function calculates the length of stay (LoS) in hours and days that an
encounter spent in the ER during a hospital stay.

It uses NACRS fields Triage Date and Time (Data Element Number 24/25) and
Date and Time Patient Left Emergency Department (Data Element Number 116/117).
}
\note{
Encounter IDs in the \code{cohort} table that are not present in the \code{er} table
are assumed to have no ER visit and are returned with \code{er_los = 0}. Please
check if any of these entries might be due to data availability issues and
consider removing them from your analyses.
Encounter IDs in the \code{er} table that have missing/invalid \code{triage_date_time}
or \code{left_er_date_time} will be returned with \code{er_los = NA}.
}
\examples{
# Compute ER LoS for all encounters in ipadmdad;
\dontrun{
er_los(cohort = ipadmdad, er = er)
}

}
