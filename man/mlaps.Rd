% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlaps.R
\name{mlaps}
\alias{mlaps}
\title{mLAPS}
\usage{
mlaps(ipadmdad, lab, hours_after_admission = 0, component_wise = FALSE)
}
\arguments{
\item{ipadmdad}{(\code{data.frame})\cr
Table equivalent to a subset of the \code{admdad} table defined in the
\href{https://geminimedicine.ca/wp-content/uploads/2023/12/GEMINI-Data-Repository-Data-Dictionary-v3.0.2.html}{GEMINI Data Repository Dictionary}.}

\item{lab}{(\code{data.table}, \code{data.frame})\cr
Table equivalent to a subset of the \code{lab} table defined in the
\href{https://geminimedicine.ca/wp-content/uploads/2023/12/GEMINI-Data-Repository-Data-Dictionary-v3.0.2.html}{GEMINI Data Repository Dictionary}.}

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
If \code{componentwise == TRUE}:
\code{genc_id} (\code{numeric}),\cr
\code{test_type_mapped_omop} (\code{character}),\cr
\code{mlaps} (\code{numeric}) max score for this test.

If \code{componentwise == FALSE}:
\code{genc_id} (\code{numeric}),\cr
\code{mlaps} (\code{numeric}) sum of max scores for each relevant test for this encounter.
}
\description{
mLAPS
}
\details{
Modified Laboratory based Acute Physiology Score (mLAPS) uses 12 lab test values from 11 unique lab tests.
In this modified version, troponin tests are not considered in the mLAPS calculation.
}
\note{
When an encounter has multiple recorded lab results for a given "LAPS-relevant" test, the test result which results in the
\emph{worst} possible LAPS contribution is taken for a conservative estimate.

Patients without entries in the lab table within \code{hours_of_admission} are not returned.
For those encounters which were not returned, it may be reasonable to impute their LAPS score with zero
if lab data was in principle available for their site and time period.
If lab data was unavailable, it might be more accurate to assign the LAPS score for these encounters as \code{NA}.
In general it is recommended to take care and be intentional when imputing LAPS scores.
}
\references{
When the function is used, please cite the following:
\itemize{
\item{Escobar G, et al. Med Care, 2008. https://doi.org/10.1097/MLR.0b013e3181589bb6}
\item{Roberts SB, et al. J Gen Intern Med, 2023. https://doi.org/10.1007/s11606-023-08245-w}
\item{Roberts SB, et al. medRxiv (preprint), 2023. https://doi.org/10.1101/2023.01.06.23284273}
}
}
