% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/icu_entry.R
\name{icu_entry}
\alias{icu_entry}
\title{Compute entry to Intensive Care Unit}
\usage{
icu_entry(cohort, ipscu, window = c(24, 48, 72))
}
\arguments{
\item{cohort}{(\code{data.table}, \code{data.frame})\cr
Cohort table with all relevant encounters of interest, where each row corresponds to
a single encounter. Must contain the following columns:
\itemize{
\item \code{genc_id} (\code{integer}): GEMINI encounter ID
\item \code{admission_date_time} (\code{character}): Date-time of admission in YYYY-MM-DD HH:MM format
}}

\item{ipscu}{(\code{data.table}, \code{data.frame})\cr
Table equivalent to the \code{ipscu} table defined in the
\href{https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5}{GEMINI Data Repository Dictionary}.
Table must contain fields:
GEMINI Encounter ID (\code{genc_id}), SCU admission time (\code{scu_admit_date_time}), and SCU number (\code{scu_unit_number}).}

\item{window}{(\code{integer}, \code{vector})\cr
Time window of ICU entry since hospital admission, in hours.
By default, \code{window = c(24, 48, 72)} to calculate ICU entry within 24, 48 or 72 hours since hospital admission.}
}
\value{
(\code{data.table})\cr
By default, for each encounter in input "cohort" returns the corresponding derived boolean (TRUE/FALSE) fields
"icu_entry_derived", "icu_entry_in_24hr_derived", "icu_entry_in_48hr_derived" and "icu_entry_in_72hr_derived".
If user specified time window x hour is used, field "icu_entry_in_xhr_derived" is computed in addition to "icu_entry_derived".
}
\description{
Determines whether an encounter has entered Intensive Care Unit (ICU) during
hospital stay using CIHI Discharge Abstract Database (DAD) fields.
}
\details{
This function uses DAD fields Admission date-time (Group 04 Fields 01/02),
and SCU Admit date-time (Group 13, Field 03/04) to derive boolean fields
indicating ICU entries at any time during hospital stay,
and within specified time window since hospital admission.

By definition in DAD (Group 13, Field 02), SCUs include ICUs and Step-Down Units.
Step-Down Units are not considered as ICUs.

Therefore, this function excludes below CIHI defined Step-Down Units numbers
from calculation:
\itemize{
\item{90 : }{Step-Down Medical Unit}
\item{93 : }{Combined Medical/Surgical Step-Down Unit}
\item{95 : }{Step-Down Surgical Unit}
\item{99 : }{No SCU}
}

Please refer to the CIHI DAD abstracting manual for more details.
}
\note{
:
By design, function will not return any NA values.
When one tries to left-join the output of this function to another table,
make sure the list of encounters aligns in both tables
}
\examples{
# Default time window 24, 48, 72 hours:
\dontrun{
icu_entry (cohort, ipscu)
}

# User specified time window:
\dontrun{
icu_entry (cohort, ipscu, window=12)
}

}
