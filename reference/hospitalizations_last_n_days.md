# Compute number of previous hospitalizations for an encounter in a given window.

Computes the number of hospitalizations an encounter has had over a
specified time period, using admission & discharge date-times from the
CIHI Discharge Abstract Database (DAD).

## Usage

``` r
hospitalizations_last_n_days(
  cohort,
  n_days = 30,
  admit_dt = "admission_date_time",
  discharge_dt = "discharge_date_time"
)
```

## Arguments

- cohort:

  (`data.table`, `data.frame`)  
  Cohort table with all relevant encounters of interest, where each row
  corresponds to a single encounter. Must contain the following columns:

  - `genc_id` (`integer`): GEMINI encounter ID

  - `patient_id_hashed` (`Varchar`): Unique hash of patient's health
    card number

  - `admission_date_time` (`character`): The date and time that the
    patient was officially registered as an inpatient for the current
    hospital admission. Must be in YYYY-MM-DD HH:MM.

  - `discharge_date_time` (`character`): The date and time when the
    patient was formally discharged from the current hospital admission.
    Must be in YYYY-MM-DD HH:MM.

- n_days:

  (`integer`)  
  Window for calculating the number of hospitalizations. n_days is set
  to 30 days by default.

- admit_dt:

  (`character`)  
  Admission date-time Variable representing admission date-time. Set to
  admission_date_time by default.

- discharge_dt:

  Discharge date-time Variable representing discharge date-time. Set to
  discharge_date_time by default.

## Value

(`data.table`)  
data.table containing the same number of rows as input `cohort` table.
This data.table contains the genc_id, along with `n_hospitalizations`,
which represents the number of previous hospitalizations in the given
window. The values in `n_hospitalizations` will be 0, 1, or 2+.

## Details

This function uses DAD fields Admission date-time (Group 04 Fields
01/02), Discharge date-time (Group 05 Fields 01/02) along with
patient_id_hashed to calculate the number of previous hospitalizations
an encounter has had in a specified window.

## Examples

``` r
# Default n_days of 30:
if (FALSE) { # \dontrun{
hospitalizations_last_n_days(cohort)
} # }

# User-input window of 182 days (6 months)
if (FALSE) { # \dontrun{
hospitalizations_last_n_days(cohort, n_days = 182)
} # }
```
