# Identify episodes of care

The `episodes_of_care()` function groups encounters that are linked via
transfers into a single episode of care. The derived episodes of care
are used in the
[`readmission()`](https://gemini-medicine.github.io/Rgemini/reference/readmission.md)
function to avoid counting transfers as readmissions. Therefore, the
criteria for transfers used in this function are based on [CIHI
guidelines for readmission
calculations](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital).
However, the `episodes_of_care` function can also be used independently
to compute episodes of care outside of readmission.

Transfer information is obtained from the GEMINI database table
`lookup_transfer` (see [GEMINI database
schema](https://geminimedicine.ca/the-gemini-database/)). By default,
the function queries all encounters in the `admdad` table to ensure that
any encounters that may be linked via transfers are accurately grouped
into the same episode of care.

Note: Users do not typically need to calculate episodes of care
themselves, but rather should use the derived `epicares` variable in the
GEMINI database, if available. This is due to the fact that the derived
variable is calculated based on all available GEMINI encounters, and
therefore, provides the most accurate identification of episodes of
care. By contrast, datacuts or cohorts that have been pre-filtered may
miss certain encounters, resulting in a loss of information. However, in
certain situations, users may still find this function useful 1) to
understand how the derived variable is calculated and 2) in case users
need to re-calculate episodes of care or readmission rates for a
restricted cohort (see below).

## Usage

``` r
episodes_of_care(dbcon, restricted_cohort = NULL)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database.

- restricted_cohort:

  (`data.table` \| `data.frame`)  
  User specified cohort that is a restricted subset of all encounters in
  DRM table "ipadmdad" (see [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/)). Must
  contain `genc_id` as the identifier. Default is `Null`, which loads
  the entire "ipadmdad" table in the user-provided database (recommended
  approach).

## Value

(`data.frame`)  
This function returns a `data.table` excluding records with invalid
`patient_id_hashed` or `admit_category` of cadaveric donors in line with
CIHI guidelines. Nine columns are returned: `genc_id`,
`patient_id_hashed`, `time_to_next_admission` (`numeric`),
`time_since_last_admission` (`numeric`), `AT_in_coded` (`TRUE`/`FALSE`),
`AT_out_coded` (`TRUE`/`FALSE`), `AT_in_occurred` (`TRUE`/`FALSE`/`NA`),
`AT_out_occurred` (`TRUE`/`FALSE`/`NA`), `epicare` (`numeric`)

## Details

An episode of care refers to all contiguous inpatient hospitalizations
admitted to any medical or intensive care service within GEMINI.
Episodes involving inter-facility transfers are linked regardless of
diagnosis. An acute care transfer is assumed to have occurred if either
of the following criteria are met (see [CIHI
guidelines](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital):

- An admission occurs within 7 hours after discharge, regardless of
  whether the transfer is coded by hospitals. OR

- An admission occurs within 7-12 hours after discharge, and at least
  one hospital has coded the transfer.

Acute transfers that are coded by hospitals (`AT_in_coded` and
`AT_out_coded`) are based on GEMINI-derived mappings of institution
types that can be found in the `lookup_transfer` table. The mapped
institution types are derived from the raw institution codes in `admdad`
(DAD fields `institution_from` and `institution_to`):
`AT_in_coded`/`AT_out_coded` is `TRUE`, when the mapped institution
from/to type is "AT". All remaining entries are set to `FALSE`.

Acute transfers that actually occurred (`AT_in_occurred` and
`AT_out_occurred`) are defined as follows:
`AT_in_occurred`/`AT_out_occurred` is `TRUE` when admission is within 7
hrs of discharge regardless of transfer coding, or, admission is within
7-12hrs of discharge and at least one hospital coded the transfer.
`AT_in_occurred`/`AT_out_occurred` is `NA` when the transfer was coded
but admission time since previous discharge is unknown. This is because
it cannot be determined if the transfer actually took place or not.
`AT_in_occurred`/`AT_out_occurred` is `FALSE`, for all remaining
entries.

Each episode of care (`epicare`) is defined by linked transfers
identified based on `AT_in_occurred`/ `AT_out_occurred`. A unique
numeric ID is assigned to each unique episode of care. That is,
contiguous encounters linked by transfers will have the same `epicare`
number.

## Note

We recommend that users run this function on the entire GEMINI data set
(the default setting) to ensure no transfers to/from are omitted. Please
review the function documentation & vignette carefully. A comprehensive
understanding of the following concepts is needed for proper use of the
function: transfer coded vs. transfer occurred, episode of care
definition.

## References

[CIHI readmission
guidelines](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital)

## See also

[`vignette("epicare_and_readmission", package = "Rgemini")`](https://gemini-medicine.github.io/Rgemini/articles/epicare_and_readmission.md)

## Examples

``` r
if (FALSE) { # \dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)

epicare_table <- episodes_of_care(dbcon)
} # }
```
