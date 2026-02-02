# Compute readmission and episodes of care.

`readmission.R` computes whether or not a patient associated with an
episode of care was readmitted to a GEMINI hospital within a time window
of interest. The default readmission time window is 7 or 30 days.

In line with CIHI guidelines, readmission calculations are based on
episodes of care instead of individual encounters in order to avoid that
linked transfers are counted as readmissions (also see documentation for
[`episodes_of_care()`](https://gemini-medicine.github.io/Rgemini/reference/episodes_of_care.md)).
Specifically, an episode of care refers to all contiguous inpatient
hospitalizations to any medical or intensive care service within the
GEMINI network. Episodes involving inter-facility transfers are linked
regardless of diagnosis. An acute care transfer is assumed to have
occurred if either of the following criteria are met:

- An admission to a GEMINI hospital within 7 hours after discharge from
  another GEMINI hospital, regardless of whether the transfer is coded

- An admission to a GEMINI hospital within 7-12 hours after discharge
  from another GEMINI hospital, and at least 1 hospital has coded the
  transfer: Coded transfers are based on the DAD Institution From and
  Institution To fields

For episodes of care involving acute care transfers, readmissions are
attributed to the last hospital from which the patient was discharged
before readmission.

## Usage

``` r
readmission(
  dbcon,
  elective_admit = TRUE,
  death = TRUE,
  MAID = FALSE,
  palliative = FALSE,
  chemo = FALSE,
  mental = FALSE,
  obstetric = FALSE,
  signout = FALSE,
  return_readmit_enc = FALSE,
  restricted_cohort = NULL,
  readm_win = c(7, 30)
)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database.

- elective_admit:

  (`logical`)  
  If `TRUE` (default), elective episodes of care are not considered to
  be true readmissions, and are therefore removed from the numerator.
  Specifically, if the first encounter of the (n)th episode of care is
  elective (`admit_category = 'L'`), the (n-1)th episode of care is not
  considered to be followed by a readmission (`readmit(n-1) = FALSE`).

- death:

  (`logical`)  
  If `TRUE` (default), episodes of care ending in in-hospital death are
  removed from the denominator. Specifically, if the (n)th episode of
  care ended in death, it is impossible for a patient to be readmitted,
  and therefore the episode of care is not considered in the readmission
  calculation (`readmit(n) = NA`).

- MAID:

  (`logical`)  
  If `TRUE` (default): Episodes of care that involve MAID are not
  considered to be true readmissions, and are therefore removed from the
  numerator. Specifically, if any encounter of the (n)th episode of care
  involves MAID, the (n-1)th episode of care is not considered to be
  followed by a readmission (`readmit(n-1) = FALSE`). Note that episodes
  of care involving MAID end in death, however, they are NOT removed
  from the denominator unless the `death` flag is set to `TRUE` as well
  (recommended approach).

- palliative:

  (`logical`)  
  If `TRUE` (default): Episodes of care involving palliative care are
  excluded from both the numerator and denominator. Specifically,
  episode of care with palliative care as a type-M diagnosis are not
  considered to be true readmissions (`readmit(n-1) = FALSE`).
  Additionally, similarly to death, palliative episodes of care cannot
  be followed by future readmission and are therefore excluded from the
  denominator (`readmit(n) = NA`)

- chemo:

  (`logical`)  
  If `TRUE` (default): Episodes of care that involve chemotherapy are
  not considered to be true readmissions, and are therefore removed from
  the numerator. Specifically, if any encounter of the (n)th episode of
  care involves chemotherapy, the (n-1)th episode of care is not
  considered to be followed by a readmission (`readmit(n-1) = FALSE`).

- mental:

  (`logical`)  
  If `TRUE` (default): Episodes of care for mental health are excluded
  from both the numerator and denominator. Specifically, episodes of
  care that fall under CIHI's Major Clinical Category (MCC) 17 are not
  considered to be true readmissions and are therefore removed from the
  numerator (`readmit(n-1) = FALSE`). Additionally, they are not
  considered to be acute care and are therefore not included in the
  readmission denominator (`readmit(n) = NA`)

- obstetric:

  (`logical`)  
  If `TRUE` (default): Episodes of care that involve obstetric delivery
  are not considered to be true readmissions, and are therefore removed
  from the numerator. Specifically, if any encounter of the (n)th
  episode of care involves obstetric delivery, the (n-1)th episode of
  care is not considered to be followed by a readmission
  (`readmit(n-1) = FALSE`).

- signout:

  (`logical`)  
  If `TRUE` (default), episodes of care where the last encounter was
  self sign-out/left against medical advice (LAMA) are removed from the
  denominator. Specifically, if the (n)th episode of care ended in
  self-signout/LAMA, it is not considered in the readmission calculation
  (`readmit(n) = NA`).

- return_readmit_enc:

  (`logical`)  
  If `TRUE`: The function will additionally return a column for each
  readmission window the user provides, containing the `genc_id`
  corresponding to the readmission encounter (i.e., encounter following
  the index encounter within n days). This can be used to analyze
  characteristics of the readmission encounter (e.g., time of
  readmission, diagnoses/clinical outcomes at the time of readmission
  etc.). For index encounters where the readmission flag is
  `FALSE`/`NA`, `readmit(n)_genc_id` will be returned as `NA`.

- restricted_cohort:

  (`data.frame` or `data.table`)  
  User specified cohort that is a restricted subset of all encounters in
  DRM table "ipadmdad" (see [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/)). Must
  contain `genc_id` as the identifier. Default is `Null`, which loads
  the entire "ipadmdad" table in the user-provided database (recommended
  approach).

- readm_win:

  (`integer` \| `vector`)  
  readmission window(s) of interest, in days (by default:
  `readm_win = c(7,30)` to calculate 7- & 30-day readmission)

## Value

This function returns a `data.table` containing `genc_id`,
`AT_in_occurred`, `AT_out_occurred`, `epicare` (see
[`episodes_of_care()`](https://gemini-medicine.github.io/Rgemini/reference/episodes_of_care.md)),
`readmit7` (default), and `readmit30` (default). `readmit7` and
`readmit30` are logical variables indicating whether or not an episode
of care was followed by a readmission. Note that readmissions are
attributed to the last encounter of each episode of care. Episodes of
care where the last encounter has `readmitX = NA` have been removed from
the denominator.

Note: If the very last encounter of an episode of care has a
transfer-out coding (but the encounter associated with that transfer
does not exist in the GEMINI database), the episode of care is excluded
from the readmission denominator (`readmit(n) = NA`). This is to avoid
inaccurate attribution of a potential readmission to the hospital
associated with the intermediate (i.e., not the last) encounter of that
episode of care.

## Details

This function calls the
[`episodes_of_care()`](https://gemini-medicine.github.io/Rgemini/reference/episodes_of_care.md)
function to link encounters with acute care transfers into episodes of
care. When conducting readmission calculations using the function
output, the number of encounters where `readmission == TRUE` is the
numerator, and the number of eligible episodes of care (where
`readmission != NA`) is the denominator. For example, 7-day readmission
rate can be calculated by `mean(readmit7, na.rm=TRUE)`, where `readmit7`
is the variable returned by the `readmission()` function. This function
is designed to be consistent with [CIHI guidelines for readmission
calculations](https://www.cihi.ca/en/indicators/all-patients-readmitted-to-hospital).
Please carefully review this reference before applying the function for
research purposes. Users may choose to deviate from the CIHI definition
of readmission by setting any of the CIHI flags (see above) to `FALSE`.

## Warning

Warnings are produced if

1.  either the `death` or `elective_admit` flags are set to `FALSE`,

2.  the input contains a user-specified restricted cohort, or

3.  more than 25% of episodes of care are removed from the denominator
    due to the CIHI flags or buffer period.

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

## default readmission calculation (with elective = TRUE & death = TRUE by default)
readmission(dbcon)

## Readmission calculation following CIHI definition
readmission(dbcon,
  elective_admit = TRUE, death = TRUE, MAID = TRUE, palliative = TRUE,
  chemo = TRUE, mental = TRUE, obstetric = TRUE, signout = TRUE
)
} # }
```
