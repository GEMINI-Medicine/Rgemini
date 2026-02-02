# Compute CIHI Hospital Frailty Risk Score

`frailty_score` returns the number of frailty conditions for hospital
admissions based on the CIHI hospital frailty risk score (HFRS). By
setting `component_wise` to `TRUE`, function alternatively returns
identified frailty-related diagnosis codes and their corresponding
frailty conditions.

## Usage

``` r
frailty_score(cohort, ipdiag, erdiag, component_wise = FALSE)
```

## Arguments

- cohort:

  (`data.table`, `data.frame`)  
  Cohort table with encounters of interest and their corresponding age.
  Each row corresponds to a single encounter. Must contain GEMINI
  encounter ID (`genc_id`), and age of the encounter (`age`).

- ipdiag:

  (`data.table`, `data.frame`)  
  `ipdiagnosis` table as defined in the [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/). This
  table must contain the `genc_id` and `diagnosis_code` fields in long
  format. The diagnosis codes must be free from any punctuation or
  special characters.

- erdiag:

  (`data.table`, `data.frame`)  
  `erdiagnosis` table as defined in the [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/). This
  table must contain the `genc_id` and `er_diagnosis_code` fields in
  long format. The diagnosis codes must be free from punctuation or
  special characters.

- component_wise:

  (`logical`)  
  Default is FALSE. When TRUE, the function does not aggregate the score
  and instead outputs a table in long format that shows the frailty
  conditions contributed to the score for each encounter. Encounters
  with no frailty conditions found (frailty_score_derived=0) are
  excluded.

## Value

(`data.frame`)  
If `component_wise == FALSE`: `genc_id` (`numeric`),  
`frailty_score_derived` (`numeric`) total number of frailty conditions
for the encounter.

If `component_wise == TRUE`: `genc_id` (`numeric`),  
`diagnosis_code` (`character`),  
ICD-10-CA diagnosis codes associated with the encounter
`frailty_categories` (`character`)  
frailty category of the ICD-10-CA code mapped to

## Details

The CIHI HFRS is a contextual measure of frailty for patients aged 65
years and older. It categorizes a list of ICD-10-CA diagnosis codes into
36 distinct frailty-related conditions. The level of frailty is
determined as the cumulative number of distinct frailty conditions (an
equal-weight algorithm) present in an individual. ALL diagnoses are
included to the calculation (i.e. all emergency department (ER) and
in-patient (IP) diagnoses, all diagnosis types). When none of the
frailty conditions is present in an individual, a score of zero is
assigned.

The function closely adheres to the CIHI HFRS with the following
adaptations:

- No look-back period: Score is computed at encounter level. The 2-year
  look-back in the CIHI HFRS is not implemented. This adaptation
  systematically underestimates frailty but ensures comparable scores
  across time and hospitals considering variations in data availability

- Score format: Integer scores are returned representing the sum of the
  number of frailty conditions. These scores can be easily converted to
  the different formats (i.e. continuous fractions, 8 risk groups,
  binary) defined by CIHI HFRS. For example, dividing the returned score
  by 36 (maximum number of conditions possible) gives the continuous
  CIHI HFRS. Users interested in further categorizing the scores should
  refer to [Amuah et al, 2023](https://doi.org/10.1503/cmaj.220926).

## Warnings

A warning message is returned when the user-provided cohort includes
encounters not qualified for assessing frailty. Encounters below the age
cutoff for frailty assessment are excluded from the results. Encounters
with no diagnosis data are excluded from the results. The size of
exclusion is detailed in the warning message. `frailty_score_derived`
for these encounters are NAs, when joining the result table to the
original cohort.

A warning message is returned if `erdiag` is explicitly set to `NULL`.
It's possible to exclude diagnoses at the emergency department from the
consideration by explicitly specifying `erdiag=NULL`. However, excluding
any types of diagnoses is not recommended. The CIHI frailty score was
developed and validated based on all diagnoses present in medical
records (both NACRS and DAD). Excluding diagnoses in NACRS was found to
underestimate frailty levels (Amuah et al, 2023).

## Notes

The original development paper of CIHI HFRS maped 595 ICD-10-CA
diagnosis codes to frailty conditions. Three codes (Z96.62, U07.1 and
U07.2) were added to this mapping in the CIHI methodology notes. This
function uses the mapping of 598 ICD-10-CA diagnosis codes in the CIHI
methodology notes to identify frailty conditions.

The previous `frailty_score()` function calculated the UK HFRS (Gilbert,
2018), and it is now deprecated. Using a similar approach as the UK
HFRS, the CIHI HFRS was developed and validated based on Canadian
cohorts, making it particularly suited for GEMINI data. The UK version
remains available in `Rgemini` version 0.3.1 and earlier but will not
receive future maintenance. Users interested in the UK version should
refer to the original publications for important differences in
diagnostic coding practices and age threshold.

## References

We recommend referencing both original articles (UK HFRS and CIHI HFRS)
when using this function:

- UK HFRS: Gilbert T, et al. Lancet, 2018.
  http://dx.doi.org/10.1016/S0140-6736(18)30668-8

- CIHI HFRS: Amuah JE, et al. CMAJ, 2023.
  https://doi.org/10.1503/cmaj.220926

- [CIHI methodology
  notes](https://www.cihi.ca/sites/default/files/document/cihi-hospital-frailty-risk-measure-meth-notes-en.pdf)

## Examples

``` r
if (FALSE) { # \dontrun{
cohort_dum <- data.table(
  genc_id = c(1, 2, 3), age = c(64, 65, 80)
)
ipdiag_dum <- dummy_diag(
  nid = 3, nrow = 10,
  ipdiagnosis = TRUE,
  pattern = "C20$|R460$" # frailty conditions
)
erdiag_dum <- dummy_diag(
  nid = 3, nrow = 5,
  ipdiagnosis = FALSE,
  pattern = "M121$" # not a frailty condition
)
# calculate frailty score
frailty_score(cohort_dum, ipdiag_dum, erdiag_dum, component_wise = FALSE)
} # }
```
