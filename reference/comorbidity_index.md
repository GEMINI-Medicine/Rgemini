# Compute Comorbidity Index

This is a generic function that is wrapped by
[`charlson_comorbidity_index()`](https://gemini-medicine.github.io/Rgemini/reference/charlson_comorbidity_index.md)
and
[`elixhauser_comorbidity_index()`](https://gemini-medicine.github.io/Rgemini/reference/elixhauser_comorbidity_index.md)
and is not meant to be used directly. The function is an interface to
[`comorbidity::comorbidity()`](https://ellessenne.github.io/comorbidity/reference/comorbidity.html)
for GEMINI data, providing options for outputting raw comorbidities or
comorbidities at admission.

## Usage

``` r
comorbidity_index(
  ipdiag,
  erdiag,
  map,
  weights,
  at_admission = TRUE,
  raw_comorbidities = FALSE
)
```

## Arguments

- ipdiag:

  (`data.table` or `data.frame`)  
  `ipdiagnosis` table as defined in the [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/).

- erdiag:

  (`data.table` or `data.frame`)  
  `erdiagnosis` table as defined in the [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/).
  Typically, ER diagnoses should be included when deriving comorbidity
  in order to increase sensitivity. However, in certain scenarios, users
  may choose to only include IP diagnoses by specifying `erdiag = NULL`.
  This may be useful when comparing cohorts with different rates of ER
  admissions.

- map:

  String denoting the mapping algorithm to be used (values are
  case-insensitive). Possible values are the Charlson score with either
  ICD-10 or ICD-9-CM codes (`charlson_icd10_quan`, `charlson_icd9_quan`)
  and the Elixhauser score, again using either ICD-10 or ICD-9-CM
  (`elixhauser_icd10_quan`, `elixhauser_icd9_quan`). These mapping are
  based on the paper by Quan et al. (2011). It is also possible to
  obtain a Swedish (`charlson_icd10_se`) or Australian
  (`charlson_icd10_am`) modification of the Charlson score using ICD-10
  codes.

- weights:

  A string denoting the weighting system to be used, which will depend
  on the mapping algorithm.

  Possible values for the Charlson index are:

  - `charlson`, for the original weights by Charlson et al. (1987);

  - `quan`, for the revised weights by Quan et al. (2011).

  Possible values for the Elixhauser score are:

  - `vw`, for the weights by van Walraven et al. (2009);

  - `swiss`, for the Swiss Elixhauser weights by Sharma et al. (2021).

  Defaults to `NULL`, in which case an unweighted score will be used.

- at_admission:

  (`logical`)  
  Whether to calculate the score for all comorbidities or for only
  pre-admit comorbidities.

- raw_comorbidities:

  (`logical`)  
  Whether to output a `data.table` of raw comorbidities as opposed to
  pre-computed scores.

## Value

(`data.table`)  
By default, for each encounter, outputs the comorbidity score. If
`raw_comorbidities` is `TRUE`, outputs a wide `data.table` with a column
for each comorbidity for each encounter.

## Details

A hierarchy of comorbidities is used when calculating the cormorbidity
score, but not when outputting `raw_comorbidites`. This affects
comorbidities present in a patient with different degrees of severity.
See documentation for
[`comorbidity::comorbidity()`](https://ellessenne.github.io/comorbidity/reference/comorbidity.html)
for details.
