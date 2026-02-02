# Compute Elixhauser comorbidity score

Based on the methodology from [Elixhauser A et al.
1998](https://www.jstor.org/stable/3766985), [Quan H et al.
2005](https://www.jstor.org/stable/3768193), and [van Walraven C et al.
2009](https://doi.org/10.1097/MLR.0b013e31819432e5) using ICD-10 codes
as the mapping algorithm, implemented in the R
[comorbidity](https://ellessenne.github.io/comorbidity/index.html)
package by [Gasparini, 2018](https://doi.org/10.21105/joss.00648).

Can compute either Elixhauser score at admission based on [HSMR
Methodology](https://www.cihi.ca/sites/default/files/document/hospital-standardized-mortality-ratio-meth-notes-en.pdf)
or for the entire patient encounter. The default is to compute
comorbidity at admission.

## Usage

``` r
elixhauser_comorbidity_index(
  ipdiag,
  erdiag,
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

## References

- Elixhauser A, et al. Med Care, 1998.
  https://www.jstor.org/stable/3766985

- Quen H, et al. Med Care, 2005. https://www.jstor.org/stable/3768193

- Van Walraven C, et al. Med Care, 2009.
  https://doi.org/10.1097/MLR.0b013e31819432e5

- Gasparini A. JOSS, 2018. https://doi.org/10.21105/joss.00648
