# Compute Charlson comorbidity index (CCI) score

Based on the methodology from [Charlson et al.
1988](https://doi.org/10.1016/0021-9681(87)90171-8), [Quan H et al.
2005](https://www.jstor.org/stable/3768193), and [Quan et al.
2011](https://doi.org/10.1093/aje/kwq433) using ICD-10 codes as the
mapping algorithm, implemented in the R
[comorbidity](https://ellessenne.github.io/comorbidity/index.html)
package by [Gasparini, 2018](https://doi.org/10.21105/joss.00648).

Can compute either CCI score at admission based on [HSMR
Methodology](https://www.cihi.ca/sites/default/files/document/hospital-standardized-mortality-ratio-meth-notes-en.pdf)
or for the entire patient encounter. The default is to compute
comorbidity at admission.

## Usage

``` r
charlson_comorbidity_index(
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

- Charlson M, et al. Journal of Chronic Diseases, 1988.
  https://doi.org/10.1016/0021-9681(87)90171-8

- Quen H, et al. Med Care, 2005. https://www.jstor.org/stable/3768193

- Quan H, et al. Am J Epidemiol, 2011.
  https://doi.org/10.1093/aje/kwq433

- Gasparini A. JOSS, 2018. https://doi.org/10.21105/joss.00648
