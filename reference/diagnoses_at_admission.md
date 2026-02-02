# Determine Diagnoses at Admission

See CIHI's [HSMR
Methodology](https://www.cihi.ca/sites/default/files/document/hospital-standardized-mortality-ratio-meth-notes-en.pdf).

Specifically page 19 for inclusion/exclusion criteria.

## Usage

``` r
diagnoses_at_admission(ipdiag, erdiag)
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

## Value

(`data.table`)  
All encounters by `genc_id` with `diagnosis_type` and `diagnosis_code`
for all diagnoses at admission.
