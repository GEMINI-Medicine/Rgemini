# mLAPS

mLAPS

## Usage

``` r
mlaps(ipadmdad, lab, hours_after_admission = 0, component_wise = FALSE)
```

## Arguments

- ipadmdad:

  (`data.frame`)  
  Table equivalent to a subset of the `admdad` table defined in the
  [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/wp-content/uploads/2023/12/GEMINI-Data-Repository-Data-Dictionary-v3.0.2.html).

- lab:

  (`data.table`, `data.frame`)  
  Table equivalent to a subset of the `lab` table defined in the [GEMINI
  Data Repository
  Dictionary](https://geminimedicine.ca/wp-content/uploads/2023/12/GEMINI-Data-Repository-Data-Dictionary-v3.0.2.html).

- hours_after_admission:

  (`numeric`)  
  Consider lab tests collected **up to** `hours_after_admission` hours
  after inpatient admission in the calculation. Default
  `hours_after_admission` is set to 0, where only lab tests collected at
  Emergency Department (before inpatient admission) are considered in
  mLAPS calculation. Since not all encounters are admitted through
  Emergency Department, depending on research question, it can be
  relevant to consider lab tests collected in early inpatient admission.
  Typically, `hours_after_admission` can be set to 24 to consider any
  lab tests collected at Emergency Department and 24 hours after
  inpatient admission.

- component_wise:

  (`logical`)  
  Does not aggregate the score and instead outputs for each LAPS
  component (test) its contribution to the overall score.

## Value

(`data.frame`)  
If `componentwise == TRUE`: `genc_id` (`numeric`),  
`test_type_mapped_omop` (`character`),  
`mlaps` (`numeric`) max score for this test.

If `componentwise == FALSE`: `genc_id` (`numeric`),  
`mlaps` (`numeric`) sum of max scores for each relevant test for this
encounter.

## Details

Modified Laboratory based Acute Physiology Score (mLAPS) uses 12 lab
test values from 11 unique lab tests. In this modified version, troponin
tests are not considered in the mLAPS calculation.

## Note

When an encounter has multiple recorded lab results for a given
"LAPS-relevant" test, the test result which results in the *worst*
possible LAPS contribution is taken for a conservative estimate.

Patients without entries in the lab table within `hours_of_admission`
are not returned. For those encounters which were not returned, it may
be reasonable to impute their LAPS score with zero if lab data was in
principle available for their site and time period. If lab data was
unavailable, it might be more accurate to assign the LAPS score for
these encounters as `NA`. In general it is recommended to take care and
be intentional when imputing LAPS scores.

## References

When the function is used, please cite the following:

- Escobar G, et al. Med Care, 2008.
  https://doi.org/10.1097/MLR.0b013e3181589bb6

- Roberts SB, et al. J Gen Intern Med, 2023.
  https://doi.org/10.1007/s11606-023-08245-w

- Roberts SB, et al. medRxiv (preprint), 2023.
  https://doi.org/10.1101/2023.01.06.23284273
