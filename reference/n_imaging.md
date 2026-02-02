# Compute the number of radiology tests per encounter

`n_imaging` returns the number of radiology tests for hospital
admission.

## Usage

``` r
n_imaging(dbcon, cohort, exclude_ed = FALSE)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database. Only `DBI` connection is
  accepted as `odbc` connection may cause connection issues in certain
  environment.

- cohort:

  (`data.frame` or `data.table`) Cohort table with all relevant
  encounters of interest, where each row corresponds to a single
  encounter. Must contain GEMINI Encounter ID (`genc_id`).

- exclude_ed:

  (`logical`) Whether to exclude tests in emergency department. When set
  to `TRUE`, tests performed in ED (testing time before admission time)
  will not be counted. When set to `FALSE`, tests will not be filtered
  by time and all tests in radiology table will be counted. Please be
  aware that this may include tests before triage time, tests after
  discharge time, and tests without testing time.

  Tests in ED are defined as `ordered_date_time` earlier than
  `admission_date_time`. When `ordered_date_time` is not available,
  `performed_date_time` is used instead.

## Value

data.table with the same number of rows as input `cohort`, with
additional derived numeric fields labelled as "n_img_xray_derived",
"n_img_ct_derived", "n_img_mri_derived", "n_img_us_derived",
"n_img_other_derived", "n_img_int_derived" and
"n_img_ct_mri_us_derived".

## Details

This function takes a list of admissions and a GEMINI database
connection to generate numeric fields counting the number of different
radiology tests for each admission.

Imaging table in the database should include field that classifies each
test into below 7 categories.

- 1: X-Ray

- 2: CT

- 3: MRI

- 4: Ultrasound

- 5: Other

- 6: Interventional radiology procedures

- 7: Echo (Excluded)

Therefore, this function should be run after some standardization
efforts on imaging table (performed by GEMINI team). Currently, the
number of advanced radiology tests is one of the performance metrics in
[MyPracticeReport](https://www.hqontario.ca/Quality-Improvement/Practice-Reports/MyPractice-General-Medicine).
CT, MRI and Ultrasound are considered advance imaging tests, which can
be retrieved by the derived variable (`n_img_ct_mri_us_derived`).

## Note

Currently, the function does not take radiology data coverage into
account. For patients without imaging tests, the function will return 0
in result columns. User should check radiology data coverage and decide
whether the imputed `0`s are appropriate or not.

## Warning

Function returns `data.table` with id field and several numeric fields.
By design, function will not return any NA values. When one tries to
left-join the output of this function with another table (another list
of admissions in the left), make sure list of admissions (or patient)
aligns in both tables.
