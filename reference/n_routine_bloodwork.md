# Compute the number of routine bloodwork tests per encounter

`n_routine_bloodwork` returns the number of routine bloodwork (Sodium
and Hemoglobin tests) for each hospital admission. Sodium and Hemoglobin
tests are defined by OMOP codes. Sodium is the code 3019550. Hemoglobin
is 3000963.

## Usage

``` r
n_routine_bloodwork(dbcon, cohort, exclude_ed = FALSE)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A `DBI` database connection to any GEMINI database.

- cohort:

  (`data.frame` or `data.table`) Cohort table with all relevant
  encounters of interest, where each row corresponds to a single
  encounter. Must contain GEMINI Encounter ID (`genc_id`).

- exclude_ed:

  (`logical`) Whether to exclude tests in emergency department. When set
  to `TRUE`, tests performed in ED settings (testing time before
  admission time) will not be counted. When set to `FALSE`, tests will
  not be filtered by time and all tests in lab table will be counted.
  Please be aware that this may include tests before triage time, tests
  after discharge time, and tests without testing time. Tests in ED are
  defined as `collection_date_time` earlier than `admission_date_time`.
  Tests with missing `collection_date_time` will be excluded when
  `exclude_ed` is set to `TRUE`.

## Value

data.table object with the same number of rows as input "cohort", with
additional derived numeric field labelled as
"n_routine_bloodwork_derived"

## Details

This function takes a list of admissions and a GEMINI database
connection to generate numeric fields counting the number of Sodium and
Hemoglobin tests for each admission.

Lab table in the database should include field that classifies each test
into a few different categories. Therefore, this function should be run
after some standardization efforts on lab table (performed by GEMINI).
Currently, the number of routine bloodwork tests is one of the
performance metrics in
[MyPracticeReport](https://www.hqontario.ca/Quality-Improvement/Practice-Reports/MyPractice-General-Medicine).

Note that this function removes tests without a valid numeric result
value. This is to ensure that any non-performed/cancelled tests are not
included in the counts since non-performed tests are not part of
GEMINI's data reference model.

## Warning

Function returns data.table with id field `genc_id` and one numeric
field indicating the number of bloodwork tests per encounter. By design,
function will not return any NA values. If a `genc_id` does not have any
entries in the "lab" table, the admission gets assigned 0 number of
tests. User should check lab data coverage and decide whether the
imputed `0`s are appropriate or not. When one tries to left-join the
output of this function with another table (another list of admissions
in the left), make sure list of admissions (or patient) aligns in both
tables.

This function requires mappings by a Subject Matter Expert to ensure
that all tests are mapped to 3019550 - Sodium (Moles/volume) in Serum or
Plasma, and 3000963 - Hemoglobin (Mass/volume) in Blood
