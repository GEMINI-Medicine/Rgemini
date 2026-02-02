# Count the number Red Blood Cell (RBC) Transfusions per encounter

This function calculates the number of appropriate transfusions and
total of all transfusions per encounter. The definition of an
appropriate transfusion can be found on the sample [My Practice
Report](https://www.hqontario.ca/Portals/0/documents/qi/practice-reports/general-medicine-sample-report.html#transfusions-qi).
A transfusion is defined by procedure in the below codes:

1.  [4144461](https://athena.ohdsi.org/search-terms/terms/4144461).

2.  [4137859](https://athena.ohdsi.org/search-terms/terms/4137859)

3.  [4022173](https://athena.ohdsi.org/search-terms/terms/4022173)

An appropriate transfusion also requires that the patients
pre-transfusion hemoglobin levels are below 80 g/L. The Athena
definition of a hemoglobin test:
[3000963](https://athena.ohdsi.org/search-terms/terms/3000963)

Red blood cell transfusions with no hemoglobin measurement within 48
hours prior to the transfusion are excluded. These scenarios are rare,
typically occurring in approximately 2% of blood transfusions in GEMINI
data.

## Usage

``` r
n_rbc_transfusions(dbcon, cohort, exclude_ed = FALSE)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A `DBI` database connection to any GEMINI database.

- cohort:

  (`data.frame` or `data.table`) Cohort table with all relevant
  encounters of interest where each row corresponds to a single
  encounter. Must contain the following columns:

  - `genc_id` (`integer`): GEMINI encounter ID

  - `hospital_num` (`integer`): Hospital number

- exclude_ed:

  (`logical`) Whether to exclude transfusions in emergency department.
  When set to `TRUE`, transfusions performed in ED (before admission
  time) will not be counted. When set to `FALSE`, transfusions will not
  be filtered by time and all RBC transfusions in `transfusion` table
  will be counted. Please be aware that this may include transfusions
  before triage time, transfusions after discharge time, and
  transfusions without issuing time.

  Transfusions in ED are defined as `issue_date_time` earlier than
  `admission_date_time`. Transfusions with missing `issue_date_time`
  will be excluded when `exclude_ed` is set to `TRUE`.

## Value

(`data.table`)  
Table with three columns: `genc_id`, `n_app_rbc_transfusion_derived`
(number of appropriate RBC transfusions), and
`n_rbc_transfusion_derived` (total of all RBC transfusions). Encounters
without any transfusion will get a 0.

## Note

Transfusion data from two hospitals with known data quality issues are
automatically removed by this function. Any `genc_ids` from those sites
are not included in the returned output. When merging the output of this
function with another table, those `genc_ids` should have a value of
`NA`.

Currently, the function does not take `transfusion` or `lab` data
coverage into account. For patients without RBC transfusion, the
function will return `0` in result columns. User should check
transfusion and lab data coverage and decide whether the imputed `0`s
are appropriate or not.
