# Loop mLAPS

A wrapper around the
[`mlaps()`](https://gemini-medicine.github.io/Rgemini/reference/mlaps.md)
function which breaks down the calculation of mLAPS on a large number of
encounters by hospital-year. This avoids memory issues that can be
caused by loading large chunks of the lab table.

## Usage

``` r
loop_mlaps(
  db,
  cohort = NULL,
  hours_after_admission = 0,
  component_wise = FALSE
)
```

## Arguments

- db:

  (`DBIConnection`)  
  RPostgres DB connection.

- cohort:

  (`data.frame` or `data.table`)  
  Containing a column of `genc_ids` to restrict the calculation to.

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
If `output_laps_components == TRUE`: `genc_id` (`numeric`),  
`test_type_mapped_omop` (`character`),  
`mlaps` (`numeric`) max score for this test.

If `output_laps_components == FALSE`: `genc_id` (`numeric`),  
`mlaps` (`numeric`) sum of max scores for each relevant test for this
encounter.

## References

When the function is used, please cite the following:
https://doi.org/10.1097/MLR.0b013e3181589bb6
https://doi.org/10.1007/s11606-023-08245-w
https://doi.org/10.1101/2023.01.06.23284273

## Examples

``` r
if (FALSE) { # \dontrun{
drv <- DBI::dbDriver("PostgreSQL")
db <- DBI::dbConnect(
  drv,
  dbname = "db_name",
  host = "domain_name.ca",
  port = 1234,
  user = getPass::getPass("Enter Username"),
  password = getPass::getPass("Enter Password")
)

cohort <- DBI::dbGetQuery(db, "SELECT genc_id FROM public.admdad LIMIT 200;")

laps <- loop_laps(db, cohort = cohort)
} # }
```
