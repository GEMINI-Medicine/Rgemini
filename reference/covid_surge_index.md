# Derive COVID Surge Index

A function that derives the COVID-19 surge index for sites. For time
periods before 2020, or where COVID-19 was not yet diagnosed, the surge
index will be 0. The function filters for the All-Medicine + ICU cohort.
This includes any encounter admitted/discharged from a medical service
or encounters who entered the ICU at any point (specialized or stepdown
unit).

The function needs to be run on the entire cohort to create accurate
values. If users have pre-filtered cohorts, please reach out to the
GEMINI team to derive the table.

## Usage

``` r
covid_surge_index(dbcon, gim_only = FALSE, include_er = FALSE)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  RPostgres DB connection.

- gim_only:

  (`logical`)  
  Flag denoting if the user would like the surge index for GIM patients
  only. Note: This argument is set to FALSE by default.

- include_er:

  (`logical`)  
  Flag denoting if ER intervention codes are to be included in the
  calculation. Note: This argument is set to FALSE by default.

## Value

(`data.frame`)  
A data.table containing each hospital and the COVID surge index for the
given month year.

## Details

The COVID surge index is a "severity-weighted measure of COVID-19
caseload relative to pre-COVID-19 bed capacity" (Kadri et al, 2021) The
index looks at overall COVID admissions, as well as COVID admissions who
entered the ICU and underwent mechanical ventilation.

## References

Kadri S, et al. Annals of Internal Medicine, 2021.
https://doi.org/10.7326/m21-1213

McAlister FA et al. JAMA Network Open, 2023.
https://doi.org/10.1001/jamanetworkopen.2023.23035

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

covid_surge <- covid_surge_index(
  dbcon = db,
  gim_only = FALSE,
  include_er = FALSE
)
} # }
```
