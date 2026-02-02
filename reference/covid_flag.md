# COVID Flag

`covid_flag` returns COVID-19 diagnosis status for each `genc_id` based
on ICD-10-CA diagnosis codes.

Returns a table with two separate flags identifying encounters with
confirmed and suspected COVID-19 diagnoses using ICD-10-CA U07.1 and
U07.2 respectively.

A confirmed COVID-19 diagnosis (U07.1) is coded when there is a positive
COVID-19 test. Effective April 1 2023, U07.1 is coded when COVID-19 was
confirmed by any COVID-19 test, including polymerase chain reaction
(PCR) tests, point-of-care molecular tests (POCT), or rapid antigen
tests (RAT). For data prior to April 1, 2023, U07.1 was only coded when
COVID-19 was confirmed by a PCR lab test (for details, see [CIHI
diagnosis type
definitions](https://www.cihi.ca/sites/default/files/document/mapping-covid-19-codes-icd-10-ca-to-base-icd-10-job-aid-en.pdf)).

A suspected COVID-19 diagnosis (U07.2) is coded when an encounter is
clinically or epidemiological diagnosed but the associated COVID-19
tests are inconclusive, not available, or not performed.

## Usage

``` r
covid_flag(cohort, ipdiag, erdiag)
```

## Arguments

- cohort:

  (`data.frame` or `data.table`) Cohort table with all relevant
  encounters of interest, where each row corresponds to a single
  encounter. Must contain GEMINI Encounter ID (`genc_id`).

- ipdiag:

  (`data.table`) `ipdiagnosis` table as defined in the [GEMINI Data
  Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/). This
  table must contain `genc_id` and `diagnosis_code` (as ICD-10-CA
  alphanumeric code) in long format.

- erdiag:

  (`data.table`) `erdiagnosis` table as defined in the [GEMINI Data
  Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/). This
  table must contain `genc_id` and `er_diagnosis_code` (as ICD-10-CA
  alphanumeric code) in long format. Typically, ER diagnoses should be
  included when deriving the COVID flags in order to increase
  sensitivity. However, in certain scenarios, users may choose to only
  include IP diagnoses by specifying `erdiag = NULL`. This may be useful
  when comparing cohorts with different rates of ER admissions.

## Value

data.table with the same number of rows as input `cohort` with
additional derived boolean fields labelled as
`"covid_icd_confirmed_flag"` and `"covid_icd_suspected_flag"`. Possible
values are `TRUE`, `FALSE` or `NA`. `NA` indicates that an encounter
does not have a diagnosis code in the diagnosis table input.

When one tries to left-join the output of this function with another
table (another list of admissions in the left), make sure list of
admissions (or patient) aligns in both tables.

## Details

Below is the current ICD-10-CA diagnosis codes related to COVID-19. For
more details, please refer to the references in this page.

- U07.1: For confirmed cases of COVID-19, assign U07.1 Emergency use of
  U07.1 (COVID-19, virus identified)

- U07.2: For suspected cases of COVID-19, assign U07.2 Emergency use of
  U07.2 (COVID-19, virus not identified)

- U07.3: Multisystem inflammatory syndrome associated with COVID-19

- U07.4: post COVID-19 condition

- U07.5: personal history of COVID-19

- U07.6: Need for immunization against COVID-19. This code is for use
  when a person encounters health services for the sole purpose of
  receiving the COVID-19 vaccine.

- U07.7: COVID-19 vaccines causing adverse effect in therapeutic use.
  This emergency use code is an external cause code. This code would
  normally be located at the ICD-10-CA block Y40-Y59 Drugs, medicaments
  and biological substances causing adverse effects in therapeutic use.

## Note

This function does not differentiate between diagnosis types. That is,
the COVID flags are derived based on all diagnosis codes that are
provided as input to this function. By default, users should include all
diagnosis types to identify COVID. However, if users wish to include
only certain diagnosis types (e.g., type-M for most responsible
discharge diagnosis), the `ipdiag` and `erdiag` input tables should be
filtered accordingly based on `diagnosis_type` *prior to running this
function* (for more details, see [CIHI diagnosis type
definitions](https://www.cihi.ca/sites/default/files/document/diagnosis-type-definitions-en.pdf).

## References

[CIHI COVID-19 data collection and coding
direction](https://www.cihi.ca/en/covid-19-resources/covid-19-data-collection-and-coding-direction)

## Examples

``` r
if (FALSE) { # \dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)

ipadm <- dbGetQuery(dbcon, "select * from admdad") %>% data.table()
ipdiagnosis <- dbGetQuery(dbcon, "select * from ipdiagnosis") %>% data.table()
erdiagnosis <- dbGetQuery(dbcon, "select * from erdiagnosis") %>% data.table()

covid <- covid_flag(cohort = ipadm, ipdiag = ipdiagnosis, erdiag = )
} # }
```
