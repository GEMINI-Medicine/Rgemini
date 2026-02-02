# Identify encounters with disability

This function identifies whether or not an encounter had any ICD-10-CA
diagnosis code(s) indicating a physical, sensory, or intellectual/
developmental disability as defined by [Brown et al.,
2021](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2776018).

Broadly, this includes any diagnosis codes for conditions that are
likely to result in functional limitations and are considered to be
chronic (see Supplemental eTable 2 in [Brown et al.,
2021](https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2776018)
for a full list of diagnosis codes). However, note that this function
only searches for relevant diagnosis codes at the encounter level and
does not check whether chronic conditions are coded consistently across
different encounters for a given patient.

By setting `component_wise` to `TRUE`, users can choose to return all
identified diagnosis codes for encounters with a disability, together
with the disability category each code was matched with (e.g.,
physical/sensory/ developmental disability - see below).

## Usage

``` r
disability(cohort, ipdiag, erdiag, component_wise = FALSE)
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
  included when deriving disability in order to increase sensitivity.
  However, in certain scenarios, users may choose to only include IP
  diagnoses by specifying `erdiag = NULL`. This may be useful when
  comparing cohorts with different rates of ER admissions.

- component_wise:

  (`logical`) If `component_wise == FALSE` (default), the function
  calculates a single (global) disability flag indicating whether each
  `genc_id` was diagnosed with *any* disability.

  If `component_wise == TRUE`, for each `genc_id` with a disability, all
  identified disability diagnosis codes are returned, together with one
  of the following 7 disability categories:

  - Physical disability - Congenital Anomalies

  - Physical disability - Musculoskeletal disorders

  - Physical disability - Neurological disorders

  - Physical disability - Permanent Injuries

  - Sensory disabilities - Hearing impairments

  - Sensory disabilities - Vision impairments

  - Developmental Disabilities

## Value

`data.table` If `component_wise == FALSE`, returns a table with all
encounters identified by the `cohort` table input and an additional
derived field `disability` (`logical`) indicating whether any diagnosis
code for a disability was identified. If a `genc_id` does not have any
entry in the diagnosis table at all, `disability = NA`. Note that this
is very rare: If no additional filtering was performed, \>99.9% of
`genc_ids` should have an entry in the `ipdiagnosis` (and \>99.9% of
`genc_ids` that were admitted via ER should have an entry in the
`erdiagnosis` table).

If `component_wise == TRUE`, will only return `genc_ids` with a
disability. The output is returned in long format, where each row
corresponds to a disability diagnosis (`diagnosis_code`) and its
corresponding `disability_category` (`character`).

## Notes

This function does not differentiate between diagnosis types. That is,
the disability flags are derived based on all diagnosis codes that are
provided as input to this function. By default, users should include all
diagnosis types to identify disabilities. However, if users wish to
include only certain diagnosis types (e.g., type-M for most responsible
discharge diagnosis), the `ipdiag` and `erdiag` input tables should be
filtered accordingly based on `diagnosis_type` *prior to running this
function* (for more details, see [CIHI diagnosis type
definitions](https://www.cihi.ca/sites/default/files/document/diagnosis-type-definitions-en.pdf).

## References

Brown HK, et al. JAMA Netw Open, 2021.
https://doi.org/10.1001/jamanetworkopen.2020.34993

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

ipadmdad <- dbGetQuery(dbcon, "select * from admdad") %>% data.table()
ipdiagnosis <- dbGetQuery(dbcon, "select * from ipdiagnosis") %>% data.table()
erdiagnosis <- dbGetQuery(dbcon, "select * from erdiagnosis") %>% data.table()

# including ER diagnosis codes
disability(cohort = ipadmdad, ipdiag = ipdiagnosis, erdiag = erdiagnosis)

# not including ER diagnosis codes
disability(cohort = ipadmdad, ipdiag = ipdiagnosis, erdiag = NULL)

# returning component-wise disability categories
disability(ipadmdad, ipdiagnosis, erdiagnosis, component_wise = TRUE)
} # }
```
