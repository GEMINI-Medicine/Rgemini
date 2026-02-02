# Compute Length of Stay in Intensive Care Unit

Calculates the total duration in Intensive Care Unit (ICU) that an
encounter spent during a hospital stay using CIHI Discharge Abstract
Database (DAD) fields

## Usage

``` r
icu_los(cohort, ipscu)
```

## Arguments

- cohort:

  (`data.table` or `data.frame`)  
  Table with all relevant encounters of interest, where each row
  corresponds to a single encounter. Must contain GEMINI Encounter ID
  (`genc_id`).

- ipscu:

  (`data.table` or `data.frame`)  
  Table equivalent to the `ipscu` table defined in the [GEMINI Data
  Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/). Table
  must contain fields: GEMINI Encounter ID (`genc_id`), SCU admission
  time (`scu_admit_date_time` in "yyyy-mm-dd hh:mm" format), SCU
  discharge time (`scu_discharge_date_time` in "yyyy-mm-dd hh:mm"
  format), and SCU number (`scu_unit_number`).

## Value

(`data.table`)  
By default, for each encounter in input "cohort" returns the
corresponding derived numeric fields "icu_los_hrs_derived" and
"icu_los_days_derived".

## Details

This function calculates the length of stay (LoS) in hours and days that
an encounter spent in the ICU during a hospital stay.

It uses DAD fields SCU Admit date-time (Group 13, Field 03/04) and SCU
Discharge date-time (Group 13, Field 05/06) to derive these numeric
fields.

Rows with either of SCU Admit/Discharge date-time fields missing, will
not be counted in calculation.

For encounters with multiple ICU visits, the function returns the sum of
the duration of each visit.

By definition in DAD (Group 13, Field 02), SCUs include ICUs and
Step-Down Units. Step-Down Units are not considered as ICUs.

Therefore, this function excludes below CIHI defined Step-Down Units
numbers from calculation:

- 90: Step-Down Medical Unit

- 93: Combined Medical/Surgical Step-Down Unit

- 95: Step-Down Surgical Unit

- 99: No SCU

Please refer to the CIHI DAD abstracting manual for more details.

## Note

: Encounter IDs in the `cohort` table that are not present in the
`ipscu` table are assumed to have no visits to ICU. For these
encounters, a value of 0 will be assigned to the derived fields.
Encounter IDs in the `ipscu` table that have any missing/invalid
`scu_admit_date_time` or `scu_discharge_date_time` will be returned with
`icu_los = NA`. Some of those entries have valid date information (but
no timestamp). Users may choose to impute missing timestamps prior to
running this function. When one tries to left-join the output of this
function to another table, make sure the list of encounters aligns in
both tables.

## Examples

``` r
# Compute ICU LoS for all encounters in ipadmdad:
if (FALSE) { # \dontrun{
icu_los(cohort = ipadmdad, ipscu = ipscu)
} # }
```
