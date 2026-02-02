# Compute Length of Stay in Emergency Room

Calculate the total duration in Emergency Room (ER) that an encounter
spent through a hospitalization using CIHI National Ambulatory Care
Reporting System (NACRS) fields

## Usage

``` r
er_los(cohort, er)
```

## Arguments

- cohort:

  (`data.table` or `data.frame`)  
  Table with all relevant encounters of interest, where each row
  corresponds to a single encounter. Must contain GEMINI Encounter ID
  (`genc_id`).

- er:

  (`data.table` or `data.frame`)  
  Table equivalent to the `er` table defined in the [GEMINI Data
  Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/). Table
  must contain fields: GEMINI Encounter ID (`genc_id`), ER triage
  date-time (`triage_date_time` in "yyyy-mm-dd hh:mm" format), Left ER
  date-time (`left_er_date_time` in "yyyy-mm-dd hh:mm" format).

## Value

(`data.table`)  
By default, for each encounter in input `cohort` returns the
corresponding derived numeric fields `er_los_hrs_derived` and
`er_los_days_derived`.

## Details

This function calculates the length of stay (LoS) in hours and days that
an encounter spent in the ER during a hospital stay.

It uses NACRS fields Triage Date and Time (Data Element Number 24/25)
and Date and Time Patient Left Emergency Department (Data Element Number
116/117).

## Note

Encounter IDs in the `cohort` table that are not present in the `er`
table are assumed to have no ER visit and are returned with
`er_los = 0`. Please check if any of these entries might be due to data
availability issues and consider removing them from your analyses.
Encounter IDs in the `er` table that have missing/invalid
`triage_date_time` or `left_er_date_time` will be returned with
`er_los = NA`.

## Examples

``` r
# Compute ER LoS for all encounters in ipadmdad;
if (FALSE) { # \dontrun{
er_los(cohort = ipadmdad, er = er)
} # }
```
