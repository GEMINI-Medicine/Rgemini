# Compute hospital Length of Stay (LoS)

`length_of_stay` returns Length of Stay for hospital admissions in unit
of hours and days.

## Usage

``` r
length_of_stay(
  ipadmdad,
  adm_dtvar = "admission_date_time",
  dis_dtvar = "discharge_date_time"
)
```

## Arguments

- ipadmdad:

  (`data.frame` or `data.table`)  
  Table with all relevant encounters of interest from DRM table
  "ipadmdad" (see [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/)). Must
  contain three fields: encounter ID (`genc_id`), and two date-time
  variables corresponding to admission and discharge (typically
  `admission_date_time` and `discharge_date_time`). Date-time variables
  must be in "yyyy-mm-dd hh:mm" format.

- adm_dtvar:

  (`character`)  
  Character string defining the column name for admission date-time
  (typically "admission_date_time").

- dis_dtvar:

  (`character`)  
  Character string defining the column name for discharge date-time
  (typically "discharge_date_time").

## Value

data.table with the same number of rows as input "ipadmdad", with
additional derived numeric fields labelled as "los_hrs_derived" and
"los_days_derived".

## Details

Length of Stay is defined as the duration of hospital in-patient stay,
calculated as `discharge_date_time - admission_date_time`. It is a
clinical outcome variable in various research studies and one of several
indicators of quality of care in GeMQIN reports.

## Warning

NA values in returned data.table indicates either missing or invalid
admission/discharge date-time.

## Note

The function takes two optional input arguments defining the admission
and discharge date-time variables of interest (by default
`admission_date_time` and `discharge_date_time`).
