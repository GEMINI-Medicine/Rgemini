# Compute day & time of hospital admissions

`day_time_of_admission` returns whether the patient was admitted during
day/night time and weekday/weekend.

## Usage

``` r
day_time_of_admission(ipadmdad, dtvar = "admission_date_time")
```

## Arguments

- ipadmdad:

  (`data.table` or `data.frame`)  
  Table with all relevant encounters of interest from DRM table
  "ipadmdad" (see [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/)). Must
  contain two fields: `genc_id` and a date-time variable (typically
  `admission_date_time` in "yyyy-mm-dd hh:mm" format).

- dtvar:

  (`character`)  
  Character string defining the date-time variable of interest (e.g.,
  "admission_date_time").

## Value

data.table with the same number of rows as input "ipadmdad", with
additional derived character fields labelled as
"day_of_admission_derived" and "time_of_admission_derived". Possible
values of these fields are "weekend" or "weekday" for the former,
"daytime" or "nighttime" for the latter.

## Details

Day time is defined between 08:00 ~ 16:59, and 17:00 ~ 07:59 for Night
time. Weekday is Monday ~ Friday, weekend is Sunday and Saturday.

This classification might be useful as baseline admission characteristic
in Epidemiological studies.

## Warning

NA values in returned data.table indicates either missing admission date
and time or its format is incorrect.

## Note

The function takes an optional input specifying the field label for the
date-time variable of interest. By default, this is
`admission_date_time` but users could specify a different variable
(e.g., `discharge_date_time`). This input can also be used in case of
differences in variable names between databases.
