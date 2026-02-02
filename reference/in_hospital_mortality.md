# Compute in-hospital mortality using CIHI DAD

`in_hospital_mortality` returns whether a patient has deceased in
hospital based on the CIHI DAD field discharge disposition

## Usage

``` r
in_hospital_mortality(ipadmdad, suicide = FALSE)
```

## Arguments

- ipadmdad:

  (`data.frame` or `data.table`)  
  Table with all relevant encounters of interest from DRM table
  "ipadmdad" (see [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/)). Must
  contain two fields: `genc_id` and `discharge_disposition`.

- suicide:

  (`logical`)  
  a TRUE/FALSE argument indicating whether suicides in facility should
  be counted towards in-hospital mortality rate or not.

## Value

`data.table` with the same number of rows as input `ipadmdad`, with
additional derived boolean field labelled as
`in_hospital_mortality_derived`. Possible values are `TRUE`, `FALSE` or
`NA`, where `NA` indicates missing discharge disposition.

## Details

This function takes the CIHI DAD "discharge disposition" field (Group
05, Field 05 As of 19/20 CIHI DAD) and generates a boolean variable
indicating whether the patient has deceased during hospital stay.

The current version of the function captures the four possible values
indicating that a patient has deceased in hospital:

- 7: Died (Deprecated post 2017)

- 72: Died in Facility

- 73: Medical Assistance in Dying

- 74: Suicide in Facility (Excluded in calculation by default; can be
  included by setting `suicide` argument to `TRUE`)

## Warning

`NA` values in returned data.table indicate missing discharge
disposition field values.
