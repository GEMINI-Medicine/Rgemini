# Generated simulated lab data

Designed to mimic the most important elements of the GEMINI lab table as
defined in the [GEMINI Data Repository
Dictionary](https://geminimedicine.ca/the-gemini-database/).

## Usage

``` r
dummy_lab(id, omop, value, unit, mintime)
```

## Arguments

- id:

  (`numeric`)  
  A single identifier that is repeated to match the length of `value`.

- omop:

  (`character`)  
  Codes corresponding to OMOP concept identifiers.

- value:

  (`numeric`)  
  Simulated result values for each lab test measurement.

- unit:

  (`character`)  
  Units corresponding to the particular lab test as defined by `omop`.
  It is repeated to match the length of `value`.

- mintime:

  (`character`)  
  In the format yyyy-mm-dd hh:mm. Earliest recorded test performed time.

## Value

(`data.table`)  
With the columns, `id`, `omop`, `value`, `unit`, and
`collection_date_time` as described above.

## Examples

``` r
lab <- dummy_lab(1, 3024641, c(7, 8, 15, 30), "mmol/L", "2023-01-02 08:00")
```
