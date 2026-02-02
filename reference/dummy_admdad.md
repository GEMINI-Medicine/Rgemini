# Generated simulated administrative data

Designed to partially mimic the `admdad` table as defined in the [GEMINI
Data Repository
Dictionary](https://geminimedicine.ca/the-gemini-database/).

## Usage

``` r
dummy_admdad(id, admtime)
```

## Arguments

- id:

  (`numeric`)  
  A single identifier that is repeated to match the length of `value`.

- admtime:

  (`character`)  
  In the format yyyy-mm-dd hh:mm. Corresponds to the admission time of
  the encounter.

## Value

(`data.table`)  
With the columns `id` and `admission_date_time` as described above.

## Examples

``` r
admdad <- dummy_admdad(1, "2023-01-02 00:00")
```
