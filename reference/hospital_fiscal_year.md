# Get hospital fiscal year

`hospital_fiscal_year` returns the corresponding hospital fiscal years
for a set of dates provided.

## Usage

``` r
hospital_fiscal_year(date_ymd)
```

## Arguments

- date_ymd:

  (`character`)  
  A character vector of dates or date-times. The date value must be in
  `yyyy-mm-dd` format.

## Value

Numeric vector with the same number of entries as input, containing the
derived fiscal year of all input dates (e.g., 2010, 2011, 2012, and so
on).

## Details

Hospital fiscal year is defined as April ~ March. For example, in case
of 2015-02-25, fiscal year will be 2014.

## Warning

NA values in returned vector indicates either missing date or its format
is incorrect. All dates must be in `yyyy-mm-dd` format and precede the
timestamp (e.g., `yyyy-mm-dd hh:mm`).

## Examples

``` r
# Get fiscal year for `discharge_date_time` variable in `ipadmdad` table
if (FALSE) { # \dontrun{
ipadmdad$fisc_year <- hospital_fiscal_year(ipadmdad$discharge_date_time)
} # }
```
