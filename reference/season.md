# Get Season

Given a date, return the named season.

## Usage

``` r
season(date)
```

## Arguments

- date:

  (`Date`)  

## Value

(`character`)  
One of "Spring", "Summer", "Fall", "Winter".

## Examples

``` r
season(as.Date(Sys.Date()))
#> [1] "Winter"
```
