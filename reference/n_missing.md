# Number of missingness

This function checks number of missingness in a `vector`, or a
`data.frame` or `data table`. It returns the results in exact number
(percentage) or returns the index of missingness

## Usage

``` r
n_missing(x, na_strings = c(""), index = FALSE)

mi2(x, na_strings = c(""), index = FALSE)
```

## Arguments

- x:

  (`vector` or `data.frame` or `data.table`) the object to be checked

- na_strings:

  (`character`) a character vector of strings which are to be
  interpreted as `NA` values. The default for `n_missing` is "", which
  will treat empty strings as `NA`.

- index:

  (`logical`) If true the function returns the index of the missing
  values instead of the number (percentage). For `vector` input, a
  logical vector of same length will be returned. For `data.frame` or
  `data.table` input, the function returns a vector with length equal to
  the number of rows of input table and any missingness in the row will
  result in a returned value of `TRUE` for the row.

## Value

(`character` or `logical`) For `vector` input, a `character` vector the
same length of `x` where each value represents whether that particular
element is missing. For `data.frame` or `data.table` input, a `logical`
vector of which the length is the number of rows of input table and each
value represents whether any element of the row is missing.

## Examples

``` r
df <- data.frame(
  x = c(1, 2, NA, 4, 5),
  y = as.POSIXct(c(
    "2023-12-01 15:00", "2021-07-09 19:30", "2020-01-02 09:00",
    "1998-08-23 15:23", NA
  )),
  z = c("NA", NA, "a", " ", ""),
  v = as.Date(c("2023-10-23", "2012-06-30", NA, "2023-08-12", "2009-01-01"))
)
n_missing(df)
#>           x           y           z           v 
#> "1 (20.0%)" "1 (20.0%)" "2 (40.0%)" "1 (20.0%)" 
n_missing(df$z)
#> [1] "2 (40.0%)"
n_missing(df, na_strings = c("", "NA", " "))
#>           x           y           z           v 
#> "1 (20.0%)" "1 (20.0%)" "4 (80.0%)" "1 (20.0%)" 
n_missing(df$z, index = TRUE)
#> [1] FALSE  TRUE FALSE FALSE  TRUE
n_missing(df, na_strings = c("", "NA", " "), index = TRUE)
#> [1] TRUE TRUE TRUE TRUE TRUE
```
