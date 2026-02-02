# Compare two sets to get the number of unique and common elements in each set

This function takes in two vectors and returns the number of overlapping
elements, along with the number of unique elements. The function can
compare sets of characters, numerics, and dates.

## Usage

``` r
compare_sets(x, y, dates = FALSE, orders = c("ymd", "ymd HM", "ymd HMS"))
```

## Arguments

- x:

  (`vector`)  
  The first set to be compared

- y:

  (`vector`)  
  The second set to be compared

- dates:

  (`logical`)  
  If set to TRUE, will attempt to convert x & y into dates, and then
  compare. Set to FALSE by default.

- orders:

  (`vector`)  
  Allows user to specify potential date or date-time formats they would
  like to compare. The function will check for `"ymd"`, `"ymd HM"`, and
  `"ymd HMS"` by default.

  For example, users can set `orders = "dmy"` to compare vectors in
  `dmy` format.

## Value

(`data.table`)  
A table showing the number of elements in both vectors, in the first
vector only, and in the second vector only

## Examples

``` r
compare_sets(c(1:10), c(5:10), dates = FALSE)
#>    in_both x_only y_only
#>      <int>  <int>  <int>
#> 1:       6      4      0
```
