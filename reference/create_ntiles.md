# Create N-tiles

This function bins continuous variables into quantiles (quartiles,
deciles, etc.) of the user's choosing. Bins are created according to the
interval `(low breakpoint, high breakpoint]`. If individual values are
tied around a breakpoint, they will be grouped into the same (lower)
bin, unlike dplyr::ntile(), which ignores ties to create equally-sized
bins. Note: The function will quit if any breakpoints are duplicated
(e.g. 1st quartile identical to 2nd quartile).

## Usage

``` r
create_ntiles(x, n)
```

## Arguments

- x:

  (`vector`)  
  A vector of numeric values.

- n:

  (`integer`)  
  The number of bins to create. For example, for quartiles set `n=4`.

## Value

(`factor`)  
A factor with `n` levels representing the bins (ntiles) of the input
values. The factor returned will be the same length as `x`.

## Examples

``` r
set.seed(123)
values <- rnorm(100)
quartiles <- create_ntiles(values, 4)
#>          0%         25%         50%         75%        100% 
#> -2.30916888 -0.49385424  0.06175631  0.69181917  2.18733299 
deciles <- create_ntiles(values, 10)
#>          0%         10%         20%         30%         40%         50% 
#> -2.30916888 -1.06822046 -0.62561263 -0.38719515 -0.22260033  0.06175631 
#>         60%         70%         80%         90%        100% 
#>  0.31482997  0.51301442  0.88153192  1.26449867  2.18733299 
```
