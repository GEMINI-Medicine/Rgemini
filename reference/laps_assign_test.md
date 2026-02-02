# Assign score to LAPS component

Assign score to LAPS component

## Usage

``` r
laps_assign_test(x, breaks, points)
```

## Arguments

- x:

  (`character`)  
  A vector of result values for a particular lab test.

- breaks:

  (`numeric`)  
  A vector of intervals for a particular test which correspond to a
  different number of points.

- points:

  (`numeric`)  
  A vector of scores for each interval in `breaks`.

## Value

(`numeric`)  
A vector of scores for each lab result value provided in `x`.

## Examples

``` r
x <- c("0.3", "0.7")
breaks <- c(0, 0.2, 0.4, 0.8, Inf)
points <- c(1, 2, 3, 4)

laps_assign_test(x, breaks, points)
#> [1] 2 3
```
