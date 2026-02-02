# Maximum Pairwise Standardized Mean Difference

Maximum Pairwise Standardized Mean Difference

## Usage

``` r
max_pairwise_smd(x, name, round_to = 3, ...)
```

## Arguments

- x:

  named (`list`)  
  Where each list element is a vector corresponding to the observations
  of the variable of interest for that particular strata (corresponding
  to the name). See example for how this can be constructed.

- name:

  (`character`)  
  Unused variable that is required to be supported by `extra.col` in
  [`table1::table1()`](https://rdrr.io/pkg/table1/man/table1.html).

- round_to:

  (`numeric`)  
  How many digits to round the standardized mean difference to.

- ...:

    
  Additional arguments passed on to
  [`table1::table1()`](https://rdrr.io/pkg/table1/man/table1.html).

## Value

(`numeric`)  
The maximum pairwise standardized mean difference between all strata for
a particular variable.

## Note

The implementation with the `stddiff` package is more fragile, than with
the `smd` package. However, the `smd` package uses the *population
variance* to calculate the SMD as opposed to the *sample variance*. This
can cause small inaccuracies in the final result. Therefore we elect to
implement with `stddiff`. Another consideration is that `stddiff` can be
maximally precise to only 3 decimal places.

Additionally, for very large cohorts, standardized mean differences
calculated on categorical variables using the `stddiff` package may
throw `In n[1] * n[2] : NAs produced by integer overflow`. This is an
implementation issue in `stddiff` in the calculation of the standard
errors for the standardized mean differences. However, since these
standard errors are not used in the final output, they can be safely
ignored.

## Examples

``` r
max_pairwise_smd(split(mtcars$disp, mtcars$am))
#> [1] 1.478
```
