# Not In

Infix function for the opposite of `%in%`.

## Usage

``` r
x %ni% y
```

## Arguments

- x:

  (`vector` or `NULL`) The values to be matched. Long vectors are
  supported.

- y:

  (`vector` or `NULL`) The values to be matched against. Long vectors
  are not supported.

## Value

(`logical`) A vector of logical values the same length of `x`, for which
each value represents whether that particular element in `x` exists in
`y`.

## Examples

``` r
c("x", "y") %ni% "x"
#> [1] FALSE  TRUE
```
