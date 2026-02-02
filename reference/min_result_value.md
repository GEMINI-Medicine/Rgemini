# Calculates minima when input vector is not empty, else returns NA.

Calculates minima when input vector is not empty, else returns NA.

## Usage

``` r
min_result_value(x)
```

## Arguments

- x:

  (`numeric`)  
  A vecctor of numerical values.

## Value

(`numeric`)  
A numerical value or NA

## Details

This is a helper function to suppress default warning message from
\`base::min()“ function when all elements in the input vector is NA,
which can be problematic for unit testing. Default to remove NA values
in minima calculation.
