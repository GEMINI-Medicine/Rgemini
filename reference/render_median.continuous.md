# Render Median (Continuous)

This is the default renderer for continuous variables in the `table1`
package. It will generate a formatted median with first and third
quartiles for each level of the variable.

## Usage

``` r
render_median.continuous(x, ...)
```

## Arguments

- x:

  (`character` or `factor`)  
  A continuous variable to summarize.

- ...:

    
  Further arguments, passed to `table1:::stats.apply.rounding()`.

## Value

named (`character`)  
Concatenated with `""` to shift values down one row for proper
alignment.

## Examples

``` r
render_median.continuous(mtcars$disp)
#>                                           Median [Q1, Q3] 
#>                           "" "196.300 [120.825, 326.000]" 
```
