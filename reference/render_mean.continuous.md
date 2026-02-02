# Render Mean (Continuous)

This is the default renderer for continuous variables in the `table1`
package. It will generate a formatted mean and standard deviation for
each level of the variable.

## Usage

``` r
render_mean.continuous(x, ...)
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
render_mean.continuous(mtcars$disp)
#>                                                 Mean (SD) 
#>                           "" "230.722 (&plusmn; 123.939)" 
```
