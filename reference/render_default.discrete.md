# Render Default (Discrete)

This is the default render for discrete variables in the `table1`
package. It will generate a sum for each level of the variable.

## Usage

``` r
render_default.discrete(x, ...)
```

## Arguments

- x:

  (`character` or `factor`)  
  A discrete variable to summarize.

- ...:

    
  Further arguments, passed to
  [`prettyNum()`](https://rdrr.io/r/base/formatc.html) for additional
  formatting (e.g., `big.mark = ","`).

## Value

named (`character`)  
Concatenated with `""` to shift values down one row for proper
alignment.

## Examples

``` r
render_default.discrete(mtcars$vs)
#>       Sum 
#>   "" "14" 
```
