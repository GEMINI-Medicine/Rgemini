# Render Cell Suppression (Missing)

This is a custom render for `table1` missing variables which performs
GEMINI "cell suppression" for any variable levels which contain fewer
than 6 observations.

This is useful when you have an indicator variable for example, and you
would like to count the total number of events.
`[table1::render.default.categorical()]` will break down the indicator
variable into its components first (0 and 1) and then give you
individual counts. This will simply count 1s (for example).

## Usage

``` r
render_cell_suppression.missing(x, ...)
```

## Arguments

- x:

  (`character` or `factor`)  
  A variable with missing values to summarize.

- ...:

    
  Further arguments, passed to `table1:::stats.apply.rounding()` and
  [`prettyNum()`](https://rdrr.io/r/base/formatc.html) for additional
  formatting (e.g., `big.mark = ","`).

## Value

named (`character`)  
Concatenated with `""` to shift values down one row for proper
alignment.

## Examples

``` r
x <- factor(sample(0:1, 99, replace = TRUE), labels = c("Female", "Male"))
x[1:3] <- NA
render_cell_suppression.missing(x)
#>                                               Missing 
#>                         "" "&lt; 6 obs. (suppressed)" 

x[5:10] <- NA
render_cell_suppression.missing(x)
#>    Missing 
#> "9 (9.1%)" 
```
