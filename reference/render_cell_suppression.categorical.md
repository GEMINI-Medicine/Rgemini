# Render Cell Suppression (Categorical)

This is a custom render for `table1` categorical variables which
performs GEMINI "cell suppression" for any variable levels which contain
fewer than 6 observations.

If the total number of these variable levels with fewer than 6
observations is less than 6, *all* cells for all levels of the variable
must be censored (because it is possible to indirectly deduce the
missing counts otherwise).

## Usage

``` r
render_cell_suppression.categorical(x, ...)
```

## Arguments

- x:

  (`character` or `factor`)  
  A categorical variable to summarize.

- ...:

    
  Optionally accept a named `digits` (`integer`) or
  `single_level_binary` (`logical`) argument which specifies the number
  of digits to round percentages to. Also accepts additional inputs that
  are passed to [`prettyNum()`](https://rdrr.io/r/base/formatc.html) to
  apply additional formatting to shown results (e.g., `big.mark = ","`).

## Value

named (`character`)  
Concatenated with `""` to shift values down one row for proper
alignment.

## Examples

``` r
x <- factor(c(rep("a", times = nrow(mtcars)), "b"), levels = c("a", "b"))
render_cell_suppression.categorical(x)
#>                             a              b 
#>             "" "(suppressed)" "(suppressed)" 

x2 <- factor(c(rep("a", times = nrow(mtcars))), levels = c("a", "b"))
render_cell_suppression.categorical(x2)
#>                           a             b 
#>            "" "32 (100.0%)"    "0 (0.0%)" 

y <- factor(
  c(rep("a", times = nrow(mtcars)), "b", "c", "d", "e", "f", "g"),
  levels = c("a", "b", "c", "d", "e", "f", "g")
)
render_cell_suppression.categorical(y)
#>                                                     a 
#>                         ""               "32 (84.2%)" 
#>                          b                          c 
#> "&lt; 6 obs. (suppressed)" "&lt; 6 obs. (suppressed)" 
#>                          d                          e 
#> "&lt; 6 obs. (suppressed)" "&lt; 6 obs. (suppressed)" 
#>                          f                          g 
#> "&lt; 6 obs. (suppressed)" "&lt; 6 obs. (suppressed)" 

z <- factor(
  c(
    rep("a", times = 100),
    rep("b", times = 50),
    rep("c", times = 7),
    rep("d", times = 2)
  ),
  levels = c("a", "b", "c", "d", "e")
)

render_cell_suppression.categorical(z)
#>                             a              b              c              d 
#>             ""  "100 (62.9%)"   "50 (31.4%)" "(suppressed)" "(suppressed)" 
#>              e 
#>     "0 (0.0%)" 
```
