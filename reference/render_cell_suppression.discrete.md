# Render Cell Suppression (Discrete)

This is a custom render for `table1` discrete variables which performs
GEMINI "cell suppression" for any variable levels which contain fewer
than 6 observations.

This is useful when you have an indicator variable for example, and you
would like to count the total number of events.
`[table1::render.default.categorical()]` will break down the indicator
variable into its components first (0 and 1) and then give you
individual counts. This will simply count 1s (for example).

## Usage

``` r
render_cell_suppression.discrete(x)
```

## Arguments

- x:

  (`character` or `factor`)  
  A discrete variable to summarize.

## Value

named (`character`)  
Concatenated with `""` to shift values down one row for proper
alignment.

## Examples

``` r
x <- 1:6
render_cell_suppression.discrete(x)
#>       Sum 
#>   "" "21" 

y <- 1:2
render_cell_suppression.discrete(y)
#>                                                   Sum 
#>                         "" "&lt; 6 obs. (suppressed)" 
```
