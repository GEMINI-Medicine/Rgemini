# Render Cell Suppression (Continuous)

This is a custom renderer for `table1` continuous variables which
performs GEMINI "cell suppression" for any variable levels which contain
fewer than 6 observations.

## Usage

``` r
render_cell_suppression.continuous(x, ...)
```

## Arguments

- x:

  (`numeric`)  
  A continuous variable to summarize.

- ...:

    
  Further arguments, such as `continuous_fn`, or those passed to
  `table1:::stats.apply.rounding()`. Use `continuous_fn` to specify the
  summary statistics to display, which accepts character string: "mean",
  "median", or c("mean", "median") to display both. Defaults to "mean".

## Value

named (`character`)  
Concatenated with `""` to shift values down one row for proper
alignment.

## Examples

``` r
x <- 1:6
render_cell_suppression.continuous(x)
#>                                         Mean (SD) 
#>                       "" "3.500 (&plusmn; 1.871)" 

y <- 1:2
render_cell_suppression.continuous(y)
#>                                             Mean (SD) 
#>                         "" "&lt; 6 obs. (suppressed)" 

## Use in `table1`:
if (FALSE) { # \dontrun{
library(table1)
dat <- expand.grid(id = 1:10, treat = c("Treated", "Placebo"))
dat$age <- runif(nrow(dat), 10, 50)
label(dat$age) <- "Age"

table1(~ age | treat,
  data = dat,
  render.continuous = render_cell_suppression.continuous,
  continuous_fn = c("mean", "median"), # to display mean and median simultaneously
  digits = 2
)
} # }
```
