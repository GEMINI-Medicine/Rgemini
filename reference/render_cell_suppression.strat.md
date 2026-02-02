# Render Cell Suppression (Strata)

This is a custom render for `table1` stratification variables which
performs GEMINI "cell suppression" for any levels which contain fewer
than 6 observations.

Note that even with strata variable cell suppression, it is possible to
reverse-calculate the total given the overall column. Therefore it is
recommended to also hide the "Overall" column in the call to
[`table1::table1()`](https://rdrr.io/pkg/table1/man/table1.html).

## Usage

``` r
render_cell_suppression.strat(label, ..., transpose = FALSE)
```

## Arguments

- label:

  (`character`)  
  For table1 versions up to 1.4.3: A character vector containing the
  labels. For table1 versions \>= 1.5.0: A list item with data for each
  strata.

- ...:

    
  Optional additional arguments. Note that the current version expects
  this to be `n` for each strata, mimicing the behavior of table1
  version \<= 1.4.3, where `n` was explicitly passed to this function.

- transpose:

  (`logical`)  
  Used internally by
  [`table1::table1()`](https://rdrr.io/pkg/table1/man/table1.html).

## Value

named (`character`)  
Concatenated with `""` to shift values down one row for proper
alignment.

## Note

Arguments from this function should not be passed directly and are
defined here to work internally with
[`table1::table1()`](https://rdrr.io/pkg/table1/man/table1.html).
