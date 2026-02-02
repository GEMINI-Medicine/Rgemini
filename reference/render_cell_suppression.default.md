# Render Cell Suppression (Default)

This is a wrapper around the render functions for each type of variable.

## Usage

``` r
render_cell_suppression.default(
  x,
  name,
  missing = any(is.na(x)),
  transpose = FALSE,
  render.empty = "NA",
  render.continuous = render_cell_suppression.continuous,
  render.categorical = render_cell_suppression.categorical,
  render.missing = render_cell_suppression.missing,
  ...
)
```

## Arguments

- x:

  (`vector`)  
  A vector of numeric, factor, character or logical values.

- name:

  (`character`)  
  Name of the variable to be rendered (ignored).

- missing:

  (`logical`)  
  Should missing values be included?

- transpose:

  (`logical`)  
  Logical indicating whether on not the table is transposed.

- render.empty:

  (`character`)  
  A character to return when x is empty.

- render.continuous:

  (`function`)  
  A function to render continuous (i.e. numeric) values. Can also be a
  character string, in which case it is passed to
  `table1:::parse.abbrev.render.code()`.

- render.categorical:

  (`function`)  
  A function to render categorical (i.e. factor, character or logical)
  values. Can also be a character string, in which case it is passed to
  `table1:::parse.abbrev.render.code()`.

- render.missing:

  (`function`)  
  A function to render missing (i.e. NA) values. Can also be a character
  string, in which case it is passed to
  `table1:::parse.abbrev.render.code()`. Set to `NULL` to ignore missing
  values.

- ...:

    
  Further arguments, passed to `table1:::stats.apply.rounding()` or
  [`prettyNum()`](https://rdrr.io/r/base/formatc.html) for additional
  formatting (e.g., `big.mark = ","`).

## Value

(`character`)  
Summary of variable as a character vector with cell suppression applied.
