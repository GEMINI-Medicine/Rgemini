# Coerce to `data.table`

Some `Rgemini` functions rely on `data.table` operations and assume the
input is provided in `data.table` format. If it is not, coerce with
message to ensure the function logic works without breaking.

## Usage

``` r
coerce_to_datatable(data)
```

## Arguments

- data:

  (`data.frame` or `data.table`)  
  The data to check class of and coerce to `data.table` if necessary.

## Value

(`data.table`)  
The original data provided as an argument, but as a `data.table`.

## Examples

``` r
mtcars <- coerce_to_datatable(mtcars)
#> Warning: mtcars was passed as a data.frame and has been coerced to a data.table.
```
