# Write temp tables

This function writes temporary tables to the database to improve query
efficiency. Temporary tables are automatically removed once the user
disconnects from the database.

## Usage

``` r
temp_table(dbcon, data, table_name = "rgemini_temp_table", analyze = TRUE)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database.

- data:

  (`data.table` or `data.frame`)  
  Data table to be written to DB as temp table.

- table_name:

  (`data.table` or `data.frame`)  
  Name of temporary table in DB (default = "rgemini_temp_table").

- analyze:

  (`logical`)  
  Whether or not to use SQL Analyze statement to further improve query
  efficiency (recommended).

## Examples

``` r
if (FALSE) { # \dontrun{
temp_table(dbcon, data.table(genc_id = c(1, 2, 3)))
} # }
```
