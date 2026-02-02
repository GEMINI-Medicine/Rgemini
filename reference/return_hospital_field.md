# Return Hospital Field

To accommodate differences in column names between databases, find the
name of the column corresponding to the hospital for downstream queries.

## Usage

``` r
return_hospital_field(db)
```

## Arguments

- db:

  (`DBIConnection`)  
  RPostgres DB connection.

## Value

(`character`)  
`hospital_id` or `hospital_num`, with preference given to `hospital_id`
if it exists.
