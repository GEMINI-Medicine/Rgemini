# Simulate ICD-10 Diagnosis Codes

This function simulates ICD-10 diagnosis codes at random or by user
specified pattern.

## Usage

``` r
sample_icd(n = 1, source = "comorbidity", dbcon = NULL, pattern = NULL)
```

## Arguments

- n:

  (`integer`)  
  Number of ICD codes to simulate.

- source:

  (`string`)  
  The source of the ICD coding to sample from. Default to "comorbidity"
  the 2011 version of ICD-10 codes implemented in the R
  [comorbidity](https://ellessenne.github.io/comorbidity/index.html)
  package. If `source` is `icd_lookup`, ICD-10-CA codes will be sampled
  from the `lookup_icd10_ca_description` table in the GEMINI database,
  see details in the [GEMINI Data Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/).

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database. Required when `source`
  is `icd_lookup`.

- pattern:

  (`string`)  
  A valid regex expression that specifies the desired pattern that the
  returned ICD codes should be matched with.

## Value

(`vector`)  
A vector of ICD diagnostic codes.

## Examples

``` r
### Simulate 100 ICD-10 codes based on the 2011 version.
if (FALSE) { # \dontrun{
sample_icd(100, source = "comorbidity")
} # }

### Simulate 100 ICD-10 codes starting with "C2" or "E10" based on the 2011 version.
if (FALSE) { # \dontrun{
sample_icd(100, source = "comorbidity", pattern = "^C2|^E10")
} # }

### Simulate 50 ICD-10-CA codes based on codes found in the `lookup_icd10_ca_description` table
if (FALSE) { # \dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)
sample_icd(50, source = "icd_lookup", dbcon = dbcon)
} # }
```
