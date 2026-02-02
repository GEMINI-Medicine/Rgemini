# Generate Simulated Diagnosis Data Table

This function generates simulated data table resembling `ipdiagnosis` or
`erdiagnosis` tables that can be used for testing or demonstration
purposes. It internally calls
[`sample_icd()`](https://gemini-medicine.github.io/Rgemini/reference/sample_icd.md)
function to sample ICD-10 codes and accepts arguments passed to
[`sample_icd()`](https://gemini-medicine.github.io/Rgemini/reference/sample_icd.md)
for customizing the sampling scheme.

## Usage

``` r
dummy_diag(nid = 5, nrow = 50, ipdiagnosis = TRUE, diagnosis_type = NULL, ...)
```

## Arguments

- nid:

  (`integer`)  
  Number of unique encounter IDs (`genc_id`) to simulate. Value must be
  greater than 0.

- nrow:

  (`integer`)  
  Total number of rows of the simulated long format diagnosis table.
  Value must be greater than or equal to that in `nid`.

- ipdiagnosis:

  (`logical`)  
  Default to "TRUE" and returns simulated "ipdiagnosis" table. If FALSE,
  returns simulated "erdiagnosis" table. See tables in [GEMINI Data
  Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/).

- diagnosis_type:

  (`character vector`)  
  The type(s) of diagnosis to return. Possible diagnosis types are ("M",
  1", "2", "3", "4", "5", "6", "9", "W", "X", and "Y"). Regardless of
  `diagnosis_type` input, the `ipdiagnosis` table is defaulted to always
  return type "M" for the first row of each encounter.

- ...:

  Additional arguments for ICD code sampling scheme. See
  [`sample_icd()`](https://gemini-medicine.github.io/Rgemini/reference/sample_icd.md)
  for details.

## Value

(`data.table`)  
A data table containing simulated data of `genc_id`,
`(er)_diagnosis_code`, `(er)_diagnosis_type`, `hospital_num`, and other
fields found in the respective diagnosis table.

## Details

To ensure simulated table resembles "ip(er)diagnosis" table, the
following characteristics are applied to fields:

- `genc_id`: Numerical identification of encounters starting from 1. The
  number of unique encounters is defined by `nid`. The total number of
  rows is defined by `nrow`, where the number of rows for each encounter
  is random, but each encounter has at least one row.

- `hospital_num`: Numerical identification of hospitals from 1 to 5. All
  rows of an encounter are linked to a single hospital

- `diagnosis_code`: "ipdiagnosis" table only. Simulated ICD-10 diagnosis
  codes. Each encounter can be associated with multiple diagnosis codes
  in long format.

- `diagnosis_type`: "ipdiagnosis" table only. The first row of each
  encounter is consistently assigned to the diagnosis type "M". For the
  remaining rows, if `diagnosis_type` is specified by users, diagnosis
  types are sampled randomly from values provided; if `diagnosis_type`
  is NULL, diagnosis types are sampled from ("1", "2", "3", "4", "5",
  "6", "9", "W", "X", and "Y"), with sampling probability proportionate
  to their prevalence in the "ipdiagnosis" table.

- `diagnosis_cluster`: "ipdiagnosis" table only. Proportionally sampled
  from values that have a prevalence of more than 1% in the
  "diagnosis_cluster" field of the "ipdiagnosis" table, which are ("",
  "A", "B").

- `diagnosis_prefix`: "ipdiagnosis" table only. Proportionally sampled
  from values that have a prevalence of more than 1% in the
  "diagnosis_prefix" field of the "ipdiagnosis" table, which are ("",
  "N", "Q", "6").

- `er_diagnosis_code`: "erdiagnosis" table only. Simulated ICD-10
  diagnosis codes. Each encounter can be associated with multiple
  diagnosis codes in long format.

- `er_diagnosis_type`: "erdiagnosis" table only. Proportionally sampled
  from values that have a prevalence of more than 1% in the
  "er_diagnosis_type" field of the "erdiagnosis" table, which are ("",
  "M", "9", "3", "O").

## Note

The following fields `(er)diagnosis_code`, `(er)diagnosis_type`,
`diagnosis_cluster`, `diagnosis_prefix` are simulated independently.
Therefore, the simulated combinations may not reflect the
interrelationships of these fields in actual data. For example, specific
diagnosis codes may be associated with specific diagnosis types,
diagnosis clusters, or diagnosis prefix in reality. However, these
relationships are not maintained for the purpose of generating dummy
data. Users require specific linkages between these fields should
consider customizing the output data or manually generating the desired
combinations.

## Examples

``` r
### Simulate a erdiagnosis table for 5 unique subjects with total 20 records:
if (FALSE) { # \dontrun{
set.seed(1)
erdiag <- dummy_diag(nid = 5, nrow = 20, ipdiagnosis = F)
} # }

### Simulate a ipdiagnosis table with diagnosis codes starting with "E11":
if (FALSE) { # \dontrun{
set.seed(1)
ipdiag <- dummy_diag(nid = 5, nrow = 20, ipdiagnosis = T, pattern = "^E11")
} # }

### Simulate a ipdiagnosis table with random diagnosis codes in diagnosis type 3 or 6 only:
if (FALSE) { # \dontrun{
set.seed(1)
ipdiag <- dummy_diag(nid = 5, nrow = 20, diagnosis_type = (c("3", "6"))) %>%
  filter(diagnosis_type != "M") # remove default rows with diagnosis_type="M" from each ID
} # }

### Simulate a ipdiagnosis table with ICD-10-CA codes:
if (FALSE) { # \dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "172.XX.XX.XXX",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)

set.seed(1)
ipdiag <- dummy_diag(nid = 5, nrow = 20, ipdiagnosis = T, dbcon = dbcon, source = "icd_lookup")
} # }
```
