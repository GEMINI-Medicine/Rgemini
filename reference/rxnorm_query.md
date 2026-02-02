# RxNorm Pharmacy Data Mapping Function

Retrieve rows from GEMINI pharmacy data matching specified drug names or
drug classes.

## Usage

``` r
rxnorm_query(
  dbcon,
  drug_name = NULL,
  drug_class = NULL,
  cohort = NULL,
  return_unmatched = FALSE,
  detailed_search = TRUE,
  return_drug_list = FALSE
)
```

## Arguments

- dbcon:

  (`DBIConnection`) A database connection to a GEMINI database. Requires
  version drm_cleandb_v3_1_0 / H4H_v5 or newer. Older DBs lack `row_num`
  in the pharmacy table and are therefore incompatible with the RxNorm
  workflow.

- drug_name:

  (`character`) Name of drug to search for (e.g., "amoxicillin").
  Generic or brand name is accepted. Multiple drugs can be provided as a
  character vector. If empty, function expects `drug_class` input
  instead.

- drug_class:

  (`character`) Optional input: Can be used as an alternative to
  `drug_name` to search whole drug classes based on ATC code (e.g.,
  "J05" = Antivirals for systemic use), rather than searching for
  individual drug names.

- cohort:

  (`data.table` or `data.frame`) Optional input allowing users to
  specify a cohort of interest. Needs to include GEMINI encounter ID
  (`genc_id`). By default, the whole GEMINI pharmacy table will be
  included in the Rxnorm search.

- return_unmatched:

  (`logical`) If FALSE (default), the function will only return pharmacy
  entries that Rxnorm matched to the searched drugs. If TRUE, the
  function will additionally return all unmatched rows, allowing users
  to inspect pharmacy entries that Rxnorm did not match to the searched
  drug(s) of interest. The function will return a list with 2 items:

  1.  `matched_rows`: All the matched entries in long format (same
      output as if the argument return_unmatched is FALSE).

  2.  `unmatched_rows`: All unmatched pharmacy rows for a cohort of
      interest (in wide format).

- detailed_search:

  (`logical`) If TRUE (default), the function searches for every related
  concept to each selected drug. This may increase sensitivity, but can
  also result in false positive matches. The impact of
  `detailed_search = TRUE` may vary by drug group. In general, we
  recommend using the default option but users should carefully review
  the output for false positives. Users can run
  `detailed_search = FALSE` at their own discretion and we recommend
  carefully checking for potential misses when doing so.

- return_drug_list:

  (`logical`) Optional input allowing users to return the list of
  searched drugs, instead of running the search (e.g., when planning a
  search by ATC class).

## Value

By default, `rxnorm_query` returns a `data.table` containing the rows
from the pharmacy table that Rxnorm matched to the drug(s) of interest.
Entries are returned in long format for each `genc_id`, together with
the following additional columns:

- `search_type`: Pharmacy column where drug match was found

- `raw_input`: Corresponding entry in the matched row(s) of the pharmacy
  table

- `row_num`: Row ID of matched pharmacy entries

- `rxnorm_match`: Rxnorm output specifying which of the searched drug(s)
  the pharmacy entry was matched to.

When `return_unmatched` is TRUE, the 2nd list item of the output will be
a `data.table` with the unmatched rows in wide format, containing all
columns from the pharmacy table included in the Rxnorm search.

## Details

RxNorm is a standardized naming system and drug thesaurus provided by
the National Library of Medicine. The GEMINI-RxNorm system (Waters et
al., 2023) automates the use of RxNorm tools with other datasets to
identify drug concepts from pharmacy orders. The procedure matches
drug-identifying information from pharmacy data to RxNorm concept
identifiers. A user interface allows researchers to search for drugs,
returning the relevant original pharmacy data and predicted drugs
through matched RxNorm concepts. The GEMINI-RxNorm system significantly
reduces the time required over manual pharmacy validation. It can reduce
an estimated 30 seconds to 5 seconds per pharmacy row, and it can reduce
the total number of rows needed to be manually validated by up to 99.99%
(Waters et al., 2023).

Note: The returned matches from this function should be manually and
carefully validated by users. Please see the vignette for the
prepare_pharm_for_validation.R function for more details.

## References

Waters R, et al. JAMIA Open, 2023.
https://doi.org/10.1093/jamiaopen/ooad062

## Examples

``` r
if (FALSE) { # \dontrun{
# Connect to DB
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Username: "),
  password = getPass("Password: ")
)

# Run Rxnorm query
rxnorm_res <- rxnorm_query(
  dbcon = dbcon,
  drug_name = c("metformin", "insulin")
)
} # }
```
