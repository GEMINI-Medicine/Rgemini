# Identify relevant CCI codes for interventions of interest

The [Canadian Classification of Health Interventions (CCI)
codes](https://www.cihi.ca/sites/default/files/document/guide-for-users-of-cci-manual-en.pdf)
provide a detailed classification of all inpatient interventions in
Canada, with more than 17,000 unique, alphanumeric codes.

`cci_search()` faciliates the process of indentifying relevant CCI codes
for interventions of interest. For example, researchers may want to
identify all CCI codes related to diagnostic imaging performed on a
specific body region. The function enables this by breaking down all
existing CCI codes into their underlying components (see hierarchical
coding structure [here](https://www.cihi.ca/en/cci-coding-structure),
and providing a step-by-step filtering approach allowing researchers to
identify CCI codes of interest.

## Usage

``` r
cci_search(dbcon)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A `DBI` database connection to any GEMINI database. The function will
  query the `lookup_cci` table to identify all unique CCI codes, which
  will then be filtered for the relevant codes based on the step-by-step
  search criteria provided by the user via the terminal.

## Value

`data.table` This function returns a table containing the filtered CCI
codes matching the user's search criteria. For each row in the output
table, the following variables are returned:

- `intervention_code`: CCI code matching the search criteria

- `CCI_description`: description of the CCI code

## Details

CCI codes contain 5-10 alphanumeric characters defining the following
features:

1.  Section (1 character): Broad realm of interventions (e.g., 1 =
    "physical/physiological therapeutic interventions")

2.  Group (2 characters): Target region or area of focus. In Sections
    1-3, this refers to anatomy site (e.g., SC = "spinal vertebrae")
    while in others, it refers to stage of pregnancy (Section 5),
    mental/sensory function (Section 6), intervention type (Section 7),
    or target disease/organism (Section 8)

3.  Intervention (2 characters): Procedure/intervention types within a
    given Section (e.g., 75 = "fusion")

4.  Qualifier 1 (2 characters): Optional details defining how (or why)
    the intervention was performed. In Sections 3/4/7, Qualifier 1 is
    all that is required to complete the CCI code. In other Sections
    (e.g., 1), it represents only a part of the qualifier - the approach
    and technique portion (e.g., LL = "using open anterior approach").

5.  Qualifier 2 (2 characters): Optional details, such as the tools,
    agents or modalities used (e.g., KD = "fixation device")

6.  Qualifier 3 (1 character): Optional details defining the type of
    tissue (e.g., A = "autograft") or group/strain of an
    organism/antigen used during the intervention.

The `cci_search()` function provides a step-by-step search allowing
users to extract a subset of CCI codes matching the Section, Group(s)
Intervention(s), and/or Qualifier(s) that are of interest for a
particular study.

At each step, the user is shown available categories and their
corresponding descriptions, which can be searched and sorted in the
viewer pane. The user is then prompted to provide the entries they want
to include via the terminal. In some cases, broader drop-down menus are
shown allowing users to select multiple categories at once (e.g., all
interventions targeting the nervous system).

## Warning

This function can only search CCI codes that exist in the `lookup_cci`
table. Some older, deprecated CCI codes may not be present in that
table, and hence, can't be identified using `cci_search()`. We therefore
recommend using this function as a starting point only, and complement
it with an additional search (e.g., using regex) to make sure all
relevant codes have been identified.

Additionally, please note that CCI codes are structured in a nested
hierarchy, where the specific Groups, Interventions, and Qualifiers vary
across Sections (1st digit of CCI codes). The function therefore only
allows users to select a single Section during the first step of the
filtering process. If users want to search codes across different
Sections, they need to run this function multiple times - once per
Section.

## References

https://www.cihi.ca/sites/default/files/document/guide-for-users-of-cci-manual-en.pdf
https://www.cihi.ca/en/cci-coding-structure

## Examples

``` r
if (FALSE) { # \dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)
cci_filtered <- cci_search(dbcon)
} # }
```
