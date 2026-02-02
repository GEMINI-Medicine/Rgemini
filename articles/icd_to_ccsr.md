# ICD to CCSR

## Overview & background

The `icd_to_ccsr` function returns the CCSR ([Clinical Classifications
Software
Refined](https://hcup-us.ahrq.gov/toolssoftware/ccsr/ccs_refined.jsp))
categories for a user-specified set of diagnosis codes.

CCSR provides a grouping of individual ICD-10 diagnosis codes into
broader, clinically meaningful disease categories. The original CCSR
grouping was developed based on US ICD-10-CM codes. GEMINI developed an
algorithm mapping Canadian diagnosis codes (ICD-10-CA) to CCSR
categories. The full mapping procedure is described
[here](https://www.medrxiv.org/content/10.1101/2022.11.29.22282888v1).

The purpose of this vignette is to provide a practical guide on how to
apply the `icd_to_ccsr` function in different research contexts, and how
to further analyze the outputs provided by the function. For more
details on the inner workings on the function itself, please refer to
the function documentation
([`?icd_to_ccsr`](https://gemini-medicine.github.io/Rgemini/reference/icd_to_ccsr.md)).

## When and why to use CCSR

There are currently more than 70,000 unique ICD-10 codes, with
additional country-specific variations. ICD-10 codes contain
fine-grained diagnosis details that may not be relevant for research
purposes. CCSR aggregates individual diagnosis codes into ~540
clinically relevant categories across 22 body systems. A full list of
current CCSR categories can be found
[here](https://docs.google.com/spreadsheets/d/1hxcXqunqgmsJXyFshW30y17hKfAFh3SFbBpCqBPcDNY/edit?usp=sharing).

CCSR facilitates disease-specific healthcare utilization and outcome as
well as dimensionality reduction in statistical modeling. CCSR
categories are particularly useful when analyzing large, heterogeneous
cohorts (e.g., GIM patients). In those cases, grouping patients into
broad disease categories can provide important insights into the patient
case mix and allows for a meaningful comparison of baseline
characteristics and clinical outcomes across different patient
sub-populations (see [*Example applications of CCSR*](#examples)) .

**When not to use CCSR:**

- For research studies that focus on a single, well-defined condition
  (e.g., COVID-19), it is often better to simply filter the cohort by
  the specific ICD-10-CA code(s) of interest (e.g., U071 = confirmed
  COVID-19) instead of using broader disease categories that might
  result in a loss of information and specificity.
- Additionally, some conditions do not have their own CCSR category. For
  example, there is no CCSR category for delirium. Instead, any
  diagnosis codes associated with delirium are grouped into CCSR
  category NVS011 = “Neurocognitive disorders”, together with a large
  range of other neurocognitive disorders.

The decision on whether to use ICD-10-CA codes vs. CCSR categories
should be made on a project-by-project basis and researchers need to
carefully consider what approach is most appropriate for their analyses.

## Running `icd_to_ccsr()` with default arguments

To run the `icd_to_ccsr` function, users need to provide 1) a valid
database connection and 2) a `dxtable` input containing all diagnosis
codes of interest. Typically, `dxtable` is created based on the
`ipdiagnosis` table (or a subset thereof) in the GEMINI database, which
contains all diagnosis codes assigned to each `genc_id`.

Here is an example of how to load the relevant data and run the
`icd_to_ccsr` function with default settings:

``` r
# Load required libraries
library(RPostgreSQL)
library(DBI)
library(getPass)

# Establish database connection
db <- DBI::dbConnect(drv,
  dbname = "DB_name",
  host = "172.XX.XX.XXX",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("Enter Password:")
)

# query relevant columns of ipdiagnosis table
ipdiagnosis <- dbGetQuery(db, "SELECT genc_id, diagnosis_code, diagnosis_type FROM ipdiagnosis;")

# Run default version of icd_to_ccsr
ipdiagnosis_ccsr <- icd_to_ccsr(db, dxtable = ipdiagnosis)

head(ipdiagnosis_ccsr, 10)
```

> **Mock output table of
> [`icd_to_ccsr()`](https://gemini-medicine.github.io/Rgemini/reference/icd_to_ccsr.md):**

| genc_id | diagnosis_type | diagnosis_code |                             diagnosis_code_desc                              | ccsr_default |                    ccsr_default_desc                     | ccsr_1 | ccsr_2 | ccsr_3 | ccsr_4 | ccsr_5 | ccsr_6 |
|:-------:|:--------------:|:--------------:|:----------------------------------------------------------------------------:|:------------:|:--------------------------------------------------------:|:------:|:------:|:------:|:------:|:------:|:------:|
|    1    |       M        |      I500      |                           Congestive heart failure                           |    CIR019    |                      Heart failure                       | CIR019 |        |        |        |        |        |
|    2    |       M        |      J159      |                       Bacterial pneumonia, unspecified                       |    RSP002    |      Pneumonia (except that caused by tuberculosis)      | INF003 | RSP002 |        |        |        |        |
|    3    |       6        |      F009      |                 Dementia in Alzheimer’s disease, unspecified                 |    NVS011    |                 Neurocognitive disorders                 | NVS011 |        |        |        |        |        |
|    4    |       M        |      N390      |                 Urinary tract infection, site not specified                  |    GEN004    |                 Urinary tract infections                 | GEN004 |        |        |        |        |        |
|    5    |       M        |      I214      |                  Acute subendocardial myocardial infarction                  |    CIR009    |               Acute myocardial infarction                | CIR009 |        |        |        |        |        |
|    6    |       M        |      J440      | Chronic obstructive pulmonary disease with acute lower respiratory infection |    RSP008    | Chronic obstructive pulmonary disease and bronchiectasis | RSP008 |        |        |        |        |        |
|    7    |       M        |      I634      |           Cerebral infarction due to embolism of cerebral arteries           |    CIR020    |                   Cerebral infarction                    | CIR020 |        |        |        |        |        |
|    8    |       M        |      N179      |                       Acute renal failure, unspecified                       |    GEN002    |           Acute and unspecified renal failure            | GEN002 |        |        |        |        |        |
|    9    |       M        |     I2510      |           Atherosclerotic heart disease of native coronary artery            |    CIR011    |     Coronary atherosclerosis and other heart disease     | CIR011 |        |        |        |        |        |
|   10    |       M        |     A4150      |                  Sepsis due to Escherichia coli \[E.coli\]                   |    INF002    |                        Septicemia                        | INF002 | INF003 |        |        |        |        |

The function returns the GEMINI-derived ICD-to-CCSR mappings. Each row
in the output corresponds to a single ICD-10-CA code (`diagnosis_code`)
of a given encounter (`genc_id`). Each diagnosis code is returned with
its description (`diagnosis_code_desc`) and one or multiple (up to 6)
CCSR categories (`ccsr_1` to `ccsr_6`). One CCSR category has been
assigned as the default category (`ccsr_default`), which corresponds to
the main disease group and is typically the category of interest for
further analyses (see [*Example applications of CCSR*](#examples)). The
output also contains the description of the CCSR default category
(`ccsr_default_desc`).

***Note: Although the GEMINI-derived CCSR mappings have been validated
by a medical expert, users are advised to spot-check the output to
verify that the ICD-10-CA descriptions broadly match the CCSR default
descriptions. Please inform the GEMINI team in case you identify any
inconsistencies. Moreover, please note that the mappings may be subject
to change as we continuously update our database to ensure consistency
with the official CCSR tool.***

## Optional input arguments

### `type_mrdx`

**default: `TRUE`**

As illustrated in the example output table above, the function by
default only returns the most responsible diagnosis (MRDx) code per
`genc_id`. That is, only a single row per encounter is returned, which
contains the type-M (or type-6 if present) diagnosis (in line with the
[CIHI definition of
MRDx](https://www.cihi.ca/sites/default/files/document/hospital-standardized-mortality-ratio-meth-notes-en.pdf);
see page 13). If users want to obtain CCSR categories for any diagnosis
types, the input argument `type_mrdx` can be set to `FALSE`, as follows:

``` r
ipdiagnosis_ccsr <- icd_to_ccsr(db, ipdiagnosis, `type_mrdx` = FALSE)
```

In that case, the function will not apply any filtering by diagnosis
type, and instead, will return all rows from the original input table.
Note that this output contains multiple rows per `genc_id`,
corresponding to the different diagnosis types. For more information on
diagnosis type coding, see
[here](https://www.cihi.ca/sites/default/files/document/diagnosis-type-definitions-en.pdf).

> **Mock output table of
> [`icd_to_ccsr()`](https://gemini-medicine.github.io/Rgemini/reference/icd_to_ccsr.md)
> with `type_mrdx = FALSE`:**

| genc_id | diagnosis_type | diagnosis_code |                     diagnosis_code_desc                     | ccsr_default |                    ccsr_default_desc                     | ccsr_1 | ccsr_2 | ccsr_3 | ccsr_4 | ccsr_5 | ccsr_6 |
|:-------:|:--------------:|:--------------:|:-----------------------------------------------------------:|:------------:|:--------------------------------------------------------:|:------:|:------:|:------:|:------:|:------:|:------:|
|    1    |       1        |      F067      |                   Mild cognitive disorder                   |    MBD013    | Miscellaneous mental and behavioral disorders/conditions | MBD013 |        |        |        |        |        |
|    1    |       3        |     I4890      |              Atrial fibrillation, unspecified               |    CIR017    |                   Cardiac dysrhythmias                   | CIR017 |        |        |        |        |        |
|    1    |       M        |      I500      |                  Congestive heart failure                   |    CIR019    |                      Heart failure                       | CIR019 |        |        |        |        |        |
|    2    |       1        |      C793      | Secondary malignant neoplasm of brain and cerebral meninges |    NEO070    |                  Secondary malignancies                  | NEO070 |        |        |        |        |        |
|    2    |       3        |     G4731      |                    Sleep apnoea, central                    |    NVS016    |                   Sleep wake disorders                   | NVS016 |        |        |        |        |        |
|    2    |       M        |      J159      |              Bacterial pneumonia, unspecified               |    RSP002    |      Pneumonia (except that caused by tuberculosis)      | INF003 | RSP002 |        |        |        |        |
|    3    |       1        |      Z951      |           Presence of aortocoronary bypass graft            |    CIR011    |     Coronary atherosclerosis and other heart disease     | CIR011 | FAC009 |        |        |        |        |
|    3    |       6        |      F009      |        Dementia in Alzheimer’s disease, unspecified         |    NVS011    |                 Neurocognitive disorders                 | NVS011 |        |        |        |        |        |
|    4    |       1        |     E1042      |     Type 1 diabetes mellitus with autonomic neuropathy      |    END003    |           Diabetes mellitus with complication            | END003 | END004 | NVS015 |        |        |        |
|    4    |       M        |      N390      |         Urinary tract infection, site not specified         |    GEN004    |                 Urinary tract infections                 | GEN004 |        |        |        |        |        |

### `unique_mrdx`

**default: `FALSE`**

Typically, each encounter should have exactly one MRDx code. However, in
certain circumstances, the function may identify encounters with
multiple MRDx codes (e.g., two type-M diagnosis codes). This could be
due to data quality issues, or it can happen if users combine in-patient
and ED diagnoses or create a long-table format when pre-processing their
data. By default, the function will show a warning if any encounters
with multiple MRDx codes are found, but it will run as usual and return
multiple rows for any encounters with more than 1 MRDx. If users want
the requirement for a unique MRDx to be strict, they can specify
`unique_mrdx = TRUE`, which will cause the function to terminate if any
encounters with multiple MRDx codes are found. In this case, users are
advised to carefully check their input table and remove any entries
causing the error message. Note that this input argument is only
relevant if `type_mrdx` is set to `TRUE`, otherwise, it will be ignored.

### `replace_invalidpdx`

**default = `TRUE`**

Some diagnosis codes will be returned with `ccsr_default = "XXX000"`,
which indicates that the diagnosis code “is unacceptable as a principal
diagnosis” (invalid PDX) according to US diagnostic coding following
Medicare Code Edits guidelines (see
[here](https://hcup-us.ahrq.gov/toolssoftware/ccsr/DXCCSR-User-Guide-v2023-1.pdf)
for more details). Since the CCSR default category is meant to reflect
the main disease category (~MRDx), the official CCSR tool sets
`ccsr_default` to `"XXX000"` for any codes that are not valid as a
PDX/MRDx.

Many of the external cause diagnosis codes (`diagnosis_type = 9`) fall
into that category as they are not meant to be coded as MRDx, and thus,
will be returned with `ccsr_default = "XXX000"`. Note that these codes
still have valid `ccsr_1` (to `ccsr_6`) categories:

> **Example codes with `ccsr_default = "XXX000"`:**

| genc_id | diagnosis_type | diagnosis_code |                                                                            diagnosis_code_desc                                                                             | ccsr_default |                                      ccsr_default_desc                                       | ccsr_1 | ccsr_2 | ccsr_3 | ccsr_4 | ccsr_5 | ccsr_6 |
|:-------:|:--------------:|:--------------:|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------:|:------------:|:--------------------------------------------------------------------------------------------:|:------:|:------:|:------:|:------:|:------:|:------:|
|   11    |       9        |      V180      |                                           Pedal cyclist injured in noncollision transport accident, driver, nontraffic accident                                            |    XXX000    | Code is unacceptable as a principal diagnosis PDX (only used for the inpatient default CCSR) | EXT008 | EXT020 |        |        |        |        |
|   12    |       9        |      W01       |                                                          Fall on same level from slipping, tripping and stumbling                                                          |    XXX000    | Code is unacceptable as a principal diagnosis PDX (only used for the inpatient default CCSR) | EXT003 | EXT020 |        |        |        |        |
|   13    |       9        |      Y04       |                                                                          Assault by bodily force                                                                           |    XXX000    | Code is unacceptable as a principal diagnosis PDX (only used for the inpatient default CCSR) | EXT016 | EXT022 |        |        |        |        |
|   14    |       9        |      Y832      | Surgical operation with anastomosis, bypass or graft as the cause of abnormal reaction or later complication, without mention of misadventure at the time of the procedure |    XXX000    | Code is unacceptable as a principal diagnosis PDX (only used for the inpatient default CCSR) | EXT025 |        |        |        |        |        |
|   15    |       9        |      Y848      |               Other medical procedures as the cause of abnormal reaction or later complication, without mention of misadventure at the time of the procedure               |    XXX000    | Code is unacceptable as a principal diagnosis PDX (only used for the inpatient default CCSR) | EXT025 |        |        |        |        |        |

When `type_mrdx = TRUE`, invalid PDX categories are typically rare.
However, due to country-specific differences in coding and mapping
ambiguity for some ICD-10-CA codes, users may find that even some type
M/6 (MRDx) codes are returned with CCSR default category `"XXX000"`.
This usually only affects a few injury/poisoning codes or codes starting
with “Z”, including pregnancy-related, procedural, or health
status/personal history codes:

> **Example MRDx codes with `ccsr_default = "XXX000"`:**

| genc_id | diagnosis_type | diagnosis_code |                                  diagnosis_code_desc                                  | ccsr_default |                                      ccsr_default_desc                                       | ccsr_1 | ccsr_2 | ccsr_3 | ccsr_4 | ccsr_5 | ccsr_6 |
|:-------:|:--------------:|:--------------:|:-------------------------------------------------------------------------------------:|:------------:|:--------------------------------------------------------------------------------------------:|:------:|:------:|:------:|:------:|:------:|:------:|
|   16    |       M        |     Z37000     | Single live birth, pregnancy resulting from both spontaneous ovulation and conception |    XXX000    | Code is unacceptable as a principal diagnosis PDX (only used for the inpatient default CCSR) | PRG030 |        |        |        |        |        |
|   17    |       M        |      Z588      |                    Other problems related to physical environment                     |    XXX000    | Code is unacceptable as a principal diagnosis PDX (only used for the inpatient default CCSR) | EXT011 |        |        |        |        |        |

The invalid PDX category is in line with coding for insurance purposes,
however, it typically is not meaningful in research contexts and can
result in a loss of information. For example, in the scenario shown
above, users may still want to group the patient with
`diagnosis_code = "Z37000"` into a relevant, pregnancy-related CCSR
category. In fact, the code has been assigned a CCSR 1 category of
`"PRG030"` (“Maternal outcome of delivery”), which contains useful
information about the encounter.

The function therefore provides an optional input argument
`replace_invalidpdx` to overwrite any `"XXX000"` values with one of the
(valid) CCSR 1-6 categories. In fact, `replace_invalidpdx` is set to
`TRUE` by default given that for most research projects, the distinction
between valid vs. invalid principle diagnosis coding may not be relevant
(or if it is, should be determined based on `diagnosis_type` instead of
mapped CCSR category). Additionally, users may specifically be
interested in CCSR groups for **any** diagnosis types
(`type_mrdx = FALSE`), in which case the definition of CCSR default
categories as valid vs. invalid PDX categories is not meaningful.

As an example, when `replace_invalidpdx` is set to `TRUE` (default),
`"XXX000"` is replaced with `"PRG030"` for diagnosis code `"Z37000"`:

> **Example output illustrating `replace_invalidpdx = TRUE`:**

| genc_id | diagnosis_type | diagnosis_code |                                  diagnosis_code_desc                                  | ccsr_default |      ccsr_default_desc       | ccsr_1 | ccsr_2 | ccsr_3 | ccsr_4 | ccsr_5 | ccsr_6 |
|:-------:|:--------------:|:--------------:|:-------------------------------------------------------------------------------------:|:------------:|:----------------------------:|:------:|:------:|:------:|:------:|:------:|:------:|
|   16    |       M        |     Z37000     | Single live birth, pregnancy resulting from both spontaneous ovulation and conception |    PRG030    | Maternal outcome of delivery | PRG030 |        |        |        |        |        |

***Note:*** Invalid PDX CCSR categories are replaced as follows: For any
codes that have only been mapped to a single CCSR category, `"XXX000"`
will be replace with the `ccsr_1` category. For any codes that have been
mapped to multiple CCSR categories, the function chooses the CCSR 1-6
category that is the most common default category among diagnosis codes
of the same ICD-10 chapter (same first 3 characters). Otherwise, if this
would still result in mapping to `"XXX000"` (e.g., if all codes of the
same chapter have been mapped to `"XXX000"`), the function defaults to
`ccsr_1`.

## Missing vs. unmapped diagnosis codes

Some entries in the output table might have missing CCSR default
categories (i.e., `is.na(ccsr_default)`). This can be due to either 1)
missing/empty diagnosis codes or 2) diagnosis codes that have not been
mapped to any CCSR categories. Users can distinguish between the two
scenarios based on `ccsr_default_descr`, which will be either 1)
`"Missing diagnosis code"` or 2) `"Unmapped"`.

### Missing diagnosis codes

Diagnosis codes that are returned with
`ccsr_default_desc = "Missing diagnosis code"` refer to entries in the
`dxtable` input where the `diagnosis_code` was `NA` or `NULL`, for
example:

> **Mock output table showing missing diagnosis codes:**

| genc_id | diagnosis_type | diagnosis_code | diagnosis_code_desc | ccsr_default |   ccsr_default_desc    | ccsr_1 | ccsr_2 | ccsr_3 | ccsr_4 | ccsr_5 | ccsr_6 |
|:-------:|:--------------:|:--------------:|:-------------------:|:------------:|:----------------------:|:------:|:------:|:------:|:------:|:------:|:------:|
|   18    |       M        |       NA       |         NA          |      NA      | Missing diagnosis code |        |        |        |        |        |        |
|   19    |       3        |       NA       |         NA          |      NA      | Missing diagnosis code |        |        |        |        |        |        |
|   20    |       1        |       NA       |         NA          |      NA      | Missing diagnosis code |        |        |        |        |        |        |

This is typically due to data quality issues and should only affect a
very small number of encounters.

If `type_mrdx` is set to `TRUE`, encounters that do not have any rows
containing a type-6 or type-M diagnosis will also be returned with
`ccsr_default = "Missing diagnosis code"` (and `diagnosis_code = NA`).
This is to ensure that the number of rows in the output table
corresponds to the number of unique encounters in the provided `dxtable`
input. The function will additionally show a warning message to inform
users if any encounters with missing MRDx codes have been found.

### Unmapped diagnosis codes

For rows where `ccsr_default = "Unmapped"`, a valid ICD-10-CA diagnosis
code exists, however, that code has not been mapped to any CCSR
categories (yet). For example:

> **Mock output table showing unmapped diagnosis codes:**

| genc_id | diagnosis_type | diagnosis_code |                              diagnosis_code_desc                               | ccsr_default | ccsr_default_desc | ccsr_1 | ccsr_2 | ccsr_3 | ccsr_4 | ccsr_5 | ccsr_6 |
|:-------:|:--------------:|:--------------:|:------------------------------------------------------------------------------:|:------------:|:-----------------:|:------:|:------:|:------:|:------:|:------:|:------:|
|   21    |       4        |     98633      |                                                                                |              |     Unmapped      |        |        |        |        |        |        |
|   22    |       1        |      U85       |                       Resistance to antineoplastic drugs                       |              |     Unmapped      |        |        |        |        |        |        |
|   23    |       9        |      Y466      | Other and unspecified antiepileptics causing adverse effect in therapeutic use |              |     Unmapped      |        |        |        |        |        |        |
|   24    |       1        |      T062      |               Injuries of nerves involving multiple body regions               |              |     Unmapped      |        |        |        |        |        |        |
|   25    |       1        |      D302      |                           Benign neoplasm of ureter                            |              |     Unmapped      |        |        |        |        |        |        |
|   26    |       3        |      U075      |                          Personal history of COVID-19                          |              |     Unmapped      |        |        |        |        |        |        |
|   27    |       3        |     E1330      |         Other specified diabetes mellitus with background retinopathy          |              |     Unmapped      |        |        |        |        |        |        |

Type-4 morphology codes (e.g., row 1 in example output above) will
always be returned as `"Unmapped"` since they are numeric codes based on
ICD-0 (oncology) diagnoses, which are not meant to be mapped to CCSR
categories.

For any other diagnosis types, codes that are returned as `"Unmapped"`
have simply not been mapped by the GEMINI team yet. We constantly update
our database to add new ICD-10-CA codes and strive to provide mappings
for as many codes as possible. However, our team prioritizes mapping
MRDx diagnosis codes. Therefore, unmapped codes are more likely to occur
when users run the
[`icd_to_ccsr()`](https://gemini-medicine.github.io/Rgemini/reference/icd_to_ccsr.md)
function with `type_mrdx = FALSE`. If users plan to analyse CCSR
categories for non-MRDx codes and find that a high percentage of codes
is unmapped, we recommend using our publicly available [GEMINI CCSR
mapping algorithm](https://github.com/GEMINI-Medicine/gemini-ccsr),
which enables (semi-)automatic ICD-to-CCSR mapping. For more details on
how to use the algorithm, please get in touch with the GEMINI team.

## Example applications of CCSR

The examples below illustrate a few common applications of CCSR in
clinical research.

### Patient case mix

Analyzing the frequency of each CCSR default category in large,
heterogeneous cohorts can provide important insights into the patient
case mix. For example, when running the
[`icd_to_ccsr()`](https://gemini-medicine.github.io/Rgemini/reference/icd_to_ccsr.md)
function on MRDx codes of a typical GIM cohort, the 5 most common CCSR
default categories among GIM patients can be obtained as follows:

``` r
library(data.table)

ccsr_summary <- ipdiagnosis_ccsr[, .(N = .N, `% patients` = .N / nrow(ipdiagnosis_ccsr) * 100),
  by = c("ccsr_default", "ccsr_default_desc")
]

head(ccsr_summary[order(N, decreasing = TRUE)], 5)
```

> **Mock output table showing top 5 most frequent CCSR default
> categories in a GIM cohort:**

| ccsr_default |                    ccsr_default_desc                     |  N   | % patients |
|:------------:|:--------------------------------------------------------:|:----:|:----------:|
|    CIR019    |                      Heart failure                       | 4775 |    4.98    |
|    RSP002    |      Pneumonia (except that caused by tuberculosis)      | 4007 |    4.23    |
|    RSP008    | Chronic obstructive pulmonary disease and bronchiectasis | 3760 |    3.85    |
|    GEN004    |                 Urinary tract infections                 | 3561 |    3.51    |
|    NVS011    |                 Neurocognitive disorders                 | 2981 |    3.23    |

Users could additionally analyse the cumulative percentage of patients
across CCSR categories to determine the number of unique disease
categories accounting for 50% (or 75%/90%…) of patients. This provides a
useful quantitative measure of the breadth of conditions managed at a
given hospital/medical ward.

### Risk adjustment using CCSR

When comparing clinical outcomes (e.g., mortality or readmission rates)
across patients/physicians/hospitals, it is important to consider
patients’ baseline characteristics, such as diagnosis group. Risk
adjustment could be performed by including CCSR categories as predictors
in regression models, or splitting cohorts into different subgroups
based on patients’ CCSR default category. Risk adjustment by CCSR could
also be used when analyzing quality of care, resource usage, healthcare
costs or other indicators.

As an example, see [Roberts et
al. (2023)](https://link.springer.com/article/10.1007/s11606-023-08245-w)
performing risk adjustment using CCSR in GEMINI data.

### Filtering cohorts by CCSR category

If users want to filter their cohort by a certain diagnosis group, they
could choose the relevant CCSR category(s) from [this
list](https://docs.google.com/spreadsheets/d/1hxcXqunqgmsJXyFshW30y17hKfAFh3SFbBpCqBPcDNY/edit?usp=sharing)
and then filter by the corresponding CCSR category number(s).

For example, to filter for any `genc_ids` with Septicemia, the relevant
CCSR category is INF002. Users have 2 options:

1.  Filter encounters where INF002 is the **CCSR default category**:

``` r
# option 1: filter by CCSR default category only
ipdiagnosis_ccsr <- ipdiagnosis_ccsr[ccsr_default == "INF002", ]
```

2.  Filter encounters where INF002 is among **any of the CCSR 1-6
    categories (recommended)**:

``` r
# option 2: filter by CCSR 1-6 (includes default category)
ipdiagnosis_ccsr <- ipdiagnosis_ccsr[
  rowSums(ipdiagnosis_ccsr[, c("ccsr_1", "ccsr_2", "ccsr_3", "ccsr_4", "ccsr_5", "ccsr_6")] == "INF002",
    na.rm = TRUE
  ) > 0,
]
```

The difference between the two approaches is that the CCSR default
category is meant as the main disease category. Focusing on that single
category is usually only relevant if users need to ensure that each
diagnosis code has exactly one corresponding CCSR category. For example,
in the analyses in the previous section, patients are grouped according
to their default CCSR category because we don’t want to double-count
patients whose MRDx code has been mapped to more than 1 CCSR category.
By contrast, when filtering for patients who fit a certain CCSR
criterion, a 1:1 mapping between diagnosis code and CCSR categories is
usually not required. Instead, we likely want to capture all patients
who have the CCSR category of interest among *any* of the mapped CCSR
1-6 categories.

In addition to that, the official CCSR tool assigns default categories
based on hierarchical rules, i.e., some CCSR categories are less likely
to be a default category than others. For example, for diagnosis codes
related to diabetes, the distinction between diabetes with vs. without
complications (CCSR END003 vs. END002) takes priority over the
distinction between type 1 vs. type 2 diabetes (CCSR END004 vs. END005).
As a result, if researchers want to filter for encounters with type-1
diabetes, searching by CCSR default category alone would result in many
relevant patients being missed because most diabetes patients have CCSR
default categories END002 or END003 (instead of END004/END005).
Therefore, the general recommendation when filtering cohorts by CCSR
category is to use all CCSR 1-6 categories (option 2 shown above),
instead of filtering by CCSR default alone.

**Additional notes on filtering by CCSR category:**

- When investigating specific, well-defined diseases, it may be more
  appropriate to filter by individual ICD-10-CA codes instead of relying
  on broad CCSR categories.
- If filtering by CCSR category is appropriate, users should still
  verify that the individual ICD-10-CA codes that are included are
  relevant for the research question.
- It is recommended to always filter by CCSR category number
  (`"INF002"`) instead of the description (`"Septicemia"`) as the
  category descriptions are subject to small changes with newer releases
  of the official CCSR tool.

## Additional considerations

### Initial vs. subsequent encounters

For some diagnoses (e.g., injuries or poisoning), the US ICD-10 codes
contain a suffix that differentiates between initial vs. subsequent
encounters. As a result, some CCSR categories make that same
distinction. For example: INJ018 = “Crushing injury, initial encounter”,
whereas INJ055 = “Crushing injury, subsequent encounter”. However,
Canadian ICD-10 codes do not differentiate between initial
vs. subsequent encounters. Therefore, ICD-10-CA codes were mapped to
“initial encounter” by default. If researchers are interested in
analyzing patients’ medical history, they **should not rely on the CCSR
description to differentiate between initial vs. subsequent
encounters**, and instead, should look at the encounter history of a
given patient within the database. More generally, it means that any
CCSR description of “initial” or “subsequent” encounter in the GEMINI
database should be ignored (i.e., INJ018 = INJ055 = “Crushing injury”).
