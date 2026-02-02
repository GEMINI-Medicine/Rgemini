# Obtain commonly used neighbourhood-level socioeconomic status (SES) variables

The `neighbourhood_ses()` function derives neighbourhood-level SES
variables for a given encounter based on the dissemination area they
reside in. All variables returned by this function are based on
Statistics Canada Census data and the Ontario Marginalization Index
(ON-Marg; see below for details).

For database versions since `drm_cleandb_v3` / `H4H_template_v4` users
can choose between 2016 vs. 2021 census/ON-Marg data. For earlier
versions, only 2016 census data are available. Note that the names of
some output variables vary by census year.

## Usage

``` r
neighbourhood_ses(dbcon, cohort, census_year)

neighborhood_ses(dbcon, cohort, census_year)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database.

- cohort:

  (`data.table` \| `data.frame`)  
  Table with all relevant encounters of interest, where each row
  corresponds to a single encounter. Must contain GEMINI Encounter ID
  (`genc_id`).

- census_year:

  (`numeric` \| `character`)  
  Statistics Canada census year. Only 2016 or 2021 are valid inputs.

## Value

(`data.frame` \| `data.table`)  
This function returns a `data.table` where each row corresponds to a
`genc_id` from the user-provided cohort input, together with the
following columns:

- The user-provided census year: `census_year` (2016 or 2021)

- DA the encounter resides in (based on PCCF+): `da_uid`

- Neighbourhood-level income (continuous):

- `atippe` (neighbourhood after tax income per single person equivalent)

- `btippe` (neighbourhood before tax income per single person
  equivalent)

- Neighbourhood-level income (quintiles from PCCF+):

- `qnatippe` and `qnbtippe`: Quintiles of `atippe` and `btippe`
  calculated based on *national* income distribution

- `qaatippe` and `qabtippe`: Quintiles of `atippe` and `btippe`
  calculated based on distribution within a given community (based on
  census metropolitan area, census agglomeration, or residual area
  within each province).

- % visible minorities: `vismin_pct`

- % with immigrant status: `immsta_pct`

- % with post-secondary education:

- Including all respondents \> 15 years of age: `ed_15over_postsec_pct`

- Only including respondents between 25-64 years:
  `ed_25to64_postsec_pct`

- Ontario Marginalization Index (continuous):

- If `census_year` = 2021: `households_dwellings`, `material_resources`,
  `age_labourforce`, `racialized_NC_pop`

- If `census_year` = 2016: `instability`, `deprivation`, `dependency`,
  `ethniccon`

- Ontario Marginalization Index (quintiles):

- All ON-Marg variables are additionally returned as quintiles, as
  indicated by the suffix `_q` (e.g., `households_dwellings_q`)

## Statistics Canada Census

The Statistics Canada census is collected every 5 years and provides a
detailed statistical portrait of communities across Canada, including
information about income, education, ethnicity, and immigrant status.

Census data are collected by dissemination area (DA), which typically
covers a population of 400-700 people. To enable linkage between GEMINI
data and DA-level location, the DA of a given encounter was derived from
their postal code using the [Postal Code Conversion File Plus
(PCCF+)](https://library.carleton.ca/sites/default/files/2023-03/PCCF%2BUserguide-2021.pdf)
program.

The `neighbourhood_ses()` function currently returns the following
census variables:

- [**Income**](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=pop123):

- Statistics Canada sources information about household income from the
  Canadian Revenue Agency

- PCCF+ provides an income per person equivalent (IPPE) by adjusting
  household income by household size

- Both continuous income and national/community quintiles are returned

- [**Education**](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=pop038):

- Indicates a person's highest level of education: Based on the
  long-form census questionnaire, which is only administered to 25% of
  households

- The function returns the % of respondents with a post-secondary
  certificate, diploma, or degree

- [**Visible
  minorities**](https://www12.statcan.gc.ca/census-recensement/2021/ref/98-500/006/98-500-x2021006-eng.cfm):

- Indicates whether a person identifies as a visible minority, defined
  as follows by the Employment Equity Act: “persons, other than
  Aboriginal peoples, who are non-Caucasian in race or non-white in
  colour” (e.g., Black, South Asian, Chinese, Latin American etc.)

- Based on the long-form census questionnaire, which is only
  administered to 25% of households

- [**Immigrant
  status**](https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=pop148):

- Indicates whether a person is, or has ever been, a landed immigrant or
  permanent resident in Canada. This indludes those who have obtained
  Canadian citizenship by naturalization.

- In 2021 census: Sourced from Immigration, Refugees and Citizenship
  Canada

- In 2016 census: Based on the long-form questionnaire (25% of
  households)

## Ontario Marginalization Index (On-Marg)

On-Marg is a neighbourhood-level index measuring marginalization
differences between areas based on a subset of variables from the
Statistics Canada census.

The index was derived from a principal component factor analysis on 42
variables, which resulted in 18 indicators along the following 4
dimensions:

1.  Households and dwellings: Measures housing density and
    characteristics of family structure (e.g., living alone, % dwellings
    not owned)

2.  Material resources: Measures access to basic material needs (e.g.,
    housing, food, and clothing), education, and employment

3.  Age and labour force: Includes indicators such as the % of seniors
    (65+), children, and those that are not part of the labour force

4.  Racialized and newcomer populations: Measures the % of people who
    are recent immigrants (within last 5 years) or identify as a visible
    minority

In the 2016 version of ON-Marg, the 4 dimensions were called
"Residential instability", "Material deprivation", "Dependency", and
"Ethnic concentration" respectively. The dimensions were renamed in 2021
to avoid deficit-based language and better reflect the census measures
associated with each dimension.

All ON-Marg variables are available as continuous factor scores as well
as quintiles. Higher scores represent a higher degree of marginalization
(i.e., Q1 = least marginalized, Q5 = most marginalized). For continuous
scores, negative (positive) values indicate that the observation falls
below (above) the average level of the factor.

## Missing values

Some encounters could not be linked to Statistics Canada data due to
missing/ invalid postal codes, or due to the fact that they reside in an
area not covered by the census. These encounters will be returned with
`da_uid = NA`.

Additionally, Statistics Canada suppresses results from certain DAs due
to low response rates or data quality issues. The corresponding
census/ON-Marg variables will be returned as `NA` for all `genc_ids` in
those DAs.

## References

- **Statistics Canada Census**

- 2021 Census:
  https://www12.statcan.gc.ca/census-recensement/2021/ref/index-eng.cfm

- 2016 Census:
  https://www12.statcan.gc.ca/census-recensement/2016/ref/index-eng.cfm

- **Ontario Maginalization Index**

- ON-Marg 2021:
  https://www.publichealthontario.ca/-/media/documents/o/2017/on-marg-userguide.pdf

- ON-Marg 2016:
  https://www.publichealthontario.ca/-/media/documents/U/2018/userguide-on-marg.pdf

- Additional information from Public Health Ontario:
  https://www.publichealthontario.ca/-/media/Event-Presentations/2023/09/ontario-marginalization-index-updates-products.pdf?rev=07baae2569164c17abaa18464075aa20&sc_lang=en

- **PCCF+**

- PCCF+ Reference Guide:
  https://library.carleton.ca/sites/default/files/2023-03/PCCF%2BUserguide-2021.pdf

- Measuring Health Inequalities - A Toolkit:
  https://www.cihi.ca/sites/default/files/document/toolkit-area-level-measurement-pccf-en.pdf

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

cohort <- dbGetQuery(dbcon, "SELECT genc_id from admdad LIMIT 100;")

neighbourhood_ses_table <- neighbourhood_ses(dbcon, cohort, 2021)
} # }
```
