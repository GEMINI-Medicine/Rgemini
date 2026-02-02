# Cohort creation

This function creates a cohort data table based on user-specified
inclusion/ exclusion criteria. It also returns a table showing the
cohort size (e.g., number of encounters) that were included/excluded at
each step of the cohort creation.

## Usage

``` r
cohort_creation(
  cohort,
  labels,
  exclusion_flag = NULL,
  show_prct = TRUE,
  group_var = NULL,
  ...
)
```

## Arguments

- cohort:

  (`list`) A list where each item corresponds to a filtered
  `data.table`/`data.frame` object that contains the cohort at a given
  step of the cohort inclusions/exclusions. The function will
  automatically combine the inclusion/exclusion steps in a sequential
  manner, and will then count the number of entries that remain after
  each criterion. For example, if you have a `data.table` object called
  `data`: To obtain a cohort of encounters that are female and older
  than 65 years, you can use:
  `cohort = list(data[gender == "F"], data[age > 65])`. In this case,
  the returned cohort inclusion/exclusion table will contain 2 rows
  listing the number of encounters that are 1) female, and 2) female
  **AND** older than 65 years (= final cohort). Note that if `data` is a
  `data.frame`, you will need to filter the relevant rows as follows:
  `cohort = list(data[data\$gender == "F", ], data[data\$age > 65, ])`

- labels:

  (`character`) Vector containing a description for each
  inclusion/exclusion step (needs to be in the same order as
  corresponding list items in `cohort` input).

- exclusion_flag:

  (`logical`) A vector indicating whether a given cohort creation step
  should be interpreted as an exclusion (rather than an inclusion). If
  `TRUE` the corresponding entries will be removed and the number (%) of
  rows that were removed (rather than included) will be shown.

  By default, all cohort steps will be interpreted as inclusion steps.

- show_prct:

  (`logical`) Flag indicating whether to show percentage values (default
  = `TRUE`). If `FALSE`, only raw counts will be shown. Note that the
  percentages always reflect the % change relative to the N in the
  *previous* inclusion/exclusion step.

- group_var:

  (`character`) Optional: Name of a grouping variable (e.g., hospital).
  If provided, cohort numbers will be stratified by each level of the
  grouping variable (in addition to overall cohort numbers).

- ...:

  Additional parameters that will be passed to `prettyNum` for
  additional formatting of numbers (e.g., `big.mark = ","`).

## Value

A list with 2 items:

1.  `cohort_data`: `data.table` containing all entries in the final
    cohort (after applying all inclusions/exclusions)

2.  `cohort_steps`: `data.table` showing the number (and %) of entries
    that were included/excluded at each step of the cohort creation.

## Examples

``` r
# create dummy data
my_data <- Rgemini::dummy_ipadmdad(10000, n_hospitals = 5)

# convert to data.table for easy filtering
my_data <- data.table::setDT(my_data)

# run cohort_creation
my_cohort <- cohort_creation(
  cohort = list(
    my_data,
    my_data[gender == "F"],
    my_data[age > 65],
    my_data[grepl("^7", discharge_disposition)]
  ),
  labels = c(
    "All GEMINI encounters",
    "Gender = Female",
    "Age > 65",
    "In-hospital death"
  ),
  exclusion_flag = c(FALSE, FALSE, FALSE, TRUE),
  group_var = "hospital_num" # optional: stratify by hospital
)

# get data table containing all entries in final cohort
cohort_data <- my_cohort[[1]]

# print table with N (%) at each inclusion/exclusion step
print(my_cohort[[2]])
#>             Cohort creation step Overall N (%)            1           2
#>     <char>                <char>        <char>       <char>      <char>
#> 1: Incl. 1 All GEMINI encounters         10000         1991        2022
#> 2: Incl. 2       Gender = Female  4852 (48.5%) 1053 (52.9%) 956 (47.3%)
#> 3: Incl. 3              Age > 65  3309 (68.2%)  647 (61.4%) 705 (73.7%)
#> 4: Excl. 1     In-hospital death  -218 (-6.6%)  -59 (-9.1%) -55 (-7.8%)
#> 5:                  Final cohort          3091          588         650
#>               3           4           5
#>          <char>      <char>      <char>
#> 1:         2032        1990        1965
#> 2: 1020 (50.2%) 860 (43.2%)   963 (49%)
#> 3:  703 (68.9%) 633 (73.6%) 621 (64.5%)
#> 4:    -42 (-6%) -31 (-4.9%)   -31 (-5%)
#> 5:          661         602         590
```
