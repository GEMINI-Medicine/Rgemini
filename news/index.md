# Changelog

## Rgemini `2.0.0`

- **Addition of RxNorm functions**:
  - [`rxnorm_query()`](https://gemini-medicine.github.io/Rgemini/reference/rxnorm_query.md)
    to identify drugs of interest from pharmacy table
  - [`prepare_pharm_for_validation()`](https://gemini-medicine.github.io/Rgemini/reference/prepare_pharm_for_validation.md)
    to standardize RxNorm mapping validation
  - [`normalize_text()`](https://gemini-medicine.github.io/Rgemini/reference/normalize_text.md)
    utility function to standardize medication names
  - Rxnorm vignette with step-by-step instructions for pharmacy mapping
    workflow
    - *Changes for internal GEMINI staff:* Compared to the previous
      GEMINIpkg version of RxNorm, the following changes have been made
      - improved documentation
      - [`rxnorm_query()`](https://gemini-medicine.github.io/Rgemini/reference/rxnorm_query.md):
        Changes to input arguments and improved query speed
      - [`prepare_pharm_for_validation()`](https://gemini-medicine.github.io/Rgemini/reference/prepare_pharm_for_validation.md):
        - DB connection to `drm_cleandb_v4_1_1` required to query
          `lookup_pharmacy_mapping` table with previously validated
          mappings
        - Pharmacy mapping DB no longer queried, but can be provided as
          `custom_lookup` input
- **Other new functions**:
  - [`covid_surge_index()`](https://gemini-medicine.github.io/Rgemini/reference/covid_surge_index.md)
    function that calculates the COVID surge index by hospital-month
  - [`cci_group()`](https://gemini-medicine.github.io/Rgemini/reference/cci_group.md)
    to derive grouping of intervention codes into CCI sections and
    subsections
  - [`cci_search()`](https://gemini-medicine.github.io/Rgemini/reference/cci_search.md)
    to identify CCI codes for interventions of interest
- **Testing improvements:**
  - Unit tests are now also run in Python via rpy2 to ensure
    cross-language compatibility
- **Bug fixes**
  - Fixed
    [`render_cell_suppression.strat()`](https://gemini-medicine.github.io/Rgemini/reference/render_cell_suppression.strat.md)
    to be compatible with table1 version 1.5.0
  - Small bug fix in
    [`daily_census()`](https://gemini-medicine.github.io/Rgemini/reference/daily_census.md)
    to return all hospital ID variables provided in `cohort` input
  - Small fix in
    [`data_coverage()`](https://gemini-medicine.github.io/Rgemini/reference/data_coverage.md)
    for `hospital_num` class
  - Removed repeated warning messages in
    [`loop_mlaps()`](https://gemini-medicine.github.io/Rgemini/reference/loop_mlaps.md)
- **Miscellaneous**:
  - Removed age exclusion in
    [`episodes_of_care()`](https://gemini-medicine.github.io/Rgemini/reference/episodes_of_care.md)
    to accommodate paeds cohort
  - Updated readmission vignette to clarify use of restricted cohort
    (all-Med & ICU) in derived readmission flags
  - Refactored
    [`disability()`](https://gemini-medicine.github.io/Rgemini/reference/disability.md)
    and
    [`frailty_score()`](https://gemini-medicine.github.io/Rgemini/reference/frailty_score.md)
    to remove dependency on `fuzzyjoin` package
  - Added hex sticker
  - Integration of issues with Jira

## Rgemini `1.1.0`

- **New functions:**
  - [`data_coverage()`](https://gemini-medicine.github.io/Rgemini/reference/data_coverage.md)
    function to facilitate data coverage checks
  - [`cohort_creation()`](https://gemini-medicine.github.io/Rgemini/reference/cohort_creation.md)
    to generate cohort and show number (%) of entries at each
    inclusion/exclusion step
  - [`neighbourhood_ses()`](https://gemini-medicine.github.io/Rgemini/reference/neighbourhood_ses.md)
    deriving neighbourhood-level variables from the Statistics Canada
    Census & Ontario Marginalization Index
  - **Utility functions:**
    - [`quiet()`](https://gemini-medicine.github.io/Rgemini/reference/quiet.md)
      to run any functions without warning/error/print messages
    - [`compare_sets()`](https://gemini-medicine.github.io/Rgemini/reference/compare_sets.md)
      to find the number of unique and common elements in two sets
    - [`create_ntiles()`](https://gemini-medicine.github.io/Rgemini/reference/create_ntiles.md)
      to bin numeric variables into user-specified quantiles
- **Function enhancements:**
  - Added option to return readmission `genc_id` in
    [`readmission()`](https://gemini-medicine.github.io/Rgemini/reference/readmission.md)
    function
  - Aligned
    [`n_routine_bloodwork()`](https://gemini-medicine.github.io/Rgemini/reference/n_routine_bloodwork.md)
    with DRM by only returning lab tests with valid numeric results
  - Removed redundant check for acute-care transfers in
    [`episodes_of_care()`](https://gemini-medicine.github.io/Rgemini/reference/episodes_of_care.md)
    (only use mapped institution types from `lookup_transfer`)
  - Enabled custom line color without specifying color grouping variable
    in
    [`plot_over_time()`](https://gemini-medicine.github.io/Rgemini/reference/plot_over_time.md)
- **Documentation updates:**
  - Added pre-commit hooks: Run `styler` and check for
    [`browser()`](https://rdrr.io/r/base/browser.html) statements &
    large files
  - Improved documentation in
    [`readmission()`](https://gemini-medicine.github.io/Rgemini/reference/readmission.md)
    vignette
  - Updated all links referencing the data dictionary
  - Improved organization of figures for vignettes

## Rgemini `1.0.2`

- Small bug fix in
  [`find_db_tablename()`](https://gemini-medicine.github.io/Rgemini/reference/find_db_tablename.md)
  for materialized views (H4H \>= v4)

## Rgemini `1.0.1`

- Enhanced
  [`find_db_tablename()`](https://gemini-medicine.github.io/Rgemini/reference/find_db_tablename.md)
  to work with materialized views (H4H \>= v4)

## Rgemini `1.0.0`

- New function
  [`homelessness()`](https://gemini-medicine.github.io/Rgemini/reference/homelessness.md)
  to derive encounter-level homelessness flag based on ICD-10-CA
  diagnosis codes
- Enhanced `cell_suppression.R` renderers for `table1` to support
  simultaneous display of both mean and median
- Small fix in
  [`episodes_of_care()`](https://gemini-medicine.github.io/Rgemini/reference/episodes_of_care.md)
  for compatibility with future DB versions due to change in
  `lookup_transfer` table
- Tested on GEMINI Data Repository v3

## Rgemini `0.5.1`

- Improved query efficiency in
  [`mlaps()`](https://gemini-medicine.github.io/Rgemini/reference/mlaps.md)
  and other functions for clinical derived variables
- Removed duplicated code in
  [`icu_entry()`](https://gemini-medicine.github.io/Rgemini/reference/icu_entry.md)

## Rgemini `0.5.0`

- **New functions:**

  - Plotting functions & vignette for data exploration:
    [`plot_summary()`](https://gemini-medicine.github.io/Rgemini/reference/plot_summary.md)
    and
    [`plot_over_time()`](https://gemini-medicine.github.io/Rgemini/reference/plot_over_time.md)
  - Plotting functions & vignette for plot aesthetics:
    [`plot_theme()`](https://gemini-medicine.github.io/Rgemini/reference/plot_theme.md)
    and
    [`gemini_colors()`](https://gemini-medicine.github.io/Rgemini/reference/gemini_colors.md)
  - [`hospitalizations_last_n_days()`](https://gemini-medicine.github.io/Rgemini/reference/hospitalizations_last_n_days.md)
    to calculate the number of previous hospitalizations in a given
    window
  - [`convert_dt()`](https://gemini-medicine.github.io/Rgemini/reference/convert_dt.md)
    to handle date-time variables and show missing/invalid entries

- **Function enhancements:**

  - `n_imaging`,
    [`n_routine_bloodwork()`](https://gemini-medicine.github.io/Rgemini/reference/n_routine_bloodwork.md)
    and `n_rbc_transfusion()` refactored and updated to be able to
    exclude clinical records in ED
  - `n_imaging`, `n_routine_bloodwork` and `n_rbc_transfusion`
    refactored and updated to be able to exclude clinical records in ED
  - Inclusion of all routine blood tests (regardless of result value) in
    [`n_routine_bloodwork()`](https://gemini-medicine.github.io/Rgemini/reference/n_routine_bloodwork.md)
  - Option to derive ICU entries as a clinical outcome with customizable
    time windows in
    [`icu_entry()`](https://gemini-medicine.github.io/Rgemini/reference/icu_entry.md)
  - For functions with date-time inputs: Allow for character and POSIXct
    inputs
  - Improved efficiency in
    [`mlaps()`](https://gemini-medicine.github.io/Rgemini/reference/mlaps.md)
    hospital query

- **Minor bug fixes & updated unit tests:**

  - [`frailty_score()`](https://gemini-medicine.github.io/Rgemini/reference/frailty_score.md):
    Return frailty score 0
  - [`daily_census()`](https://gemini-medicine.github.io/Rgemini/reference/daily_census.md):
    Limit time period by discharge date
  - [`disability()`](https://gemini-medicine.github.io/Rgemini/reference/disability.md):
    Only return encounters in cohort input
  - Table 1 render functions: Enable `prettyNum` formatting
  - `Rgemini:::check_input()`: Return error for `odbc` connections

- **Small documentation updates & clean-up of pkdgown reference file**

## Rgemini 0.4.2

- Small bug fix in `Rgemini:::find_db_tablename()`

## Rgemini 0.4.1

- Small bug fix in `Rgemini:::check_input()`

## Rgemini 0.4.0

- Adds
  [`n_missing()`](https://gemini-medicine.github.io/Rgemini/reference/n_missing.md)
  function to check number of missingness
- Switch function
  [`frailty_score()`](https://gemini-medicine.github.io/Rgemini/reference/frailty_score.md)
  to calculate the CIHI Hospital Frailty Risk Score for each encounter
- Deprecates function
  [`frailty_score()`](https://gemini-medicine.github.io/Rgemini/reference/frailty_score.md)
  that calculates the UK Hospital Frailty Risk Score
- New utility function checking user inputs
- Adds core
  [`mlaps()`](https://gemini-medicine.github.io/Rgemini/reference/mlaps.md)
  function and wrapper function to run over multiple sites and years
- Adds
  [`er_los()`](https://gemini-medicine.github.io/Rgemini/reference/er_los.md)
  function to calculate emergency room length-of-stay
- New function
  [`dummy_ipadmdad()`](https://gemini-medicine.github.io/Rgemini/reference/dummy_ipadmdad.md)
  to simulate “ipadmdad” data with random hospital-level intercepts
- Enhanced
  [`daily_census()`](https://gemini-medicine.github.io/Rgemini/reference/daily_census.md)
  function to allow exclusion of days with 0 counts
- Additional input argument in
  [`covid_flag()`](https://gemini-medicine.github.io/Rgemini/reference/covid_flag.md)
  for inclusion of ER diagnoses
- New function
  [`disability()`](https://gemini-medicine.github.io/Rgemini/reference/disability.md)
  that derives disability flag for each encounter
- Adds function to calculate Elixhauser comorbidity scores
  [`elixhauser_comorbidity_index()`](https://gemini-medicine.github.io/Rgemini/reference/elixhauser_comorbidity_index.md)
- Adds utility function to generate warning about mapped values
- Standardizes citation format in function references section

## Rgemini 0.3.1

- Allows the user to specify the number of digits to round percentages
  to in cell suppression of categorical variables, without forcing
  rounding to integer
- Allows user to render only one level for binary variables in `table1`
- Fixes singularity due to missing levels in `table1`
- Fixes
  [`find_db_tablename()`](https://gemini-medicine.github.io/Rgemini/reference/find_db_tablename.md)
  and
  [`readmission()`](https://gemini-medicine.github.io/Rgemini/reference/readmission.md)
  for DBs with foreign data wrappers

## Rgemini 0.3.0

- Exports `n_rbc_transfusion()` function
- Adds installation instructions for GEMINI HPC
- Tested on GEMINI Data Repository v2.1.2

## Rgemini 0.2.0

- Small bug fixes in
  [`n_imaging()`](https://gemini-medicine.github.io/Rgemini/reference/n_imaging.md)
  and `n_rbc_transfusion()`
- Standardized argument names for DB and cohort inputs
- Standardized function names for count functions
- Improved documentation

## Rgemini 0.1.0

- This is the initial release of `Rgemini`, which includes variety of
  functions to do data analysis with the
  [GEMINI](https://www.geminimedicine.ca/) dataset
