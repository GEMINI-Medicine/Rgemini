# Rgemini `develop`

* **New vignette:**
  * Migrated Rxnorm-Pharmacy-Mapping Vignette from GEMINIpkg, and added usage guidelines for HPC4Health users on `rxnorm_query()` and `prepare_pharm_for_validation()`.

* **New functions:**
  * `prepare_pharm_for_validation()` function migrated from GEMINIpkg to standardize workflow for pharmacy mapping validation following rxnorm_query.
    * To support broader use cases, the migrated function now takes the database connection as the first argument (`pharm_dbcon`), while the `GEMINIpkg::prepare_pharm_for_validation() connects to the pharmacy mapping database automatically.

**Rxnorm migration**
* added `GEMINIpkg::gemini_rxnorm_query()` as `rxnorm_query()`, with the following changes
  * improved documentation
  * changed the `db_con` parameter to `dbcon`
  * changed `drug_input` argument name to `drug_name` and `class_input` to `drug_class`
  * changed the `genc_ids` parameter to `cohort`
  * removed the `sites` parameter
  * added `detailed_search` and `return_drug_list` as explicit parameters
  * applied `find_db_tablename` to query `pharmacy_subset` on H4H
* Added `normalize_text()` utility function
 
**Other changes**
* Fixed `render_cell_suppression.strat()` to be compatible with table1 version 1.5.0
* Removed age exclusion in `episodes_of_care()` to accommodate paeds cohort
* Small bug fix in `daily_census` to return all hospital ID variables provided in `cohort` input

* **Testing improvements:**
  * Unit tests are now also run in Python via rpy2 to ensure cross-language compatibility

* **Miscellanous:**
  * Added hex sticker
  * Removed repeated warning messages in `loop_mlaps`
  * Integration of issues with Jira
  * Improved function documentation



# Rgemini `1.1.0`

* **New functions:**
  * `data_coverage()` function to facilitate data coverage checks
  * `cohort_creation()` to generate cohort and show number (%) of entries at each inclusion/exclusion step
  * `neighbourhood_ses()` deriving neighbourhood-level variables from the Statistics Canada Census & Ontario Marginalization Index
  * **Utility functions:**
    * `quiet()` to run any functions without warning/error/print messages
    * `compare_sets()` to find the number of unique and common elements in two sets
    * `create_ntiles()` to bin numeric variables into user-specified quantiles

* **Function enhancements:**
  * Added option to return readmission `genc_id` in `readmission()` function
  * Aligned `n_routine_bloodwork()` with DRM by only returning lab tests with valid numeric results 
  * Removed redundant check for acute-care transfers in `episodes_of_care()` (only use mapped institution types from `lookup_transfer`)
  * Enabled custom line color without specifying color grouping variable in `plot_over_time()`

* **Documentation updates:**
  * Added pre-commit hooks: Run `styler` and check for `browser()` statements & large files
  * Improved documentation in `readmission()` vignette
  * Updated all links referencing the data dictionary
  * Improved organization of figures for vignettes


# Rgemini `1.0.2`

* Small bug fix in `find_db_tablename()` for materialized views (H4H >= v4)

# Rgemini `1.0.1`

* Enhanced `find_db_tablename()` to work with materialized views (H4H >= v4)

# Rgemini `1.0.0`

* New function `homelessness()` to derive encounter-level homelessness flag based on ICD-10-CA diagnosis codes
* Enhanced `cell_suppression.R` renderers for `table1` to support simultaneous display of both mean and median
* Small fix in `episodes_of_care()` for compatibility with future DB versions due to change in `lookup_transfer` table
* Tested on GEMINI Data Repository v3

# Rgemini `0.5.1`

* Improved query efficiency in `mlaps()` and other functions for clinical derived variables
* Removed duplicated code in `icu_entry()`

# Rgemini `0.5.0`

* **New functions:**

  * Plotting functions & vignette for data exploration: `plot_summary()` and `plot_over_time()`
  * Plotting functions & vignette for plot aesthetics: `plot_theme()` and `gemini_colors()`
  * `hospitalizations_last_n_days()` to calculate the number of previous hospitalizations in a given window
  * `convert_dt()` to handle date-time variables and show missing/invalid entries
* **Function enhancements:**

  * `n_imaging`, `n_routine_bloodwork()` and `n_rbc_transfusion()` refactored and updated to be able to exclude clinical records in ED
  * `n_imaging`, `n_routine_bloodwork` and `n_rbc_transfusion` refactored and updated to be able to exclude clinical records in ED
  * Inclusion of all routine blood tests (regardless of result value) in `n_routine_bloodwork()`
  * Option to derive ICU entries as a clinical outcome with customizable time windows in `icu_entry()`
  * For functions with date-time inputs: Allow for character and POSIXct inputs
  * Improved efficiency in `mlaps()` hospital query
* **Minor bug fixes & updated unit tests:**

  * `frailty_score()`: Return frailty score 0
  * `daily_census()`: Limit time period by discharge date
  * `disability()`: Only return encounters in cohort input
  * Table 1 render functions: Enable `prettyNum` formatting
  * `Rgemini:::check_input()`: Return error for `odbc` connections
* **Small documentation updates & clean-up of pkdgown reference file**

# Rgemini 0.4.2

* Small bug fix in `Rgemini:::find_db_tablename()`

# Rgemini 0.4.1

* Small bug fix in `Rgemini:::check_input()`

# Rgemini 0.4.0

* Adds `n_missing()` function to check number of missingness
* Switch function `frailty_score()` to calculate the CIHI Hospital Frailty Risk Score for each encounter
* Deprecates function `frailty_score()` that calculates the UK Hospital Frailty Risk Score
* New utility function checking user inputs
* Adds core `mlaps()` function and wrapper function to run over multiple sites and years
* Adds `er_los()` function to calculate emergency room length-of-stay
* New function `dummy_ipadmdad()` to simulate "ipadmdad" data with random hospital-level intercepts
* Enhanced `daily_census()` function to allow exclusion of days with 0 counts
* Additional input argument in `covid_flag()` for inclusion of ER diagnoses
* New function `disability()` that derives disability flag for each encounter
* Adds function to calculate Elixhauser comorbidity scores `elixhauser_comorbidity_index()`
* Adds utility function to generate warning about mapped values
* Standardizes citation format in function references section

# Rgemini 0.3.1

* Allows the user to specify the number of digits to round percentages to in cell suppression of categorical variables, without forcing rounding to integer
* Allows user to render only one level for binary variables in `table1`
* Fixes singularity due to missing levels in `table1`
* Fixes `find_db_tablename()` and `readmission()` for DBs with foreign data wrappers

# Rgemini 0.3.0

* Exports `n_rbc_transfusion()` function
* Adds installation instructions for GEMINI HPC
* Tested on GEMINI Data Repository v2.1.2

# Rgemini 0.2.0

* Small bug fixes in `n_imaging()` and `n_rbc_transfusion()`
* Standardized argument names for DB and cohort inputs
* Standardized function names for count functions
* Improved documentation

# Rgemini 0.1.0

* This is the initial release of `Rgemini`, which includes variety of functions to do data analysis with the [GEMINI](https://www.geminimedicine.ca/) dataset
