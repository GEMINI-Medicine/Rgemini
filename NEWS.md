# Rgemini `develop`

* **New functions:**
	* `data_coverage()` function to facilitate data coverage checks
	* Utility function `quiet()` to run any functions without warning/error/print messages

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
