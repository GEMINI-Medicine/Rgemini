# Rgemini `develop`

* Updated unit tests & small bug fix in `daily_census`
* Ordered package names in pkgdown reference file
* Small bug fix in `Rgemini:::disability`

# Rgemini 0.4.2

* Small bug fix in `Rgemini:::find_db_tablename`

# Rgemini 0.4.1

* Small bug fix in `Rgemini:::check_input`

# Rgemini 0.4.0

* Adds `n_missing()` function to check number of missingness.
* Switch function `frailty_score()` to calculate the CIHI Hospital Frailty Risk Score for each encounter.
* Deprecates function `frailty_score()` that calculates the UK Hospital Frailty Risk Score.
* New utility function checking user inputs
* Adds core `mlaps()` function and wrapper function to run over multiple sites and years.
* Adds `er_los()` function to calculate emergency room length-of-stay.
* New function `dummy_ipadmdad()` to simulate "ipadmdad" data with random hospital-level intercepts
* Enhanced `daily_census()` function to allow exclusion of days with 0 counts
* Additional input argument in `covid_flag()` for inclusion of ER diagnoses
* New function `disability()` that derives disability flag for each encounter.
* Adds function to calculate Elixhauser comorbidity scores `elixhauser_comorbidity_index()`.
* Adds utility function to generate warning about mapped values.
* Standardizes citation format in function references section.

# Rgemini 0.3.1

* Allows the user to specify the number of digits to round percentages to in cell suppression of categorical variables, without forcing rounding to integer.
* Allows user to render only one level for binary variables in `table1`.
* Fixes singularity due to missing levels in `table1`.
* Fixes `find_db_tablename` and `readmission` for DBs with foreign data wrappers.

# Rgemini 0.3.0

* Exports `n_rbc_transfusion` function.
* Adds installation instructions for GEMINI HPC.
* Tested on GEMINI Data Repository v2.1.2

# Rgemini 0.2.0

* Small bug fixes in `n_imaging` and `n_rbc_transfusion`
* Standardized argument names for DB and cohort inputs
* Standardized function names for count functions
* Improved documentation

# Rgemini 0.1.0

* This is the initial release of `Rgemini`, which includes variety of functions to do data analysis with the [GEMINI](https://www.geminimedicine.ca/) dataset.
