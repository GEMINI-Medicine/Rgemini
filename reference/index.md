# Package index

## Derived Variables

These functions derive variables from existing data.

- [`charlson_comorbidity_index()`](https://gemini-medicine.github.io/Rgemini/reference/charlson_comorbidity_index.md)
  : Compute Charlson comorbidity index (CCI) score
- [`comorbidity_index()`](https://gemini-medicine.github.io/Rgemini/reference/comorbidity_index.md)
  : Compute Comorbidity Index
- [`covid_flag()`](https://gemini-medicine.github.io/Rgemini/reference/covid_flag.md)
  : COVID Flag
- [`covid_surge_index()`](https://gemini-medicine.github.io/Rgemini/reference/covid_surge_index.md)
  : Derive COVID Surge Index
- [`daily_census()`](https://gemini-medicine.github.io/Rgemini/reference/daily_census.md)
  : Compute daily census
- [`day_time_of_admission()`](https://gemini-medicine.github.io/Rgemini/reference/day_time_of_admission.md)
  : Compute day & time of hospital admissions
- [`disability()`](https://gemini-medicine.github.io/Rgemini/reference/disability.md)
  : Identify encounters with disability
- [`elixhauser_comorbidity_index()`](https://gemini-medicine.github.io/Rgemini/reference/elixhauser_comorbidity_index.md)
  : Compute Elixhauser comorbidity score
- [`episodes_of_care()`](https://gemini-medicine.github.io/Rgemini/reference/episodes_of_care.md)
  : Identify episodes of care
- [`er_los()`](https://gemini-medicine.github.io/Rgemini/reference/er_los.md)
  : Compute Length of Stay in Emergency Room
- [`frailty_score()`](https://gemini-medicine.github.io/Rgemini/reference/frailty_score.md)
  : Compute CIHI Hospital Frailty Risk Score
- [`homelessness()`](https://gemini-medicine.github.io/Rgemini/reference/homelessness.md)
  : Homelessness flag
- [`hospital_fiscal_year()`](https://gemini-medicine.github.io/Rgemini/reference/hospital_fiscal_year.md)
  : Get hospital fiscal year
- [`hospitalizations_last_n_days()`](https://gemini-medicine.github.io/Rgemini/reference/hospitalizations_last_n_days.md)
  : Compute number of previous hospitalizations for an encounter in a
  given window.
- [`icu_entry()`](https://gemini-medicine.github.io/Rgemini/reference/icu_entry.md)
  : Compute entry to Intensive Care Unit
- [`icu_los()`](https://gemini-medicine.github.io/Rgemini/reference/icu_los.md)
  : Compute Length of Stay in Intensive Care Unit
- [`in_hospital_mortality()`](https://gemini-medicine.github.io/Rgemini/reference/in_hospital_mortality.md)
  : Compute in-hospital mortality using CIHI DAD
- [`laps_assign_test()`](https://gemini-medicine.github.io/Rgemini/reference/laps_assign_test.md)
  : Assign score to LAPS component
- [`length_of_stay()`](https://gemini-medicine.github.io/Rgemini/reference/length_of_stay.md)
  : Compute hospital Length of Stay (LoS)
- [`loop_mlaps()`](https://gemini-medicine.github.io/Rgemini/reference/loop_mlaps.md)
  : Loop mLAPS
- [`mlaps()`](https://gemini-medicine.github.io/Rgemini/reference/mlaps.md)
  : mLAPS
- [`n_imaging()`](https://gemini-medicine.github.io/Rgemini/reference/n_imaging.md)
  : Compute the number of radiology tests per encounter
- [`n_missing()`](https://gemini-medicine.github.io/Rgemini/reference/n_missing.md)
  [`mi2()`](https://gemini-medicine.github.io/Rgemini/reference/n_missing.md)
  : Number of missingness
- [`n_rbc_transfusions()`](https://gemini-medicine.github.io/Rgemini/reference/n_rbc_transfusions.md)
  : Count the number Red Blood Cell (RBC) Transfusions per encounter
- [`n_routine_bloodwork()`](https://gemini-medicine.github.io/Rgemini/reference/n_routine_bloodwork.md)
  : Compute the number of routine bloodwork tests per encounter
- [`neighbourhood_ses()`](https://gemini-medicine.github.io/Rgemini/reference/neighbourhood_ses.md)
  [`neighborhood_ses()`](https://gemini-medicine.github.io/Rgemini/reference/neighbourhood_ses.md)
  : Obtain commonly used neighbourhood-level socioeconomic status (SES)
  variables
- [`readmission()`](https://gemini-medicine.github.io/Rgemini/reference/readmission.md)
  : Compute readmission and episodes of care.
- [`season()`](https://gemini-medicine.github.io/Rgemini/reference/season.md)
  : Get Season
- [`stat_holidays_ON()`](https://gemini-medicine.github.io/Rgemini/reference/stat_holidays_ON.md)
  : Flag statutory holidays in Ontario

## Data Transformation

These functions aid in querying the data, either by applying filters or
mapping values.

- [`cci_group()`](https://gemini-medicine.github.io/Rgemini/reference/cci_group.md)
  : Grouping CCI intervention codes
- [`cci_search()`](https://gemini-medicine.github.io/Rgemini/reference/cci_search.md)
  : Identify relevant CCI codes for interventions of interest
- [`diagnoses_at_admission()`](https://gemini-medicine.github.io/Rgemini/reference/diagnoses_at_admission.md)
  : Determine Diagnoses at Admission
- [`icd_to_ccsr()`](https://gemini-medicine.github.io/Rgemini/reference/icd_to_ccsr.md)
  : Identify CCSR categories for ICD-10-CA diagnosis codes
- [`max_result_value()`](https://gemini-medicine.github.io/Rgemini/reference/max_result_value.md)
  : Calculates maxima when input vector is not empty, else returns NA.
- [`min_result_value()`](https://gemini-medicine.github.io/Rgemini/reference/min_result_value.md)
  : Calculates minima when input vector is not empty, else returns NA.
- [`prepare_pharm_for_validation()`](https://gemini-medicine.github.io/Rgemini/reference/prepare_pharm_for_validation.md)
  : Prepare RxNorm Data for Validation
- [`rxnorm_query()`](https://gemini-medicine.github.io/Rgemini/reference/rxnorm_query.md)
  : RxNorm Pharmacy Data Mapping Function

## Cohort Creation & Data Checks

These functions help with cohort generation and commonly performed data
checks

- [`cohort_creation()`](https://gemini-medicine.github.io/Rgemini/reference/cohort_creation.md)
  : Cohort creation
- [`data_coverage()`](https://gemini-medicine.github.io/Rgemini/reference/data_coverage.md)
  : Check data coverage

## Cell Suppression

This includes all helper functions associated with producing a Table 1.

- [`max_pairwise_smd()`](https://gemini-medicine.github.io/Rgemini/reference/max_pairwise_smd.md)
  : Maximum Pairwise Standardized Mean Difference
- [`render_cell_suppression.categorical()`](https://gemini-medicine.github.io/Rgemini/reference/render_cell_suppression.categorical.md)
  : Render Cell Suppression (Categorical)
- [`render_cell_suppression.continuous()`](https://gemini-medicine.github.io/Rgemini/reference/render_cell_suppression.continuous.md)
  : Render Cell Suppression (Continuous)
- [`render_cell_suppression.default()`](https://gemini-medicine.github.io/Rgemini/reference/render_cell_suppression.default.md)
  : Render Cell Suppression (Default)
- [`render_cell_suppression.discrete()`](https://gemini-medicine.github.io/Rgemini/reference/render_cell_suppression.discrete.md)
  : Render Cell Suppression (Discrete)
- [`render_cell_suppression.missing()`](https://gemini-medicine.github.io/Rgemini/reference/render_cell_suppression.missing.md)
  : Render Cell Suppression (Missing)
- [`render_cell_suppression.strat()`](https://gemini-medicine.github.io/Rgemini/reference/render_cell_suppression.strat.md)
  : Render Cell Suppression (Strata)
- [`render_default.discrete()`](https://gemini-medicine.github.io/Rgemini/reference/render_default.discrete.md)
  : Render Default (Discrete)
- [`render_mean.continuous()`](https://gemini-medicine.github.io/Rgemini/reference/render_mean.continuous.md)
  : Render Mean (Continuous)
- [`render_median.continuous()`](https://gemini-medicine.github.io/Rgemini/reference/render_median.continuous.md)
  : Render Median (Continuous)
- [`render_strict_cell_suppression.categorical()`](https://gemini-medicine.github.io/Rgemini/reference/render_strict_cell_suppression.categorical.md)
  : Render Strict Cell Suppression (Categorical)

## Dummy Data

Helper functions designed to produce sample data.

- [`dummy_admdad()`](https://gemini-medicine.github.io/Rgemini/reference/dummy_admdad.md)
  : Generated simulated administrative data
- [`dummy_diag()`](https://gemini-medicine.github.io/Rgemini/reference/dummy_diag.md)
  : Generate Simulated Diagnosis Data Table
- [`dummy_ipadmdad()`](https://gemini-medicine.github.io/Rgemini/reference/dummy_ipadmdad.md)
  : Simulate ipadmdad data
- [`dummy_lab()`](https://gemini-medicine.github.io/Rgemini/reference/dummy_lab.md)
  : Generated simulated lab data
- [`sample_icd()`](https://gemini-medicine.github.io/Rgemini/reference/sample_icd.md)
  : Simulate ICD-10 Diagnosis Codes

## Plotting Functions

Functions for common plotting needs.

- [`gemini_colors()`](https://gemini-medicine.github.io/Rgemini/reference/gemini_colors.md)
  : GEMINI colors

- [`plot_color_palettes()`](https://gemini-medicine.github.io/Rgemini/reference/plot_color_palettes.md)
  : Plot GEMINI color palettes

- [`plot_over_time()`](https://gemini-medicine.github.io/Rgemini/reference/plot_over_time.md)
  : Plot variable over time

- [`plot_summary()`](https://gemini-medicine.github.io/Rgemini/reference/plot_summary.md)
  : Plot descriptive summary statistics of multiple variables

- [`plot_theme()`](https://gemini-medicine.github.io/Rgemini/reference/plot_theme.md)
  : Plot theme for ggplots

- [`scale_color_gemini()`](https://gemini-medicine.github.io/Rgemini/reference/scale_color_gemini.md)
  :

  Wrapper function for
  [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  that applies GEMINI color palettes to ggplot objects.

- [`scale_fill_gemini()`](https://gemini-medicine.github.io/Rgemini/reference/scale_fill_gemini.md)
  :

  Wrapper function for
  [`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  that applies GEMINI color palettes to ggplot objects.

## Mapping files

Files containing variable mappings.

- [`mapping_cci`](https://gemini-medicine.github.io/Rgemini/reference/mapping_cci.md)
  :

  Mapping data for `cci_group` and `cci_filter` functions

- [`mapping_cihi_frailty`](https://gemini-medicine.github.io/Rgemini/reference/mapping_cihi_frailty.md)
  :

  Mapping data for `frailty_score` function

- [`mapping_disability`](https://gemini-medicine.github.io/Rgemini/reference/mapping_disability.md)
  :

  Mapping data for `disability` function

## Utility Functions

- [`check_input()`](https://gemini-medicine.github.io/Rgemini/reference/check_input.md)
  : Check user inputs

- [`coerce_to_datatable()`](https://gemini-medicine.github.io/Rgemini/reference/coerce_to_datatable.md)
  :

  Coerce to `data.table`

- [`compare_sets()`](https://gemini-medicine.github.io/Rgemini/reference/compare_sets.md)
  : Compare two sets to get the number of unique and common elements in
  each set

- [`coverage_message()`](https://gemini-medicine.github.io/Rgemini/reference/coverage_message.md)
  : Coverage Message

- [`convert_dt()`](https://gemini-medicine.github.io/Rgemini/reference/convert_dt.md)
  : Convert date-time variables into POSIXct/POSIXt format.

- [`create_ntiles()`](https://gemini-medicine.github.io/Rgemini/reference/create_ntiles.md)
  : Create N-tiles

- [`find_db_tablename()`](https://gemini-medicine.github.io/Rgemini/reference/find_db_tablename.md)
  : Find DB table/view name in database.

- [`fix_var_str()`](https://gemini-medicine.github.io/Rgemini/reference/fix_var_str.md)
  : Fix variable strings

- [`lunique()`](https://gemini-medicine.github.io/Rgemini/reference/lunique.md)
  : Returns number of unique values

- [`mapping_message()`](https://gemini-medicine.github.io/Rgemini/reference/mapping_message.md)
  : Mapping Message

- [`gemini_colors()`](https://gemini-medicine.github.io/Rgemini/reference/gemini_colors.md)
  : GEMINI colors

- [`` `%ni%` ``](https://gemini-medicine.github.io/Rgemini/reference/grapes-ni-grapes.md)
  : Not In

- [`scale_color_gemini()`](https://gemini-medicine.github.io/Rgemini/reference/scale_color_gemini.md)
  :

  Wrapper function for
  [`ggplot2::scale_color_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  that applies GEMINI color palettes to ggplot objects.

- [`scale_fill_gemini()`](https://gemini-medicine.github.io/Rgemini/reference/scale_fill_gemini.md)
  :

  Wrapper function for
  [`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html)
  that applies GEMINI color palettes to ggplot objects.

- [`normalize_text()`](https://gemini-medicine.github.io/Rgemini/reference/normalize_text.md)
  : Normalize string values

- [`return_hospital_field()`](https://gemini-medicine.github.io/Rgemini/reference/return_hospital_field.md)
  : Return Hospital Field

- [`quiet()`](https://gemini-medicine.github.io/Rgemini/reference/quiet.md)
  : Suppress errors/messages/warnings
