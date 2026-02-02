# Compute entry to Intensive Care Unit

Determines whether an encounter has entered Intensive Care Unit (ICU)
during hospital stay using CIHI Discharge Abstract Database (DAD)
fields.

## Usage

``` r
icu_entry(
  cohort,
  ipscu,
  as_outcome = FALSE,
  exclude_cutoff = 0,
  entry_since_cutoff = c(24, 48, 72)
)
```

## Arguments

- cohort:

  (`data.table`, `data.frame`)  
  Cohort table with all relevant encounters of interest, where each row
  corresponds to a single encounter. Must contain the following columns:

  - `genc_id` (`integer`): GEMINI encounter ID

  - `admission_date_time` (`character`): Date-time of admission in
    YYYY-MM-DD HH:MM format

- ipscu:

  (`data.table`, `data.frame`)  
  Table equivalent to the `ipscu` table defined in the [GEMINI Data
  Repository
  Dictionary](https://geminimedicine.ca/the-gemini-database/). Table
  must contain fields: GEMINI Encounter ID (`genc_id`), SCU admission
  time (`scu_admit_date_time`), and SCU number (`scu_unit_number`).

- as_outcome:

  (`logical`)  
  Whether ICU admission is as a clinical outcome or not. Default to
  FALSE. When set to TRUE, records with direct ICU admission before
  admitted to inpatient care (IP admission) (i.e. `scu_admit_date_time`
  \<= `admission_date_time`) are excluded.

- exclude_cutoff:

  (`integer`)  
  The number of hours to add to the IP admission time to establish a
  cutoff time for excluding records from being identified as ICU
  admissions. This parameter is only relevant when `as_outcome=TRUE`.
  For example, when `exclude_cutoff = 12`, records with ICU entry time
  `scu_admit_date_time` \<= `admission_date_time + 12 hours` are
  excluded from being identified as ICU admissions. Default is
  exclude_cutoff = 0, where any ICU entries before IP admission time are
  excluded.

- entry_since_cutoff:

  (`integer`, `vector`)  
  Time window of ICU entry since IP admission (or since x hours post IP
  admission when user specifies `exclude_cutoff`), in hours. This
  parameter, together with `exclude_cutoff` specifies the time interval
  during which ICU admissions are identified. By default,
  `exclude_cutoff = 0` and `entry_since_cutoff = c(24, 48, 72)`,
  function calculates ICU entry within 24, 48 and 72 hours since IP
  admission. Users can specify different values for the two parameters
  to customize the time interval of interest. For example, when
  `exclude_cutoff = 24` and `entry_since_cutoff = 48`, function
  determines whether a patient was admitted to ICU between 24 to 72
  (24+48) hours post IP admission (i.e. time interval (24, 72\] hours)).

## Value

(`data.table`)  
By default, for each encounter in input "cohort" returns the
corresponding derived boolean (TRUE/FALSE) fields "icu_entry_derived",
"icu_entry_in_24hr_derived", "icu_entry_in_48hr_derived" and
"icu_entry_in_72hr_derived". If user specified time window x hour is
used, field "icu_entry_in_xhr_derived" is computed in addition to
"icu_entry_derived".

## Details

This function uses DAD fields Admission date-time (Group 04 Fields
01/02), and SCU Admit date-time (Group 13, Field 03/04) to derive
boolean fields indicating ICU entries at any time during hospital stay,
and within specified time window since hospital admission.

By definition in DAD (Group 13, Field 02), SCUs include ICUs and
Step-Down Units. Step-Down Units are not considered as ICUs.

Therefore, this function excludes below CIHI defined Step-Down Units
numbers from calculation:

- 90: Step-Down Medical Unit

- 93: Combined Medical/Surgical Step-Down Unit

- 95: Step-Down Surgical Unit

- 99: No SCU

Please refer to the CIHI DAD abstracting manual for more details.

## Note

: By design, this function will not return any NA values for
`icu_entry`, unless the flag is time sensitive (i.e., if `as_outcome` is
`TRUE`, or when deriving ICU admission within a certain time
window/after a cut-off). For time sensitive flags, any genc_ids with an
ICU entry but *no* valid ICU admission date-time will be returned as NA.
If a genc_id has *at least one* valid ICU admission date-time, all valid
entries will be included in the calculation. Note that a lot of invalid
ICU admission date-times contain date information only. Users may choose
to impute missing timestamps prior to running this function.

When one tries to left-join the output of this function to another
table, make sure the list of encounters aligns in both tables. As there
are 2 dependent parameters (exclude_cutoff, entry_since_cutoff)
determining the time interval during which ICU admissions will be
identified, please read function documentation carefully when specifying
a non-default value to these parameters to ensure the identification
time interval is as intented. See examples below for common use cases.

## Examples

``` r
# ICU admission within the first 24 hours since IP admission
# (i.e. you are interested in knowing % of encounters admitted to ICU):
if (FALSE) { # \dontrun{
icu_entry(cohort, ipscu, as_outcome = FALSE, entry_since_cutoff = 24)
} # }

# ICU admission within the first 24 hours since IP admission and with
# ICU as clinical outcome excluding records with ICU prior to IP admission:
if (FALSE) { # \dontrun{
icu_entry(cohort, ipscu, as_outcome = TRUE, entry_since_cutoff = 24)
} # }

# ICU admission within the first 72 hours since IP admission and with
# ICU as a clinical outcome excluding records with ICU prior to the
# first 24 hours of IP admission (i.e. you are interested in knowing
# patients who were admitted to ICU between the interval of (24, 72] hours
# since IP admission):
if (FALSE) { # \dontrun{
icu_entry(
  cohort, ipscu,
  as_outcome = TRUE,
  exclude_cutoff = 24,
  entry_since_cutoff = 48 # = 48 instead of 72 because 24+48=72
)
} # }
```
