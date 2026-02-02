# Grouping CCI intervention codes

The [Canadian Classification of Health Interventions (CCI)
codes](https://www.cihi.ca/sites/default/files/document/guide-for-users-of-cci-manual-en.pdf)
provide a detailed classification of all inpatient interventions in
Canada, with more than 17,000 unique, alphanumeric codes.

The `cci_group()` function categorizes CCI codes into broader,
clinically meaningful intervention categories based on each code's first
3 characters.

Broadly, intervention codes can be grouped into the following CCI
Sections based on the first digit:

- 1: Physical/Physiological Therapeutic Interventions (1AA - 1ZZ)

- 2: Diagnostic Interventions (2AA - 2ZZ)

- 3: Diagnostic Imaging Interventions (3AF - 3ZZ)

- 5: Obstetrical and Fetal Interventions (5AB - 5PD)

- 6: Cognitive, Psychosocial and Sensory Therapeutic Interventions
  (6AA - 6VA)

- 7: Other Healthcare Interventions (7SC - 7SJ)

- 8: Therapeutic Interventions Strengthening the Immune System and/or
  Genetic Composition (8AA - 8ZZ)

Each section can further be broken down into more detailed subsections,
based on the CCI group (2nd-3rd character).For example, CCI codes in
Sections 1-3 can be differentiated by anatomy region, such as:

- Therapeutic Interventions on the Nervous System (1AA - 1BZ)

- Therapeutic Interventions on the Eye and Ocular Adnexa (1CC - 1CZ)

- Therapeutic Interventions on the Ear and Mastoid (1DA - 1DZ)

- etc.

For a complete list of all subsections, see [CIHI CCI sections and code
ranges](https://www.cihi.ca/en/overview-of-cci-sections-and-code-ranges).

## Usage

``` r
cci_group(cci_codes)
```

## Arguments

- cci_codes:

  (`data.frame` \| `data.table`) Table containing `intervention_code`
  column that lists all CCI intervention codes of interest (e.g., from
  the `ipintervention` or `erintervention` table).

## Value

`data.table` Data table containing the CCI codes of interest, together
with their corresponding groupings as follows:

- `section` (`numeric`): CCI section number (based on first character)

- `section_descr` (`character`): Section description

- `subsection` (`character`): CCI subsection (first 3 characters)

- `subsection_descr` (`character`): Subsection description

## References

https://www.cihi.ca/en/overview-of-cci-sections-and-code-ranges

## Examples

``` r
cci_codes <- data.frame(intervention_code = c(
  "1MA52HA", "2PB70DA", "3SC10KM", "5LB08ZZ", "6KA02ME", "8AA70BABA"
))
res <- cci_group(cci_codes)
```
