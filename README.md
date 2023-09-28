
# Rgemini

<!-- badges: start -->
[![R-CMD-check](https://github.com/GEMINI-Medicine/Rgemini/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/GEMINI-Medicine/Rgemini/actions/workflows/check-standard.yaml)

<!-- badges: end -->

`Rgemini` (Our GEMINI) is an R package that provides a variety of functions to do data analysis with the [GEMINI](https://www.geminimedicine.ca/) dataset.

All functions have been developed and tested on the current version of the [GEMINI database](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).

## Installation

`Rgemini` is currently not yet available on CRAN. The development version can be installed from GitHub with the following:

``` r
remotes::install_github("GEMINI-Medicine/Rgemini", build_vignettes = TRUE)
```

If installing `Rgemini` from the GEMINI HPC environment, simply call `install.packages("Rgemini")`.
 
## Example

Most functions require access to the GEMINI database. With access, the functions can be used as follows:

``` r
library(Rgemini)

drv <- DBI::dbDriver("PostgreSQL")

db <- DBI::dbConnect(
  drv,
  dbname = "db_name",
  host = "172.XX.XX.XXX",
  port = 1234,
  user = getPass::getPass("Enter Username"),
  password = getPass::getPass("Enter Password")
)

admdad <- DBI::dbGetQuery(
  db,
  "SELECT * FROM public.admdad LIMIT 200;"
)

readm <- readmission(
  db = db,
  elective_admit = TRUE,
  death = TRUE,
  MAID = TRUE,
  palliative = TRUE,
  chemo = TRUE,
  mental = TRUE,
  obstetric = TRUE,
  signout = TRUE,
  restricted_cohort = admdad,
  readm_win = c(7, 30, 60, 90, 150)
)
```

Please review the extensive [package documentation](https://gemini-medicine.github.io/Rgemini/) and vignettes for detailed examples and sample code.

## Package versions

Different versions of `Rgemini` are released with X.Y.Z. increments following these conventions:

* `Z`: Patch version, bug fixes, documentation, tests, changing argument defaults, optimizing the internals of a function without affecting the external interface (argument names etc).
* `Y`: Minor version, new function added, some large changes to existing functions (including even changes to the type and number of arguments).
* `X`: Major version, change to the underlying DB the code was written for (table and variable names).

Usually we'd accumulate a series of changes on develop and release them all at once on master with a version increment.

## Citation

If you find `Rgemini` useful, please cite it in your publications using `citation("Rgemini")`:

> The GEMINI team (2023). Rgemini: R Functions for GEMINI Data. R package version 0.3.0. https://gemini-medicine.github.io/Rgemini/
