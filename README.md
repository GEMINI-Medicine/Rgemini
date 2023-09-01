
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

Please review the extensive package documentation and vignettes for detailed examples and sample code.
