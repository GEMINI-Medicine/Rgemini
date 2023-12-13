
# Rgemini

<!-- badges: start -->
[![R-CMD-check](https://github.com/GEMINI-Medicine/Rgemini/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/GEMINI-Medicine/Rgemini/actions/workflows/check-standard.yaml)

<!-- badges: end -->

`Rgemini` (Our GEMINI) is a custom R package that provides a variety of functions to perform data analyses with [GEMINI](https://www.geminimedicine.ca/) data.

All functions have been developed by the GEMINI team and were tested on the current version of the [GEMINI database](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).

## Installation

`Rgemini` is currently not yet available on CRAN. The development version can be installed from GitHub with the following:

``` r
remotes::install_github("GEMINI-Medicine/Rgemini", build_vignettes = TRUE)
```

### Alternatives

If the installation method above does not work, try one of the following:

1. Install using `pak::pkg_install("GEMINI-Medicine/Rgemini")`.
2. `git clone` the package to some directory such as `/path/to/repo`, and run `devtools::install("/path/to/repo", build_vignettes = TRUE)`.
3. Download the latest source tarball from the [package releases](https://github.com/GEMINI-Medicine/Rgemini/tags) to some directory such as `/path/to/tarball` and run `install.packages("/path/to/tarball", repos = NULL, type="source")`.
4. Try configuring secure downloads as described in [this blog post](https://support.posit.co/hc/en-us/articles/206827897-Secure-Package-Downloads-for-R37).

If none of the above methods work, please create a post on our [discussion board](https://github.com/GEMINI-Medicine/Rgemini/discussions/categories/q-a).

### HPC4Health

If installing `Rgemini` from the GEMINI HPC4Health environment, simply call `install.packages("Rgemini")`.

If your `R` script has a dependency on `Rgemini` and you would like to run this script with `Slurm`, please point your library call to the local folder under which `Rgemini` was installed.

For example:

```r
library(Rgemini, lib.loc = "$HOME/R/x86_64-pc-linux-gnu-library/4.1")
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

Please review the extensive [package documentation](https://gemini-medicine.github.io/Rgemini/) and vignettes for detailed examples and sample code.

## Contributing

Currently, only GEMINI team members can directly contribute to this package. However, we invite all users to share their feedback and suggestions with us so we can continue to improve `Rgemini`. 

If you identify any package bugs or would like to request enhancements, please open a [GitHub issue](https://github.com/GEMINI-Medicine/Rgemini/issues). We have also activated the GitHub
[discussion forum](https://github.com/GEMINI-Medicine/Rgemini/discussions) for you to submit any questions about package functionality or discuss applications of `Rgemini` functions to your research question.
Note that the repository is public, so please do not share any sensitive information and/or GEMINI data on the GitHub site!

For GEMINI team members: Please carefully review the [CONTRIBUTING.md](https://github.com/GEMINI-Medicine/Rgemini/blob/master/CONTRIBUTING.md) file for guidelines on how to contribute and review code.

## Package versions

Different versions of `Rgemini` are released with X.Y.Z. increments following these conventions:

* `Z`: Patch version, bug fixes, documentation, tests, changing argument defaults, optimizing the internals of a function without affecting the external interface (argument names etc).
* `Y`: Minor version, new function added, some large changes to existing functions (including even changes to the type and number of arguments).
* `X`: Major version, change to the underlying DB the code was written for (table and variable names).

Usually we accumulate a series of changes on develop and release them all at once on master with a version increment.

## Citation

If you find `Rgemini` useful, please cite it in your publications using `citation("Rgemini")`:

> The GEMINI team (2023). Rgemini: R Functions for GEMINI Data. R package version 0.3.0. https://gemini-medicine.github.io/Rgemini/

Note that particular functions may require additional citations. Please consult the references in function-specific documentation whenever applicable.
