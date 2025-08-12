# Rgemini <a href="https://gemini-medicine.github.io/Rgemini/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/GEMINI-Medicine/Rgemini/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/GEMINI-Medicine/Rgemini/actions/workflows/check-standard.yaml)

<!-- badges: end -->

`Rgemini` ("Our GEMINI") is a custom R package that provides a variety of functions to perform data analyses with [GEMINI](https://www.geminimedicine.ca/) data.

All functions have been developed by the GEMINI team and were tested on the current version of the [GEMINI database](https://geminimedicine.ca/the-gemini-database/#data-dictionary).

## Installation

`Rgemini` is currently not yet available on CRAN. Please follow the installation instructions below, depending on the environment you work in (HPC4Health or other).

### HPC4Health

The `Rgemini` package is automatically installed for all HPC4Health users working with GEMINI data. If you run into any issues using `Rgemini` functions or require a specific package version, please submit a ticket on HPC4Health.

### Other users

All other users can install the package from GitHub using the following:
``` r
remotes::install_github("GEMINI-Medicine/Rgemini", dependencies = TRUE)
```

You can also specify `build_vignettes = TRUE` to include all package vignettes. However, in that case, make sure to install any additional packages required for the vignettes (see packages listed under "Suggests" [here](https://github.com/GEMINI-Medicine/Rgemini/blob/main/DESCRIPTION)). Alternatively, you can review the knitted vignettes [here](https://gemini-medicine.github.io/Rgemini/articles/).

If the installation instructions from above don't work, you can `git clone` the `Rgemini` repository from the `main` branch, and then run `devtools::install("/path/to/repo", repos = NULL, type="source")`. Alternatively, you can try one of the following:

1. Install using `pak::pkg_install("GEMINI-Medicine/Rgemini")`.
2. Download the latest source tarball from the [package releases](https://github.com/GEMINI-Medicine/Rgemini/tags) to some directory such as `/path/to/tarball` and run `install.packages("/path/to/tarball", repos = NULL, type = "source")`.
3. Try configuring secure downloads as described in [this blog post](https://support.posit.co/hc/en-us/articles/206827897-Secure-Package-Downloads-for-R37).

If none of the above methods work, please create a post on our [discussion board](https://github.com/GEMINI-Medicine/Rgemini/discussions/categories/q-a).

## Example

Some functions require access to the GEMINI database. With access, the functions can be used as follows:

``` r
library(Rgemini)

# establish DB connection
drv <- DBI::dbDriver("PostgreSQL")
db <- DBI::dbConnect(
  drv,
  dbname = "db_name",
  host = "domain_name.ca",
  port = 1234,
  user = getPass::getPass("Enter Username"),
  password = getPass::getPass("Enter Password")
)

# For HPC4Health users: For newer datacuts (H4H >= v4.0.0) set the schema according to the datacut name
dbSendQuery(db, "Set schema 'datacut_name'");

# query data
admdad <- DBI::dbGetQuery(
  db,
  "SELECT * FROM admdad LIMIT 200;"
)

# run Rgemini function
readm <- readmission(
  db = db
)
```

Please review the extensive [package documentation](https://gemini-medicine.github.io/Rgemini/) and vignettes for detailed examples and sample code.

## Using Rgemini in Python

`Rgemini` can be used in Python via the `rpy2` package, which provides an interface to R from Python. This allows Python users to access Rgemini functions while working in their preferred environment.

### Installation and Setup

First, install `rpy2` in your Python environment:

```bash
pip install rpy2
```

### Example Usage

Here's how to use Rgemini functions in Python:

```python
import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

# Import R packages
base = importr('base')
rgemini = importr('Rgemini')

# Example: Using the lunique function
r_vector = robjects.IntVector([1, 1, 2, 2, 2, 3])
result = rgemini.lunique(r_vector)
print(f"Number of unique values: {result[0]}")
```

[Here](https://github.com/GEMINI-Medicine/Rgemini/discussions/200) is how to use Rgemini functions in R snippets in Python.

### Important Limitations and Disclaimers

**Testing Coverage**: Python compatibility has only been tested for functions that have unit tests in the R package. To check which functions have test coverage, review the test files in the [`tests/testthat/`](https://github.com/GEMINI-Medicine/Rgemini/tree/main/tests/testthat) directory.

**Use with Caution**: Even for functions where unit tests exist, users should exercise caution when running Rgemini in Python. The unit tests may not cover all possible use cases or edge conditions. We strongly recommend performing additional validation checks to ensure function outputs are correct for your specific data and use case.

**Data Type Considerations**: Be aware that data types may behave differently between R and Python. Pay special attention to how dates, factors, and missing values are handled when transferring data between the two environments.

## Contributing

Currently, only GEMINI team members can directly contribute to this package. However, we invite all users to share their feedback and suggestions with us so we can continue to improve `Rgemini`. 

If you identify any package bugs or would like to request enhancements, please open a [GitHub issue](https://github.com/GEMINI-Medicine/Rgemini/issues). We have also activated the GitHub
[discussion forum](https://github.com/GEMINI-Medicine/Rgemini/discussions) for you to submit any questions about package functionality or discuss applications of `Rgemini` functions to your research question.
Note that the repository is public, so please do not share any sensitive information and/or GEMINI data on the GitHub site!

For GEMINI team members: Please carefully review the [CONTRIBUTING.md](https://github.com/GEMINI-Medicine/Rgemini/blob/main/CONTRIBUTING.md) file for guidelines on how to contribute and review code.

## Package versions

Different versions of `Rgemini` are released with X.Y.Z. increments following these conventions:

* `Z`: Patch version, bug fixes, documentation, tests, changing argument defaults, optimizing the internals of a function without affecting the external interface (argument names etc).
* `Y`: Minor version, new function added, some large changes to existing functions (including even changes to the type and number of arguments).
* `X`: Major version, change to the underlying DB the code was written for (e.g., table and variable names).

Usually we accumulate a series of changes on develop and release them all at once on `main` with a version increment.

## Citation

If you find `Rgemini` useful, please cite it in your publications using `citation("Rgemini")`:

> The GEMINI team (2023). Rgemini: R Functions for GEMINI Data. R package version <version_number>. https://gemini-medicine.github.io/Rgemini/

Note that particular functions may require additional citations. Please consult the references in function-specific documentation whenever applicable.
