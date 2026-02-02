# Check user inputs

Function checking whether user-provided inputs for a function are
appropriate. The following check is applied for all inputs:

- Whether input is of correct type (e.g., `logical`, `numeric`,
  `character` etc.) For some input types, the following additional
  checks can be applied optionally:

- Check whether length of provided input is as expected

- For `numeric`/`integer` inputs: Check whether provided input is within
  acceptable interval (e.g., between 1-100).

- For `character` (categorical) inputs: Check whether input corresponds
  to one of acceptable categories.

- For `data.table|data.frame` inputs: 1) Check whether required columns
  exist in table, 2) whether each column is of a specified type, and 3)
  whether all entries are unique.

## Usage

``` r
check_input(
  arginput,
  argtype,
  length = NULL,
  categories = NULL,
  interval = NULL,
  colnames = NULL,
  coltypes = NULL,
  unique = FALSE
)
```

## Arguments

- arginput:

  (`character`)  
  Input argument to be checked. Users can provide multiple inputs to be
  checked within a single call to this function by providing all inputs
  as a list (e.g., `arginput = list(input1, input2)`). However, this
  only works if all input arguments (e.g., input1 AND input2) are
  supposed to meet the same criteria (e.g., both should be numeric
  within interval 0-10).

- argtype:

  (`character`)  
  Required type of input argument based on
  [`class()`](https://rdrr.io/r/base/class.html). Example type(s) users
  can specify:

  - `"logical"`

  - `"character"`

  - `"numeric"` (or `"integer"` if specifically checking for integers)

  - `"data.table"`

  - `"data.frame"`

  - `"DBI" | "dbcon" | "PostgreSQL"` for DB connection input

  - `"list"`

  - `"Date"`, `"POSIXct"`, `"POSIXt"`

  - ...

  If an input object can be one of several acceptable types (e.g.,
  `data.table` OR `data.frame`), types should be provided as a character
  vector (e.g., `argtype = c("data.frame", "data.table")`).

  If `argtype` is `"integer"`, the tests will pass

  1.  if `class(input) == "integer"` or

  2.  if `class(input) == "numeric"` and the number is an integer

  If `argtype` is `"numeric"`, inputs that are of class `"integer"` will
  also pass. In other words, integers are treated as a special case of
  numeric in the case of `argtype`. Therefore, checks with
  `argtype = c("integer", "numeric")` (i.e., input should be either
  integer *or* numeric) are not meaningful and should be avoided.
  Instead, users should specify if inputs need to be an `"integer"`
  specifically (`argtype = "integer"`), or if they just need to be any
  `"numeric"` input (`argtype = "numeric"`).

- length:

  (`numeric`)  
  Optional input specifying the expected length of a given input
  argument (e.g., use `length = 2` to check if a vector/list contains 2
  elements).

- categories:

  (`character`)  
  Optional input if argtype is `"character"`. Character vector
  specifying acceptable categories for character inputs (e.g.,
  `categories = c("none", "all")`)

- interval:

  (`numeric`)  
  Optional input if argtype is `"numeric"` or `"integer"`. Numeric
  vector specifying acceptable range for numeric inputs (e.g.,
  `interval = c(1,100)`, or for non-negative numbers:
  `interval = c(0, Inf)`). Note that `interval` specifies a closed
  interval (i.e., end points are included).

- colnames:

  (`character`)  
  Optional input if argtype is `"data.frame"` or `"data.table"`.
  Character vector specifying all columns that need to exist in the
  input table (e.g., `colnames = c("genc_id", "discharge_date_time")`).

- coltypes:

  (`character`)  
  Optional input if argtype is `"data.frame"` or `"data.table"`.
  Character vector specifying required data type of each column in
  `colnames` (e.g., `coltypes = c("integer", "character")`) where the
  order of the vector elements should correspond to the order of the
  entries in `colnames`. If a column can have multiple acceptable types,
  types should be separated by `|` (e.g.,
  `coltypes = c("integer|numeric", "character|POSIXct")`)). For any
  columns that do not have to be of a particular type, simply specify as
  `""` (e.g., `coltypes = c("integer|numeric", "")`).

  Note: As opposed to `argtype`, `coltypes` need to strictly correspond
  to the type that is returned by `class(column)`. That means that type
  `"integer"` is *not* a special case of `"numeric"`, but is treated as
  a separate type. This is relevant for `genc_id` columns, which are of
  class `"integer"`, and therefore `coltype = "numeric"` will return an
  error.

- unique:

  (`logical`)  
  Optional input if argtype is `"data.frame"` or `"data.table"`. Flag
  indicating whether all rows in the provided input table need to be
  distinct.

## Value

  
If any of the input checks fail, function will return error message and
stop execution of parent `Rgemini` function. Otherwise, function will
not return anything.

## Examples

``` r
if (FALSE) { # \dontrun{
my_function <- function(input1 = TRUE, # logical
                        input2 = 2, # numeric
                        input3 = 1.5, # numeric
                        input4 = data.frame(
                          genc_id = as.integer(5),
                          discharge_date_time = Sys.time(),
                          hospital_num = 1
                        )) {
  # check single input
  check_input(input1, "logical")

  # check multiple inputs that should be of same type/meet same criteria
  check_input(
    arginput = list(input2, input3), argtype = "numeric",
    length = 1, interval = c(1, 10)
  )

  # check table input (can be either data.frame or data.table)
  check_input(input4,
    argtype = c("data.table", "data.frame"),
    colnames = c("genc_id", "discharge_date_time", "hospital_num"),
    coltypes = c("integer", "character|POSIXct", ""),
    unique = TRUE
  )
}

# will not result in any errors (default inputs are correct)
my_function()

# will result in an error
my_function(input1 = 1) # input 1 has to be logical
} # }
```
