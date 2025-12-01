#' Imports for the entire package
#' Doesn't require Depends or `@import` per function
#'
#' @rawNamespace
#' import(data.table, except = c("first", "last", "between", "month", "hour",
#' "quarter", "week", "year", "wday", "second", "minute", "mday", "yday",
#' "isoweek"))
#' @rawNamespace
#' import(dplyr, except = c("first", "last", "between", "matches"))
#'
NULL

#' @title
#' Not In
#'
#' @description
#' Infix function for the opposite of `%in%`.
#'
#' @param x (`vector` or `NULL`)
#' The values to be matched. Long vectors are supported.
#'
#' @param y (`vector` or `NULL`)
#' The values to be matched against. Long vectors are not supported.
#'
#' @return (`logical`)
#' A vector of logical values the same length of `x`, for which each
#' value represents whether that particular element in `x` exists in `y`.
#'
#' @export
#'
#' @examples
#' c("x", "y") %ni% "x"
#'
`%ni%` <- function(x, y) {
  Negate(`%in%`)(x, y)
}


#' @title
#' Returns number of unique values
#'
#' @param x (`vector`)
#'
#' @export
#'
#' @examples
#' lunique(c(1, 1, 2, 2, 2, 3))
lunique <- function(x) {
  length(unique(x))
}


#' @title
#' Coerce to `data.table`
#'
#' @description
#' Some `Rgemini` functions rely on `data.table` operations and assume the input
#' is provided in `data.table` format. If it  is not, coerce with message to
#' ensure the function logic works without breaking.
#'
#' @param data (`data.frame` or `data.table`)\cr
#' The data to check class of and coerce to `data.table` if necessary.
#'
#' @return (`data.table`)\cr
#' The original data provided as an argument, but as a `data.table`.
#'
#' @export
#'
#' @examples
#' mtcars <- coerce_to_datatable(mtcars)
#'
coerce_to_datatable <- function(data) {
  var <- deparse(substitute(data))
  data <- copy(data) # take copy to avoid changing original table
  if (!is.data.table(data)) {
    data <- as.data.table(data)
    warning(var, " was passed as a data.frame and has been coerced to a data.table.\n", immediate. = TRUE)
  }
  return(data)
}

#' @title
#' Find DB table/view name in database.
#'
#' @description
#' Some `Rgemini` functions internally query DB tables. The table names cannot
#' be hard-coded in those functions since some tables in HPC datacuts have a
#' `_subset` suffix.
#' This function searches table names in the user-specified database that match
#' the DRM (Data Reference Model) table of interest.
#'
#' Currently, the function expects the relevant tables in all databases to only
#' differ based on their suffix (e.g., "ipintervention" vs.
#' "ipintervention_subset"). This strict search (as opposed to a more flexible
#' regex search) is used to allow for a broad range of table names to be
#' searched while avoiding false positive matches.
#'
#'
#' @section HPC datacuts with materialized views:
#' For HPC datacuts created from `gemini_h4h_template_v4_0_0` (or newer),
#' users only have access to materialized views and not tables. For these
#' datacuts, users need to set the schema right after establishing a DB
#' connection as follows:
#'
#' db <- DBI::dbConnect(drv,
#'    dbname = "db",
#'    host = "domain_name.ca",
#'    port = 1234,
#'    user = "user",
#'    password = getPass("Enter Password:"))
#'
#' dbSendQuery(db, "Set schema 'test_datacut'");
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param drm_table (`character`)\cr
#' Table name to be searched, based on the DRM (e.g., `"admdad"`, `"lab"`,
#' `"physicians"`, `"lookup_transfer"` etc.).
#'
#' Users need to specify the full DRM table name (e.g., `"admdad"` instead of
#' `"adm"`) to avoid potential confusion with other tables.
#'
#' @param verbose (`logical`)\cr
#' Whether or not to show a message indicating which DB table was found.
#'
#' @return (`character`)\cr
#' Returns the full name of the relevant DB table as a character.
#'
#' @import DBI
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass("Enter user:"),
#'   password = getPass("password")
#' )
#'
#' admdad_name <- find_db_tablename(dbcon, "admdad", verbose = TRUE)
#'
#' # query identified table
#' admdad <- dbGetQuery(dbcon, paste0("select * from ", admdad_name, ";"))
#' }
#'
find_db_tablename <- function(dbcon, drm_table, verbose = FALSE) {
  ## Check if table input is supported
  check_input(drm_table, "character")

  ## Define search criteria for different tables
  search_fn <- function(table_names, table = drm_table) {
    # check for DRM direct match with table name or table + _subset suffix
    # note: a previous version of this function used a more flexible regex
    # search, however, the table names are fairly fixed so we can use this
    # instead to avoid flase matches
    res <- table_names[table_names %in% c(table, paste0(table, "_subset"))]

    return(res)
  }

  # Check current schema_name from SQL
  schema_name <- dbGetQuery(dbcon, "SELECT current_schema();")

  # if there is schema_name is public that means no materialized view
  if (schema_name == "public") {
    ## Find all table names and run search as defined above
    tables <- dbListTables(dbcon)
    table_name <- search_fn(tables)

    ## If none found, might be due to DB versions with foreign data wrappers
    #  In that case try this:
    if (length(table_name) == 0) {
      tables <- dbGetQuery(
        dbcon,
        "SELECT table_name from information_schema.tables
      WHERE table_type='FOREIGN' and table_schema='public';"
      )$table_name
      table_name <- search_fn(tables)
    }
  } else { # This is when there are materialized views under a given schema
    dbSendQuery(dbcon, paste0("Set schema '", schema_name, "';")) # Set the right schema

    tables <- dbGetQuery(
      dbcon,
      "SELECT matviewname AS table_name,
       schemaname AS schema_name
    FROM pg_matviews;"
    )$table_name
    table_name <- search_fn(tables)
  }

  ## Get unique value (some DBs have duplicate table names)
  table_name <- unique(table_name)

  ## Check returned value
  # get DB name
  db_name <- dbGetQuery(dbcon, "SELECT current_database()")$current_database

  # error if no table found
  if (length(table_name) == 0) {
    stop(paste0(
      "No table corresponding to '", drm_table, " under schema '", schema_name,
      "' identified in database '", db_name, "'.
      Please make sure your database contains the relevant table/view."
    ))
  }

  # error if more than 1 table found
  if (length(table_name) > 1) {
    stop(paste0(
      "Multiple tables/views corresponding to '", drm_table, "' under schema '", schema_name,
      "' identified in database '", db_name, ": ",
      paste0(table_name, collapse = ", "), ".
      Please ensure that the searched table/view name results in a unique match."
    ))
  }

  ## show identified table
  if (verbose) {
    cat(paste0(
      "\nThe following table/view in '", db_name, "' under schema '", schema_name,
      "' was found to match the DRM table name/view '",
      drm_table, "': '", table_name, "'\n "
    ))
  }

  return(table_name)
}


#' @title
#' Return Hospital Field
#'
#' @description
#' To accommodate differences in column names between databases, find the name of the column corresponding to
#' the hospital for downstream queries.
#'
#' @param db (`DBIConnection`)\cr
#' RPostgres DB connection.
#'
#' @return (`character`)\cr
#' `hospital_id` or `hospital_num`, with preference given to `hospital_id` if it exists.
#'
return_hospital_field <- function(db) {
  admdad <- find_db_tablename(db, "admdad", verbose = FALSE)

  # find variable name corresponding to hospital identifier (hospital_id/hospital_num)
  # to do minimial changes to querying one row to get all the column names instead

  admdad_cols <- dbGetQuery(db, paste0("SELECT * from ", admdad, " limit 1;")) %>% data.table()

  fields <- colnames(admdad_cols)

  if ("hospital_id" %in% fields) {
    return("hospital_id")
  } else if ("hospital_num" %in% fields) {
    return("hospital_num")
  } else {
    error("A field corresponding to the hospital was not found.")
  }
}


#' @title
#' Check user inputs
#'
#' @description
#' Function checking whether user-provided inputs for a function are
#' appropriate. The following check is applied for all inputs:
#' - Whether input is of correct type (e.g., `logical`, `numeric`, `character`
#' etc.)
#' For some input types, the following additional checks can be applied
#' optionally:
#' - Check whether length of provided input is as expected
#' - For `numeric`/`integer` inputs: Check whether provided input is within
#' acceptable interval (e.g., between 1-100).
#' - For `character` (categorical) inputs: Check whether input corresponds to
#' one of acceptable categories.
#' - For `data.table|data.frame` inputs: 1) Check whether required columns exist
#' in table, 2) whether each column is of a specified type, and 3) whether all
#' entries are unique.
#'
#' @param arginput (`character`)\cr
#' Input argument to be checked. Users can provide multiple inputs to be checked
#' within a single call to this function by providing all inputs as a list
#' (e.g., `arginput = list(input1, input2)`). However, this only works if all
#' input arguments (e.g., input1 AND input2) are supposed to meet the same
#' criteria (e.g., both should be numeric within interval 0-10).
#'
#' @param argtype (`character`)\cr
#' Required type of input argument based on `class()`. Example type(s) users can
#' specify:
#' - `"logical"`
#' - `"character"`
#' - `"numeric"` (or `"integer"` if specifically checking for integers)
#' - `"data.table"`
#' - `"data.frame"`
#' - `"DBI" | "dbcon" | "PostgreSQL"` for DB connection input
#' - `"list"`
#' - `"Date"`, `"POSIXct"`, `"POSIXt"`
#' - ...
#'
#' If an input object can be one of several acceptable types (e.g.,
#' `data.table` OR `data.frame`), types should be provided as a character vector
#' (e.g., `argtype = c("data.frame", "data.table")`).
#'
#' If `argtype` is `"integer"`, the tests will pass
#' 1) if `class(input) == "integer"` or
#' 2) if `class(input) == "numeric"` and the number is an integer
#'
#' If `argtype` is `"numeric"`, inputs that are of class `"integer"` will also
#' pass. In other words, integers are treated as a special case of numeric in
#' the case of `argtype`. Therefore, checks with
#' `argtype = c("integer", "numeric")` (i.e., input should be either integer
#' *or* numeric) are not meaningful and should be avoided. Instead, users should
#' specify if inputs need to be an `"integer"` specifically
#' (`argtype = "integer"`), or if they just need to be any `"numeric"` input
#' (`argtype = "numeric"`).
#'
#' @param length (`numeric`)\cr
#' Optional input specifying the expected length of a given input argument
#' (e.g., use `length = 2` to check if a vector/list contains 2 elements).
#'
#' @param categories (`character`)\cr
#' Optional input if argtype is `"character"`.
#' Character vector specifying acceptable categories for character inputs (e.g.,
#' `categories = c("none", "all")`)
#'
#' @param interval (`numeric`)\cr
#' Optional input if argtype is `"numeric"` or `"integer"`.
#' Numeric vector specifying acceptable range for numeric inputs (e.g.,
#' `interval = c(1,100)`, or for non-negative numbers: `interval = c(0, Inf)`).
#' Note that `interval` specifies a closed interval (i.e., end points are
#' included).
#'
#' @param colnames (`character`)\cr
#' Optional input if argtype is `"data.frame"` or `"data.table"`.
#' Character vector specifying all columns that need to exist in the input table
#' (e.g., `colnames = c("genc_id", "discharge_date_time")`).
#'
#' @param coltypes (`character`)\cr
#' Optional input if argtype is `"data.frame"` or `"data.table"`.
#' Character vector specifying required data type of each column in `colnames`
#' (e.g., `coltypes = c("integer", "character")`) where the order of the vector
#' elements should correspond to the order of the entries in `colnames`.
#' If a column can have multiple acceptable types, types should be separated by
#' `|` (e.g., `coltypes = c("integer|numeric", "character|POSIXct")`)). For any
#' columns that do not have to be of a particular type, simply specify as `""`
#' (e.g., `coltypes = c("integer|numeric", "")`).
#'
#' Note: As opposed to `argtype`, `coltypes` need to strictly correspond to the
#' type that is returned by `class(column)`. That means that type `"integer"` is
#' *not* a special case of `"numeric"`, but is treated as a separate type. This
#' is relevant for `genc_id` columns, which are of class `"integer"`, and
#' therefore `coltype = "numeric"` will return an error.
#'
#' @param unique (`logical`)\cr
#' Optional input if argtype is `"data.frame"` or `"data.table"`. Flag
#' indicating whether all rows in the provided input table need to be distinct.
#'
#' @return \cr
#' If any of the input checks fail, function will return error message and stop
#' execution of parent `Rgemini` function. Otherwise, function will not return
#' anything.
#'
#' @examples
#' \dontrun{
#' my_function <- function(input1 = TRUE, # logical
#'                         input2 = 2, # numeric
#'                         input3 = 1.5, # numeric
#'                         input4 = data.frame(
#'                           genc_id = as.integer(5),
#'                           discharge_date_time = Sys.time(),
#'                           hospital_num = 1
#'                         )) {
#'   # check single input
#'   check_input(input1, "logical")
#'
#'   # check multiple inputs that should be of same type/meet same criteria
#'   check_input(
#'     arginput = list(input2, input3), argtype = "numeric",
#'     length = 1, interval = c(1, 10)
#'   )
#'
#'   # check table input (can be either data.frame or data.table)
#'   check_input(input4,
#'     argtype = c("data.table", "data.frame"),
#'     colnames = c("genc_id", "discharge_date_time", "hospital_num"),
#'     coltypes = c("integer", "character|POSIXct", ""),
#'     unique = TRUE
#'   )
#' }
#'
#' # will not result in any errors (default inputs are correct)
#' my_function()
#'
#' # will result in an error
#' my_function(input1 = 1) # input 1 has to be logical
#' }
#'
check_input <- function(arginput, argtype,
                        length = NULL,
                        categories = NULL, # for character inputs only
                        interval = NULL, # for numeric inputs only
                        colnames = NULL, # for data.table/.frame inputs only
                        coltypes = NULL, #          "-"
                        unique = FALSE) { #          "-"


  ## Get argument names and restructure input
  if (any(class(arginput) == "list")) {
    # Note: Users can provide multiple arginputs to be checked by combining them
    # into a list ...or they might want to check an arginput that is supposed to
    # be a list itself
    # Here: We infer which one it is based on deparse(substitute)
    # If arginput is provided as a single input name:
    # -> assume input itself is supposed to be a list
    # If each list item corresponds to a separate argument name
    # -> assume user wants to check individual items
    # it's a bit hacky but seems to work for tested scenarios
    argnames <- sapply(substitute(arginput), deparse)[-1]
    if (length(argnames) < 1) {
      argnames <- deparse(substitute(arginput))
      arginput <- list(arginput = arginput)
    }
  } else {
    # get name of argument
    argnames <- deparse(substitute(arginput))

    # turn arginput into list (for Map function below to work)
    arginput <- list(arginput = arginput)
  }


  ## Define new function to check for integers
  #  Note: base R's `is.integer` does not return TRUE if type == numeric
  #  Note 2: For coltypes check below, this function is not used
  #  (instead coltypes are checked for whether class(column) returns "integer")
  is_integer <- function(x) {
    if (is.numeric(x)) {
      tol <- .Machine$double.eps
      return(abs(x - round(x)) < tol)
    } else {
      return(FALSE)
    }
  }


  ## Function defining all input checks
  run_checks <- function(arginput, argname) {
    ###### CHECK 1 (for all input types): Check if type is correct
    ## For DB connections
    if (any(grepl("dbi|con|posgre|sql", argtype, ignore.case = TRUE))) {
      if (inherits(arginput, "OdbcConnection") || !grepl("PostgreSQL", class(arginput)[1])) {
        stop(
          paste0(
            "Invalid user input in '",
            as.character(sys.calls()[[1]])[1], "': '",
            argname, "' needs to be a valid PostgreSQL database connection.\n",
            "Database connections established with `odbc` are currently not supported.\n",
            "Instead, please use the following method to establish a DB connection:\n",
            "drv <- dbDriver('PostgreSQL')\n",
            "dbcon <- DBI::dbConnect(drv, dbname = 'db_name', ",
            "host = 'domain_name.ca', port = 1234, ",
            "user = getPass('Enter user:'), password = getPass('password'))\n",
            "\nPlease refer to the function documentation for more details."
          ),
          call. = FALSE
        )
      } else if (!RPostgreSQL::isPostgresqlIdCurrent(arginput)) {
        # if PostgreSQL connection, make sure it's still active
        stop(
          paste0(
            "Please make sure your database connection is still active.\n",
            "You may need to reconnect to the database if the connection has timed out."
          ),
          call. = FALSE
        )
      }

      ## For all other inputs
    } else if ((any(argtype == "integer") && !all(is_integer(arginput))) ||
      (!any(argtype == "integer") && !any(class(arginput) %in% argtype) &&
        (!(any(argtype == "numeric") &&
          all(is_integer(arginput)))))) { # in case argtype is "numeric" and provided input is "integer", don't show error
      stop(
        paste0(
          "Invalid user input in '", as.character(sys.calls()[[1]])[1], "': '",
          argname, "' needs to be of type '", paste(argtype,
            collapse = "' or '"
          ), "'.",
          "\nPlease refer to the function documentation for more details."
        ),
        call. = FALSE
      )
    }


    ###### CHECK 2: Check if length of input argument is as expected [optional]
    if (!is.null(length)) {
      if (length(arginput) != length) {
        stop(
          paste0(
            "Invalid user input in '", as.character(sys.calls()[[1]])[1],
            "': '", argname, "' needs to be of length ", length,
            "\nPlease refer to the function documentation for more details."
          ),
          call. = FALSE
        )
      }
    }


    ###### CHECK 3 (for character inputs):
    ###### Check if option is one of acceptable alternatives [optional]
    if (any(argtype == "character") && !is.null(categories)) {
      if (any(!arginput %in% categories)) {
        stop(
          paste0(
            "Invalid user input in '", as.character(sys.calls()[[1]])[1],
            "': '", argname, "' needs to be either '", paste0(
              paste(categories[seq_along(categories) - 1], collapse = "', '"),
              "' or '", categories[length(categories)]
            ), "'.",
            "\nPlease refer to the function documentation for more details."
          ),
          call. = FALSE
        )
      }
    }


    ###### CHECK 4 (for numeric/integer inputs):
    ###### Check if number is within acceptable interval [optional]
    if (any(argtype %in% c("numeric", "integer")) && !is.null(interval)) {
      if (any(arginput < interval[1]) || any(arginput > interval[2])) {
        stop(
          paste0(
            "Invalid user input in '", as.character(sys.calls()[[1]])[1],
            "': '", argname, "' needs to be within closed interval [",
            interval[1], ", ", interval[2], "].",
            "\nPlease refer to the function documentation for more details."
          ),
          call. = FALSE
        )
      }
    }


    ###### CHECK 5 (for data.table/data.frame inputs):
    ###### Check if nrow() > 0 & if relevant columns exist [optional]
    if (any(argtype %in% c("data.frame", "data.table")) && !is.null(colnames)) {
      if (nrow(arginput) == 0) {
        stop(
          paste0(
            "Invalid user input in '", as.character(sys.calls()[[1]])[1],
            "': '", argname, "' input table has 0 rows.",
            "\nPlease carefully check your input."
          ),
          call. = FALSE
        )
      }

      # get missing columns
      missing_cols <- setdiff(colnames, colnames(arginput))

      if (length(missing_cols) > 0) {
        stop(
          paste0(
            "Invalid user input in '", as.character(sys.calls()[[1]])[1],
            "': '", argname, "' input table is missing required column(s) '",
            paste0(missing_cols, collapse = "', '"), "'.",
            "\nPlease refer to the function documentation for more details."
          ),
          call. = FALSE
        )
      }
    }


    ###### CHECK 6 (for data.table/data.frame inputs):
    ###### Check if required columns are of correct type [optional]
    if (any(argtype %in% c("data.frame", "data.table")) && !is.null(coltypes)) {
      # for simplicity of error output:
      # only show first column where incorrect type was found (if any)
      # ignore coltypes without specification ("")
      check_col_type <- function(col, coltype) {
        if (coltype != "" && !any(grepl(coltype,
          class(as.data.table(arginput)[[col]]),
          ignore.case = TRUE
        ))) {
          stop(
            paste0(
              "Invalid user input in '", as.character(sys.calls()[[1]])[1],
              "': '", col, "' in input table '", argname,
              "' has to be of type '", coltype, "'.",
              "\nPlease refer to the function documentation for more details."
            ),
            call. = FALSE
          )
        }
      }
      mapply(check_col_type, colnames, coltypes)
    }


    ###### CHECK 7 (for data.table/data.frame inputs):
    ###### Check if all rows are distinct [optional]
    if (any(argtype %in% c("data.frame", "data.table")) && unique == TRUE) {
      if (any(duplicated(arginput))) {
        stop(
          paste0(
            "Invalid user input in '", as.character(sys.calls()[[1]])[1], "': ",
            "Input table '", argname, "' has to contain unique rows.",
            "\nPlease check for duplicate entries and ",
            "refer to the function documentation for more details."
          ),
          call. = FALSE
        )
      }
    }
  }


  ### Run checks on all input arguments (if multiple)
  check_all <- Map(run_checks, arginput, argnames)
}


#' @title
#' Mapping Message
#'
#' @description
#' Message to display to inform the user that the function being used relies on
#' GEMINI SME mapped values.
#'
#' @param what (`character`)\cr
#' Which values were mapped.
#'
#' @param addtl (`character`)\cr
#' An additional, specific message to append to the generic message.
#'
#' @return
#' Prints a message to the console.
#'
mapping_message <- function(what, addtl = NULL) {
  msg <- paste0(
    "\n***Note:***\nThe output of this function is based on manual mapping of ", what,
    " by a GEMINI Subject Matter Expert.\n",
    "Please carefully check mapping coverage for your cohort of interest, or contact the GEMINI team if you require additional support.\n",
    addtl
  )

  cat(msg)
}

#' @title
#' Coverage Message
#'
#' @description
#' Message to display to inform the user that the function being used does not
#' check clinical data coverage.
#'
#' @param what (`character`)\cr
#' Which clinical table is used.
#'
#' @param addtl (`character`)\cr
#' An additional, specific message to append to the generic message.
#'
#' @return
#' Prints a message to the console.
#'
coverage_message <- function(what, addtl = NULL) {
  msg <- paste0(
    "\n***Note:***\nThis function does not check ", what,
    " data coverage for the input cohort. It returns 0 for any genc_ids with no record\nin ",
    what, " table. The result 0s may not be appropriate for patients who are not within ",
    what, " data coverage period(s). \nPlease carefully check ", what,
    " data coverage for your cohort, or contact the GEMINI team if you require additional support.\n",
    addtl
  )

  cat(msg)
}

#' @title
#' Convert date-time variables into POSIXct/POSIXt format.
#'
#' @description
#' This function converts date-time variables into a user-specified format.
#' `convert_dt` is called by other `Rgemini` functions to make sure that
#' any date-time inputs are in the expected order (typically `"ymd HM"`). It
#' can also be used independently of other `Rgemini` functions to clean up
#' date-times for analyses.
#' The function additionally returns warning messages about missing/invalid
#' date-time entries that can be useful for debugging.
#'
#' @param dt_var \cr
#' A vector containing the date-time values to be converted to the specified
#' format. The vector can be of any class that can be converted to date-times by
#' `lubridate::parse_date_time()` (usually "character" or "POSIXct"). If
#' users have already pre-processed date-time variables into POSIXct (e.g.,
#' using `lubridate::ymd_hm()`) prior to running the function,
#' `convert_dt()` will simply return the original `dt_var`.
#'
#' @param orders (`character`)\cr
#' Order(s) to be used to parse `dt_var`, such as `"ymd HM"` (default),
#' `"ymd HMS"`, `"ymd"` etc. Will be passed to `lubridate::parse_date_time()`,
#' which accepts date-time formats without expecting "%" prefix (see `orders`
#' input argument in \link[lubridate]{parse_date_time}.
#'
#' Multiple acceptable formats can be specified by providing a character vector,
#' e.g. `format = c("ymd HM", "ymd HMS", "ymd")`.
#'
#' If expecting a date format in "ymd" (regardless of whether timestamp exists
#' or not), use `convert_dt(dt_var, orders = "ymdHMS", truncated = 3)`.
#'
#' @param check_ts_zeros (`logical`)\cr
#' Whether to check for timestamps "00:00", which may represent missing times
#' for some variables. This is only user species any `orders` containing time
#' information, e.g., `orders = c("ymd HM", "ymd HMS")`.
#'
#' @param dt_varname (`character`)\cr
#' Name of variable referring to user-provided date-time variable. This is only
#' used for warning messages to improve clarity. Usually, `dt_varname` can be
#' inferred directly based on the provided input, but in some cases where
#' inputs are passed between multiple functions, it can be helpful to specify
#' the variable name explicitly (for example, see `stat_holidays_ON()`.
#'
#' @param addtl_msg (`character`)\cr
#' Additional warning message to be shown if any missing/invalid date-time
#' entries were found. If `addtl_msg = NULL`, the following message will be
#' shown by default:
#' "Please carefully consider how to deal with missing/invalid date-time entries
#' and perform any additional pre-processing prior to running the function
#' \[function_name\] (e.g., impute missing dates/timestamps etc.)."
#'
#' @param ...
#' Additional parameters passed to `lubridate::parse_date_time()`, such as
#' `truncated` , `exact`, etc.
#'
#' @return (`POSIXct` | `POSIXt`)\cr
#' Returns converted `dt_var` parsed according to specified date-time `orders`.
#'
#' Will also return the following warning messages (if applicable):
#' 1) Number (%) of entries in `dt_var` that are missing (NA, "", " ")
#' 2) Number (%) of entries in `dt_var` that could not be parsed into specified
#' format
#' 3) For formats containing (non-optional) timestamps: Number (%) of entries in
#' `dt_var` that only contain date information
#'
#' Any missing/invalid date-time entries will be returned as `NA` by this
#' function.
#'
#' @import lubridate
#'
#' @export
#'
#' @examples
#' my_date_time <- c("2020-01-01 12:00", "2021-03-06 09:25")
#' convert_dt(my_date_time, orders = "ymd HM")
convert_dt <- function(dt_var,
                       orders = "ymd HM",
                       check_ts_zeros = FALSE,
                       dt_varname = NULL,
                       addtl_msg = NULL,
                       ...) {
  ## initialize all counts of missing/invalid entries
  n_missing_dt <- n_invalid_dt <- n_date_only <- n_zeros <- 0

  ## get name of date-time variable if not explicitly specified
  if (is.null(dt_varname)) {
    dt_varname <- deparse(substitute(dt_var))
  }

  ## Check 1: Missing entries (NA, "", " ")
  n_missing_dt <- sum(n_missing(dt_var, na_strings = c("", " "), index = TRUE))
  if (n_missing_dt > 0) {
    warning(
      paste0(
        n_missing(dt_var, na_strings = c("", " ")), " entries in variable `",
        dt_varname, "` are missing (NA, \"\", or \" \")."
      ),
      immediate. = TRUE, call. = FALSE
    )
  }
  # remove any missing entries (to be excluded from denominators for checks below)
  dt_var_non_missing <- dt_var[!n_missing(dt_var, na_strings = c("", " "), index = TRUE)]

  ## convert to correct format
  if (!any(grepl("POSIX", class(dt_var)))) {
    # suppress generic warning from parse_date_time here and show more
    # informative warning below instead
    dt_var_res <- lubridate::parse_date_time(dt_var, orders = orders, quiet = TRUE, ...)

    ## Check 2: Entries that can't be parsed according to specified format
    n_invalid_dt <- sum(is.na(dt_var_res)) - n_missing_dt
    if (n_invalid_dt > 0) {
      warning(
        paste0(
          n_invalid_dt, " (", round(100 * n_invalid_dt / length(dt_var_non_missing), 1), "%)",
          " of all non-missing entries in variable `", dt_varname,
          "` could not be parsed with date-time order(s): \"",
          paste(orders, collapse = "\", \""), "\"."
        ),
        immediate. = TRUE, call. = FALSE
      )
    }

    ## Check 3: Check if timestamp exists
    #  Will only be checked if expected format contains time ("HM"/"HMS" etc.)
    #  and `truncated` (if specified) wouldn't make time stamp optional
    #  Date-only inputs like "2020-01-01" would fail check

    # check if truncation should be applied to order characters
    args <- list(...)
    orders_trunc <- if (!is.null(args$truncated)) {
      substr(orders, 1, nchar(orders) - args$truncated)
    } else {
      orders
    }

    if (all(grepl("hm", orders_trunc, ignore.case = TRUE))) {
      # if user provided non-POSIXct input, check characters for timestamp info
      ts_regex <- "([0-1][0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?"
      n_date_only <- sum(!grepl(ts_regex, as.character(dt_var))) - n_missing_dt
      if (n_date_only > 0) {
        warning(
          paste0(
            n_date_only, " (", round(100 * n_date_only / length(dt_var_non_missing), 1), "%)",
            " of all non-missing entries in variable `", dt_varname,
            "` could not be parsed due to missing timestamps."
          ),
          immediate. = TRUE, call. = FALSE
        )
      }
    }
  } else {
    # if date-time variable was already pre-processed into POSIXct/POSIXt by
    # user, return variable as is
    dt_var_res <- dt_var
  }

  ## Check 4 (optional): Check how many have timestamp 00:00
  #  Will only be checked if any expected format contains time ("HM"/"HMS" etc.)
  #  Any entries with "00:00"/"00:00:00" after date component will fail.
  #  Check will be performed regardless of any truncation if check_ts_zeros = T
  if (any(grepl("hm", orders, ignore.case = TRUE))) {
    if (check_ts_zeros == TRUE) {
      n_zeros <- sum(
        grepl(" 00:00", as.character(dt_var_non_missing)) | nchar(as.character(dt_var_non_missing)) < 12
      )
      if (n_zeros > 0) {
        warning(
          paste0(
            n_zeros, " (", round(100 * n_zeros / length(dt_var_non_missing), 1), "%)",
            " of all non-missing entries in variable `",
            dt_varname, "` have timestamp \"00:00/00:00:00\". ",
            "Please consider if these entries may represent missing timestamps."
          ),
          immediate. = TRUE, call. = FALSE
        )
      }
    }
  }

  # show general warning about missing/invalid date-times
  # (can be customized by providing `addtl_msg`)
  if (any(c(n_missing_dt, n_invalid_dt, n_date_only, n_zeros) > 0)) {
    if (is.null(addtl_msg) || !addtl_msg %in% c("", " ", "\n")) {
      warning(
        ifelse(is.null(addtl_msg),
          paste0(
            "Please carefully consider how to deal with missing/invalid date-time",
            " entries and perform any additional pre-processing prior to running",
            " the function `", as.character(sys.calls()[[1]])[1],
            "` (e.g., impute missing dates/timestamps etc.).\n"
          ),
          addtl_msg
        ),
        immediate. = TRUE, call. = FALSE
      )
    } else {
      cat("\n")
    }
  }

  return(dt_var_res)
}


#' Fix variable strings
#'
#' @description
#' Removes any "_" from variable names and applies title case capitalization.
#' This is currently used for axis labels/titles in plotting functions.
#'
#' @param str (`character`)
#' Character string to be cleaned up
#'
fix_var_str <- function(str) {
  str <- tools::toTitleCase(gsub("[_.]", " ", str))
}

#' @title Compare two sets to get the number of unique and common elements in each set
#' @description
#' This function takes in two vectors and returns the number
#' of overlapping elements, along with the number of unique elements.
#' The function can compare sets of characters, numerics, and dates.
#' @param x (`vector`) \cr
#' The first set to be compared
#' @param y (`vector`) \cr
#' The second set to be compared
#' @param dates (`logical`)\cr
#' If set to TRUE, will attempt to convert x & y into dates, and then compare. Set to FALSE by default.
#' @param orders (`vector`)\cr
#' Allows user to specify potential date or date-time formats they would like
#' to compare. The function will check for `"ymd"`, `"ymd HM"`, and `"ymd HMS"`
#' by default.
#'
#' For example, users can set `orders = "dmy"` to compare vectors in `dmy`
#' format.
#'
#' @return (`data.table`)\cr
#' A table showing the number of elements in both vectors, in the first vector only, and in the second vector only
#'
#' @export
#'
#' @examples
#' compare_sets(c(1:10), c(5:10), dates = FALSE)
compare_sets <- function(x,
                         y,
                         dates = FALSE,
                         orders = c("ymd", "ymd HM", "ymd HMS")) {
  if (dates == TRUE) {
    x <- convert_dt(x, orders = orders)
    y <- convert_dt(y, orders = orders)
  }

  in_both <- length(intersect(x, y))
  x_only <- length(unique(setdiff(x, y)))
  y_only <- length(unique(setdiff(y, x)))
  data.table(in_both, x_only, y_only)
}

#' @title Suppress errors/messages/warnings
#'
#' @description
#' Run function without showing any errors/warnings/printed messages.
#'
#' Note that certain messages (e.g., from RPostgreSQL) cannot be suppressed.
#'
#' @param func
#' Function to be run quietly
#'
#' @export
quiet <- function(func) {
  sink(tempfile(), type = "out")
  on.exit(sink())
  invisible(force(suppressMessages(suppressWarnings(func))))
}


#' @title
#' Create N-tiles
#'
#' @description
#' This function bins continuous variables into quantiles
#' (quartiles, deciles, etc.) of the user's choosing. Bins are
#' created according to the interval `(low breakpoint, high breakpoint]`.
#' If individual values are tied around a breakpoint, they will be grouped
#' into the same (lower) bin, unlike dplyr::ntile(), which ignores ties to
#' create equally-sized bins.
#' Note: The function will quit if any breakpoints are
#' duplicated (e.g. 1st quartile identical to 2nd quartile).
#'
#' @param x (`vector`)\cr
#' A vector of numeric values.
#'
#' @param n (`integer`)\cr
#' The number of bins to create. For example, for quartiles set `n=4`.
#'
#' @return (`factor`)\cr
#' A factor with `n` levels representing the bins (ntiles) of the input values.
#' The factor returned will be the same length as `x`.
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' values <- rnorm(100)
#' quartiles <- create_ntiles(values, 4)
#' deciles <- create_ntiles(values, 10)
#'
create_ntiles <- function(x, n) {
  ## check that x is numeric
  check_input(
    x, "numeric"
  )

  ## check that n is positive integer between 2-Inf
  check_input(
    n, "integer",
    interval = c(2, Inf)
  )

  ## create breaks
  probs <- seq(0, 1, length.out = n + 1)

  ## split into quantiles
  ntiles <- quantile(x, probs = probs)

  ## check for duplicates
  if (anyDuplicated(ntiles)) {
    percentile_names <- paste0(probs * 100, "%")
    formatted_ntiles <- paste(percentile_names, ntiles, sep = ": ", collapse = "\n")
    stop(paste("Duplicated percentiles detected:\n", formatted_ntiles))
  }

  ## assign labels for ntiles
  results <- cut(x, breaks = ntiles, include.lowest = TRUE, labels = seq_len(n))

  ## display ntiles
  print(ntiles)

  ## return output
  return(results)
}


#' Normalize string values
#'
#' This function performs a series of text cleaning and normalization on text data.
#' For example, it is used in `prepare_pharm_for_validation()` to clean up RxNorm outputs for validation.
#' The operations include:
#' - Converts text to ASCII encoding, such that special characters can be intepreted properly by R.
#' - Converts all characters to lowercase.
#' - Trims leading and trailing whitespace.
#' - Removes leading and trailing periods.
#' - Optional: Converts to the singular form through lemmatization.
#' @param x (`character`)\cr
#' Text input to be normalized. Multiple values can be provided as a character vector.
#' @param lemma (`logical`)\cr
#' If set to `TRUE`, singularizes x via `textstem::lemmatize_words()`.
#' For use in the RxNorm workflow: Note that lemmatization is an NLP technique that may not always convert drug
#' names or classification terms into their respective singular form, but this processing is still helpful to a
#' certain extent to help minimize variations due to plural forms.
#' @importFrom textstem lemmatize_words
#' @importFrom stringi stri_trans_general
#' @export
#' @examples
#' normalize_text(" Ámoxícíllíns. ")
normalize_text <- function(x, lemma = FALSE) {
  # Convert special characters to ASCII equivalents, cross-platform
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- trimws(x)
  x <- gsub("^\\.|\\.$", "", x)
  if (lemma == TRUE) {
    x <- textstem::lemmatize_words(x)
  }
  return(x)
}

#' @title
#' Sample a truncated log normal distribution
#'
#' @description
#' Sample from a log normal distribution using the `rlnorm` function
#' Truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param meanlog (`numeric`) The mean of the log normal distribution
#'
#' @param sdlog (`numeric`) The standard deviation of the log normal distribution
#'
#' @param min (`numeric`) The minimum value to truncate the data to.
#'
#' @param max (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the log normal distribution, truncated to the specified range.
#'
#' @export
#'
rlnorm_trunc <- function(n, meanlog, sdlog, min, max, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (any(min > max)) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  res <- rlnorm(n, meanlog, sdlog)
  # keep redrawing until all are in range
  # get out of range values
  oor <- (res < min) | (res > max)
  while (any(oor)) {
    res[oor] <- rlnorm(sum(oor, na.rm = TRUE), meanlog, sdlog)
    oor <- (res < min) | (res > max)
  }
  return(res)
}

#' @title
#' Sample a truncated normal distribution
#'
#' @description
#' Sample from a normal distribution using the `rnorm` function but truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param mean (`numeric`) The mean of the normal distribution
#'
#' @param sd (`numeric`) The standard deviation of the normal distribution
#'
#' @param min (`numeric`) The minimum value to truncate the data to.
#'
#' @param max (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the normal distribution, truncated to the specified range.
#'
#' @export
#'
rnorm_trunc <- function(n, mean, sd, min, max, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (any(min > max)) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  res <- rnorm(n, mean, sd)
  while (sum(res < min) + sum(res > max) > 0) {
    res[c(res < min | res > max)] <- rnorm(
      sum(res < min) + sum(res > max),
      mean,
      sd
    )
  }
  return(res)
}

#' @title
#' Sample a truncated skewed normal distribution
#'
#' @description
#' Sample from a skewed normal distribution using the `rsn` function
#' Truncate it to specified minimum and maximum values
#'
#' @param n (`integer`) The length of the output vector
#'
#' @param xi (`numeric`) The center of the skewed normal distribution
#'
#' @param omega (`numeric`) The spread of the skewed normal distribution
#'
#' @param alpha (`numeric`) The skewness of the skewed normal distribution
#'
#' @param min (`numeric`) The minimum value to truncate the data to.
#'
#' @param max (`numeric`) The maximum value to truncate the data to.
#'
#' @param seed (`integer`) Optional, a number for setting the seed for reproducible results
#'
#' @return A numeric vector following the skewed normal distribution, truncated to the specified range.
#'
#' @importFrom sn rsn
#' @export
#'
rsn_trunc <- function(n, xi, omega, alpha, min, max, seed = NULL) {
  # checks for input validity
  if (any(min > max)) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }
  # first sampling of results
  res <- rsn(n = n, xi = xi, omega = omega, alpha = alpha)
  if (n == 1) {
    # if only one number is sampled
    while (res[1] < min || res[1] > max) {
      res <- rsn(n = 1, xi = xi, omega = omega, alpha = alpha)
    }
    return(res[1])
  } else {
    # re-sample until all values are in the specified range
    while (sum(res < min) + sum(res > max) > 0) {
      res[c(res < min | res > max)] <- rsn(
        n = sum(res < min) + sum(res > max),
        xi = xi,
        omega = omega,
        alpha = alpha
      )
    }
  }
  return(res)
}

#' @title
#' Chopped, skewed normal distribution for time variables
#'
#' @description
#' The function samples from a skewed normal distribution using `rsn` to obtain time of day data in hours.
#' Values greater than 24 are subtracted by 24 (moved to the next day) so that a real time variable is observed.
#'
#' @param nrow (`integer`) The number of data points to sample
#'
#' @param xi (`numeric`) The center of the skewed normal distribution
#'
#' @param omega (`numeric`) The spread of the skewed normal distribution
#'
#' @param alpha (`numeric`) The skewness of the skewed normal distribution
#'
#' @param min (`numeric`) Optional, a minimum value to left truncate the value to; the default value is set to 0.
#'
#' @param max (`numeric`) Optional, a maximum value to right truncate the value to; the default value is set to 48.
#'
#' @param seed (`integer`) Optional, an integer for setting the seed for reproducible results.
#'
#'
#' @return A numeric vector following the specified distribution.
#'
#' @export
#'
sample_time_shifted <- function(nrow, xi, omega, alpha, min = 0, max = 48, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (min > max) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  # sampling of skewed normal distribution
  time_orig <- rsn_trunc(
    n = nrow,
    xi = xi,
    omega = omega,
    alpha = alpha,
    min = min,
    max = max,
  )
  # times greater than 24 hours are after 12am
  # subtract 25 to turn 12am into 00:00
  final_time <- ifelse(time_orig >= 24,
    time_orig - 24,
    time_orig
  )
  return(final_time)
}


#' @title
#' Chopped log normal distribution for time variables
#'
#' @description
#' The function samples from a log normal using `rlnorm` to obtain time of day data in hours.
#' Values greater than 24 are subtracted by 24 (moved to the next day) so that a real time variable is observed.
#'
#' @param nrow (`integer`) The number of data points to sample
#'
#' @param meanlog (`numeric`) The log mean for the distribution
#'
#' @param sdlog (`numeric`) The log standard deviation
#'
#' @param min (`numeric`) Optional, a minimum value to left truncate the value to; the default value is set to 0.
#'
#' @param max (`numeric`) Optional, a maximum value to right truncate the value to; the default value is set to 48.
#'
#' @param seed (`integer`) Optional, an integer for setting the seed for reproducible results.
#'
#'
#' @return A numeric vector following the specified distribution.
#'
#' @export
#'
sample_time_shifted_lnorm <- function(nrow, meanlog, sdlog, min = 0, max = 48, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (min > max) {
    stop("The min is greater than the max. Invalid sampling range provided - stopping.")
  }
  sample_dist <- function(nrow, meanlog, sdlog) {
    # sampling of skewed normal distribution
    time_orig <- rlnorm(
      n = nrow,
      meanlog = meanlog,
      sdlog = sdlog
    )
    # times greater than 24 hours are after 12am
    # subtract 25 to turn 12am into 00:00
    final_time <- ifelse(time_orig >= 24,
      time_orig - 24,
      time_orig
    )
    return(final_time)
  }
  res <- sample_dist(nrow, meanlog, sdlog)
  while (sum(res < min) + sum(res > max) > 0) {
    oor_sum <- sum(res < min) + sum(res > max)
    res[c(res < min || res > max)] <- sample_dist(oor_sum, meanlog, sdlog)
  }
  return(res)
}


#' @title
#' Generate a data table with basic inpatient stay information.
#' At the minimum, it will include an encounter and hospital ID,
#' along with other information if `cohort` is included in the input.
#'
#' @description
#' This function creates a data table of simulated encounter IDs and hospital IDs.
#' The creation is either based on user's desired number of encounters and unique hospitals,
#' or based on a given set of encounter IDs. It can be used to create long format data tables
#' where users have control over average repeat frequency.

#'
#' @param nid (`integer`)\cr Optional, number of unique encounter IDs to simulate
#'
#' @param n_hospitals (`integer`)\cr Optional, number of hospitals to simulate and assign to encounter IDs
#'
#' @param avg_repeats (`numeric`)\cr The average number of repeats per row in the final data table
#'
#' @param include_prop (`numeric`)\cr A number between 0 and 1,
#' for the proportion of unique rows in `cohort` to include in the final data table
#'
#' @param cohort (`data.table`)\cr Optional, resembling the GEMINI "admdad" table to build the returned data table from
#'
#' @param by_los (`logical`)\cr Optional, whether to assign more repeats to longer hospital stays or not.
#' Default to FALSE. When TRUE, two additional columns are required in the input `cohort` dataset -
#' `admission_date_time` and `discharge_date_time` for calculating length of stay.
#'
#' @param seed (`integer`)\cr Optional, a number for setting the seed for reproducible results
#'
#' @return (`data.table`)\cr A data.table object with the same columns as `cohort`,
#' but with some rows excluded and/or repeated based on user specifications.
#' If `cohort` is not included, then it will have the following fields:
#' - `genc_id` (`integer`): GEMINI encounter number, may be repeated in multiple rows based on avg_repeats
#' - `hospital_num` (`integer`): An integer identifying the hospital attached to the encounter
#'
#' @export
#'
#' @examples
#' sample_cohort <- data.table::data.table(genc_id = 1:100, hospital_num = rep(1:5, each = 20))
#' generate_id_hospital(cohort = sample_cohort, include_prop = 0.8, avg_repeats = 1.5, by_los = TRUE, seed = 1)
#' generate_id_hospital(nid = 1000, n_hospitals = 10, avg_repeats = 1)
#'
generate_id_hospital <- function(
  nid = 1000, n_hospitals = 10, avg_repeats = 1.5, include_prop = 1, cohort = NULL, by_los = FALSE, seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(cohort)) {
    # if cohort is not provided, create data.table out of parameters
    hosp_names <- seq(1:n_hospitals)
    hosp_assignment <- sample(hosp_names, nid, replace = TRUE) # this creates randomness in hospital size

    # simulate number of repeats for each id (from Poisson)
    if (avg_repeats != 1) {
      n_repeats <- rpois(nid, lambda = avg_repeats) # random sample number of repeats for each id
      n_repeats[n_repeats == 0] <- 1
    } else {
      n_repeats <- rep(1, nid)
    }
    # expand ids and sites
    id_list <- 1:nid
    id_vector <- rep(id_list, times = n_repeats)
    site_vector <- rep(hosp_assignment, times = n_repeats)

    res <- data.table(genc_id = id_vector, hospital_num = site_vector, stringsAsFactors = FALSE)
  } else {
    include_set <- cohort[sample(seq_len(nrow(cohort)), round(include_prop * nrow(cohort))), ]

    if (avg_repeats == 1) {
      n_repeats <- rep(1, nrow(include_set))
    } else {
      # sample rows with repeats
      n_repeats <- rpois(nrow(include_set), lambda = avg_repeats)
      n_repeats[n_repeats == 0] <- 1
    }

    # may sort by LOS to assign more repeats to longer stays
    if (by_los) {
      # convert date times to a useable format
      tryCatch(
        {
          include_set$admission_date_time <- Rgemini::convert_dt(include_set$admission_date_time, "ymd HM")
        },
        warning = function(w) {
          stop(conditionMessage(w))
        }
      )

      tryCatch(
        {
          include_set$discharge_date_time <- Rgemini::convert_dt(include_set$discharge_date_time, "ymd HM")
        },
        warning = function(w) {
          stop(conditionMessage(w))
        }
      )

      include_set$los <- as.numeric(difftime(
        include_set$discharge_date_time,
        include_set$admission_date_time,
        units = "hours"
      ))
      # order from shortest to longest
      include_set <- include_set[order(los)]
      n_repeats <- sort(n_repeats)

    }
    # In both cases, assign repeats
    res <- include_set[rep(seq_len(.N), times = n_repeats), ]
  }

  res[, genc_id := as.integer(genc_id)]
  res[, hospital_num := as.integer(hospital_num)]
  return(res)
}
