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
#' Find DB table name using regex search.
#'
#' @description
#' Some `Rgemini` functions internally query DB tables. The table names cannot
#' be hard-coded in those functions since HPC datacuts sometimes have slightly
#' different table names (e.g., `admdad` is called `admdad_subset` in some
#' datacuts). This function uses a simple regex search to identify the full
#' table name in a given DB that matches the DRM (Data Reference Model) table
#' name of interest.
#'
#' Currently, the function only supports a subset of table names (see below) and
#' expects the relevant tables in all databases to only differ based on their
#' suffix (e.g., "ipintervention" vs. "ipintervention_subset"). For some tables,
#' the function uses `grepl("^tablename", drm_table)` to look for table
#' names that *start with* the same name as specified in DRM (e.g., any that
#' start with "ipintervention").
#' For other tables, the function uses a stricter search to avoid finding
#' multiple matches: Specifically, for "admdad", "lab", and "transfusion", the
#' function tries to identify tables with the exact same name (i.e.,
#' "admdad/lab/transfusion") or the corresponding table name with a "_subset"
#' suffix (for HPC datacuts).
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database.
#'
#' @param drm_table (`character`)\cr
#' Table name to be searched, based on the DRM. Currently only accepts the
#' following inputs (which have been verified to work across different
#' DBs/datacuts):
#' - `"admdad"`
#' - `"ipdiagnosis"`
#' - `"ipintervention"`
#' - `"ipcmg"`
#' - `"transfusion"`
#' - `"lab"`
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
#' admdad_name <- find_db_tablename(dbcon, "admdad")
#'
#' # query identified table
#' admdad <- dbGetQuery(dbcon, paste0("select * from ", admdad_name, ";"))
#' }
#'
find_db_tablename <- function(dbcon, drm_table, verbose = TRUE) {
  ## Check if table input is supported
  check_input(drm_table, "character",
    categories = c(
      "admdad", "ipdiagnosis", "ipintervention", "ipcmg",
      "lab", "transfusion"
    )
  )

  ## Define search criteria for different tables
  search_fn <- function(table_names, table = drm_table) {

    if (drm_table %in% c("lab", "transfusion", "admdad")) {
      # for lab & transfusion table table:
      # check for specific table names lab/lab_subset and transfusion/transfusion_subset
      # (otherwise, lab/transfusion_mapping or other tables might be returned)
      res <- table_names[table_names %in% c(table, paste0(table, "_subset"))]
    } else {
      # for all other tables, simply search for names starting with search term
      res <- table_names[grepl(paste0("^", drm_table), table_names)]
    }

    return(res)
  }


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

  ## Get unique value (some DBs have duplicate table names)
  table_name <- unique(table_name)

  ## Check returned value
  # get DB name
  db_name <- dbGetQuery(dbcon, "SELECT current_database()")$current_database

  # error if no table found
  if (length(table_name) == 0) {
    stop(paste0(
      "No table corresponding to '", drm_table,
      "' identified in database '", db_name, "'.
      Please make sure your database contains the relevant table."
    ))
  }

  # error if more than 1 table found
  if (length(table_name) > 1) {
    stop(paste0(
      "Multiple tables corresponding to '", drm_table,
      "' identified in database '", db_name, ": ",
      paste0(table_name, collapse = ", "), ".
      Please ensure that the searched table name results in a unique match."
    ))
  }

  ## show identified table
  if (verbose) {
    cat(paste0(
      "\nThe following table in '", db_name,
      "' was found to match the DRM table name '",
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
  fields <- dbGetQuery(db, paste0("SELECT column_name FROM information_schema.columns WHERE table_name = '", admdad,"';"))$column_name

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
      if (!RPostgreSQL::isPostgresqlIdCurrent(arginput) &&
        !grepl("PostgreSQL", class(arginput)[1])) {
        stop(
          paste0(
            "Invalid user input in '",
            as.character(sys.calls()[[1]])[1], "': '",
            argname, "' needs to be a valid database connection.\n",
            "We recommend the following method to establish a DB connection:\n",
            "drv <- dbDriver('PostgreSQL')\n",
            "dbcon <- DBI::dbConnect(drv, dbname = 'db_name', ",
            "host = 'domain_name.ca', port = 1234, ",
            "user = getPass('Enter user:'), password = getPass('password'))\n",
            "\nPlease refer to the function documentation for more details."
          ),
          call. = FALSE
        )
      }

      ## For all other inputs
    } else if ((any(argtype == "integer") && !all(is_integer(arginput))) ||
      (!any(argtype == "integer") && !any(class(arginput) %in% argtype) &&
        (!(any(argtype == "numeric") && all(is_integer(arginput)))))) { # in case argtype is "numeric" and provided input is "integer", don't show error
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
              paste(categories[1:length(categories) - 1], collapse = "', '"),
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
    ###### Check if relevant columns exist [optional]
    if (any(argtype %in% c("data.frame", "data.table")) && !is.null(colnames)) {
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
  msg <- paste(
    "\n***Note:***\nThe output of this function is based on manual mapping of", what, "by a GEMINI Subject Matter Expert.\n",
    "Please carefully check mapping coverage for your cohort of interest, or contact the GEMINI team if you require additional support.\n",
    addtl
  )

  cat(msg)
}
