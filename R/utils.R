
#' Imports for the entire package
#' Doesn't require Depends or `@import` per function
#'
#' @rawNamespace import(data.table, except = c("first", "last", "between", "month", "hour", "quarter", "week", "year", "wday", "second", "minute", "mday", "yday", "isoweek"))
#' @rawNamespace import(dplyr, except = c("first", "last", "between", "matches"))
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
#' c('x', 'y') %ni% 'x'
#'
`%ni%` <- function(x, y) {
  Negate(`%in%`)(x, y)
}


#' @title
#' Count missing
#'
#' @description
#' This function checks the number of missingness in a vector, returns the results in the exact number (percentage)
#'
#' @param x (`vector`)
#'
#' @export
count_missing <- function(x) {
  result <- sum(is.na(x) | x == "")
  myperc(result, length(x), 0, 1)
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

lunique <- function(x){
  length(unique(x))
}


#' @title
#' Coerce to `data.table`
#'
#' @description
#' Some `Rgemini` functions rely on `data.table` operations and assume the input is provided
#' in `data.table` format. If it  is not, coerce with message to ensure the function logic
#' works without breaking.
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

  if (!is.data.table(data)) {
    data <- as.data.table(data)
    warning(var, " was passed as a data.frame and has been coerced to a data.table", immediate. = TRUE)
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
#' suffix (e.g., "admdad" vs. "admdad_subset"). Specifically, for most table
#' names, the function uses `grepl("^tablename", drm_table)` to look for table
#' names that *start with* the same name as specified in DRM (e.g., 'admdad').
#' Exceptions are the "lab" and "transfusion" tables. Because there are other
#' tables with similar names (e.g., "transfusion_mapping" table), the function
#' specifically looks for tables called either "lab"/"transfusion" or
#' "lab_subset"/"transfusion_subset" (for HPC datacuts).
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
#'                         dbname = "db",
#'                         host = "172.XX.XX.XXX",
#'                         port = 1234,
#'                         user = getPass("Enter user:"),
#'                         password = getPass("password"))
#'
#' admdad_name <- find_db_tablename(dbcon, "admdad")
#'
#' # query identified table
#' admdad <- dbGetQuery(dbcon, paste0("select * from ", admdad_name, ";"))
#' }
#'
find_db_tablename <- function(dbcon, drm_table, verbose = TRUE) {

  ## Check if table input is supported
  if (!drm_table %in% c("admdad", "ipdiagnosis", "ipintervention", "ipcmg", "lab", "transfusion")) {
    stop("Invalid user input for argument drm_table.
          Currently, only the following table names are supported:
         'admdad', 'ipdiagnosis', 'ipintervention', 'ipcmg', 'lab', or 'transfusion'")
  }

  ## Define search criteria for different tables
  search_fn <- function(table_names, table = drm_table) {

    if (drm_table %in% c("lab", "transfusion")) {
      # for lab & transfusion table table:
      # check for specific table names lab/lab_subset and transfusion/transfusion_subset
      # (otherwise, lab/transfusion_mapping or other tables might be returned)
      res <- table_names[table_names %in% c(table, paste0(table, "subset"))]

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
  if (length(table_name) == 0){
    tables <- dbGetQuery(dbcon, "SELECT table_name from information_schema.tables
                                 WHERE table_type='FOREIGN' and table_schema='public';")$table_name
    table_name <- search_fn(tables)
  }

  ## Get unique value (some DBs have duplicate table names)
  table_name <- unique(table_name)

  ## Check returned value
  # get DB name
  db_name <- dbGetQuery(dbcon, "SELECT current_database()")$current_database

  # error if no table found
  if (length(table_name) == 0){
    stop(paste0("No table corresponding to '", drm_table, "' identified in database '", db_name,  "'.
                 Please make sure your database contains the relevant table."))
  }

  # error if more than 1 table found
  if (length(table_name) > 1){
    stop(paste0("Multiple tables corresponding to '", drm_table, "' identified in database '", db_name,  ": ", paste0(table_name, collapse = ", "), ".
                 Please ensure that the searched table name results in a unique match."))
  }

  ## show identified table
  if (verbose) {
    cat(paste0("\nThe following table in '", db_name,
               "' was found to match the DRM table name '",
               drm_table, "': '", table_name, "'\n "))
  }

  return(table_name)
}



#' @title
#' Check user inputs
#'
#' @description
#' Function checking whether user-provided input objects are appropriate.
#' Checks for the following:
#' - For all inputs: Whether input is of correct class (e.g., `logical`,
#' `numeric`, `character` etc.)
#' - For `numeric` inputs: Check whether provided input is within acceptable
#' range (e.g., > 0).
#' - For `character` (categorical) inputs: Check whether input corresponds to
#' one of acceptable categories.
#' - For `data.table|data.frame` inputs: 1) Check whether required columns exist
#' in table and 2) whether each column is of required type (optional)
#'
#' @param arginput (`character`)\cr
#' Input argument to be checked.
#'
#' @param argclass (`character`)\cr
#' Acceptable class(es) of input object. Has to be one of the following:
#' - `"logical"`
#' - `"character"`
#' - `"numeric"` (or `"integer"` if specifically checking for integers)
#' - `"data.table"`
#' - `"data.frame"`
#' - `"DBI" | "dbcon" | "PostgreSQL"` for DB connection input
#'
#' If an input object can be one of several acceptable classes (e.g.,
#' `data.table` OR `data.frame`), available options should be provided as a
#' character vector (e.g., `argclass = c('data.frame', 'data.table')`).
#'
#'
#' @return \cr
#' If any of the input checks fail, function will return error message and
#' execution of called `Rgemini` function will be stopped.
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
check_input <- function(arginput, argclass,
                        options = NULL, # for character inputs only
                        range = NULL, # for numeric inputs only
                        colnames = NULL, colclass = NULL) { # for data.table/data.frame inputs only

  argname <- deparse(substitute(arginput)) # get name of input argument

  ## Define new function to check for integers
  # (Note: base R `is.integer` does not return TRUE if class = numeric)
  is.integer <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


  ##### CHECK 1 (for all inputs): Check if class is correct
  ## For DB connections
  if (any(grepl("dbi|con|posgre|sql", argclass, ignore.case = TRUE))){
    if (!RPostgreSQL::isPostgresqlIdCurrent(arginput) & !grepl("PostgreSQL", class(arginput)[1])){
      stop(
        paste0("Invalid user input in '", sys.calls()[[1]], "': '",
               argname,"' needs to be a valid database connection.\n",
               "\nWe recommend the following method to establish the connection:\n",
               "drv <- dbDriver('PostgreSQL')\n",
               "dbcon <- DBI::dbConnect(drv, dbname = 'db_name', host = 'XXX.XX.XX.net', port = 1234, user = getPass('Enter user:'), password = getPass('password'))\n",
               "\nPlease refer to the function documentation for more details."),
        call. = FALSE
      )
    }

  ## For all other inputs
  } else if ((argclass == "integer" & !is.integer(arginput)) |
             argclass != "integer" & !any(class(arginput) %in% argclass)){
    stop(
      paste0("Invalid user input in '", sys.calls()[[1]], "': '",
             argname,"' needs to be of type '", paste(argclass, collapse = "' or '"), "'.",
             "\nPlease refer to the function documentation for more details."),
      call. = FALSE
    )
  }


  ##### CHECK 2 (for character inputs): Check if option is one of acceptable alternatives [optional]
  if (argclass == "character" & !is.null(options)){
    if (any(!arginput %in% options)){
      stop(
        paste0("Invalid user input in '", sys.calls()[[1]], "': '",
               argname,"' needs to be either '", paste0(paste(options[1:length(options)-1], collapse = "', '"), "' or '", options[length(options)]), "'.",
               "\nPlease refer to the function documentation for more details."),
        call. = FALSE
      )
    }
  }

}

