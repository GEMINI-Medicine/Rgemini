# Find DB table/view name in database.

Some `Rgemini` functions internally query DB tables. The table names
cannot be hard-coded in those functions since some tables in HPC
datacuts have a `_subset` suffix. This function searches table names in
the user-specified database that match the DRM (Data Reference Model)
table of interest.

Currently, the function expects the relevant tables in all databases to
only differ based on their suffix (e.g., "ipintervention" vs.
"ipintervention_subset"). This strict search (as opposed to a more
flexible regex search) is used to allow for a broad range of table names
to be searched while avoiding false positive matches.

## Usage

``` r
find_db_tablename(dbcon, drm_table, verbose = FALSE)
```

## Arguments

- dbcon:

  (`DBIConnection`)  
  A database connection to any GEMINI database.

- drm_table:

  (`character`)  
  Table name to be searched, based on the DRM (e.g., `"admdad"`,
  `"lab"`, `"physicians"`, `"lookup_transfer"` etc.).

  Users need to specify the full DRM table name (e.g., `"admdad"`
  instead of `"adm"`) to avoid potential confusion with other tables.

- verbose:

  (`logical`)  
  Whether or not to show a message indicating which DB table was found.

## Value

(`character`)  
Returns the full name of the relevant DB table as a character.

## HPC datacuts with materialized views

For HPC datacuts created from `gemini_h4h_template_v4_0_0` (or newer),
users only have access to materialized views and not tables. For these
datacuts, users need to set the schema right after establishing a DB
connection as follows:

db \<- DBI::dbConnect(drv, dbname = "db", host = "domain_name.ca", port
= 1234, user = "user", password = getPass("Enter Password:"))

dbSendQuery(db, "Set schema 'test_datacut'");

## Examples

``` r
if (FALSE) { # \dontrun{
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "db",
  host = "domain_name.ca",
  port = 1234,
  user = getPass("Enter user:"),
  password = getPass("password")
)

admdad_name <- find_db_tablename(dbcon, "admdad", verbose = TRUE)

# query identified table
admdad <- dbGetQuery(dbcon, paste0("select * from ", admdad_name, ";"))
} # }
```
