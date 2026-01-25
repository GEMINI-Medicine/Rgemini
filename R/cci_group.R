#' @title
#' Grouping CCI intervention codes
#'
#' @description
#' The Canadian Classification of Health Interventions (CCI) codes provides a
#' detailed documentation of all in-patient intervention in Canada.
#'
#' The `cci_group()` function categorises inputted CCI codes into their
#' anatomical groupings/ type of intervention for codes with available groupings
#' found within the 2022 CIHI document/ CIHI website
#' (https://www.cihi.ca/en/overview-of-cci-sections-and-code-ranges).
#'
#' @section User Input
#' This function prompts the user to one column of CCI codes and outputs a table
#' with a column named `Groupings`.
#'
#' @param user_table (`data.frame` | `data.table`)
#' Column of table with CCI codes of interest.
#'
#' @returns `data.table`
#' This function returns a table containing the CCI codes of interest, together
#' with their corresponding definitions.
#' For each row in the output table, the following variables are returned 
#' within column `Groupings`
#' - `anatomy_site` outputs where the the intervention occured if none availabe, outputs 'NA'. Only available for codes in Sections 1, 2, 3, 5, 6.
#' - `agent_type` outputs the agent type; if none available, outputs 'NA'. Only available for codes in Section 1.
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
#' }
#' a <- cci_filter(cci_lookup)
#'
#' @export
##install.packages(c("roxygen2", "data.table", "readxl", "dplyr"))
library(RPostgreSQL)
library(getPass)
library(roxygen2)
library(readxl)
library(dplyr)
library(data.table)
library(getPass)
drv <- dbDriver("PostgreSQL")
dbcon <- DBI::dbConnect(drv,
  dbname = "drm_cleandb_v3_1_0",
  host = "prime.smh.gemini-hpc.ca",
  port = 5432,
  user = getPass("Username:"),
  password = getPass("Enter Password:")
)

  # ALL CODES
cci_lookup <- dbGetQuery(dbcon, "SELECT * FROM lookup_cci;") %>% as.data.frame()

cci_group <- function(dbcon, sample) {
  cci_table <- read_excel("/mnt/nfs/projects/research_projects/Summer_Students/Alice/CCI_lookup.xlsx") %>%
    as.data.frame()

  df <- data.frame(
    cci_code = character(),
    anatomy_site = character(),
    agent_type = character(),
    stringsAsFactors = FALSE
  )

  cci_codes <- as.data.table(sample)
  cci_codes$Section <- substr(cci_codes$sample, 1, 1)
  cci_codes$Group <- substr(cci_codes$sample, 2, 3)
  cci_codes$Qualifier2 <- substr(cci_codes$sample, 8, 9)
  cci_table_g <- cci_table %>% filter(Field == "Group")
  cci_table_q <- cci_table %>% filter(Field == "Qualifier2")
  for (i in 1:nrow(cci_codes)) {
    section_val <- as.character(cci_codes[i, "Section"])
    group_val <- as.character(cci_codes[i, "Group"])
    q2_val <- as.character(cci_codes[i, "Qualifier2"])
    desc1 <- cci_table_g %>%
      filter(Category == section_val, ID == group_val) %>%
      pull(Grouping) %>%
      as.character()
    # SOMETHING IS WRONG HERE!!
    desc2 <- cci_table_q %>%
      filter(Category == section_val, ID == q2_val) %>%
      pull(Grouping) %>%
      as.character()
    if (length(desc1) == 0) {
      desc1 <- NA
    }
    if (length(desc2) == 0 || identical(desc2, "Miscellaneous")) {
      desc2 <- NA
    }
    single_code <- cci_codes[i, 1]
    df <- rbind(
      df,
      data.frame(cci_code = single_code, anatomy_site = desc1, agent_type = desc2)
    )
  }
  return(df)
}
subs <- cci_lookup %>% 
  sample_n(1000) %>%
  as.data.frame()
subs <- subs$intervention_code
print(subs)
View(subs)
