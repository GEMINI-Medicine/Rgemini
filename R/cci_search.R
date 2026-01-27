#' @title
#' Filtering and Identify CCI intervention codes
#'
#' @description
#' The [Canadian Classification of Health Interventions (CCI) codes](https://www.cihi.ca/en/cci-coding-structure)
#' provide detailed documentation of all in-patient interventions in Canada.
#' CCI codes are alphanumeric codes with 5-10 characters, where a given character
#' defines the following features:
#' 1) Section (1 character): Broad realm of interventions (e.g., 1 = physical/physiological therapeutic interventions)
#' 2) Group (2 characters): Subgroups within each Section, e.g., based on anatomy site (e.g., SC = spinal vertebrae) 
#' 3) Intervention (2 characters): Procedure/intervention types within a given Section (e.g., 75 = fusion)
#' 4) Qualifier 1 (2 characters): Optional details, such as approach/technique used (e.g., LL = using open anterior approach) 
#' 5) Qualifier 2 (2 characters): Optional details, such as devices used/implanted (e.g., KD = fixation device)
#' 6) Qualifier 3 (1 character): Optional details, such as tissues used (e.g., A = autograft)
#'
#' The codes are structured hierarchically and the specific groups, interventions, and qualifiers vary across Sections. 
#'
#' The `cci_search()` function provides a step-by-step search allowing users to extract a
#' subset of CCI codes matching the Section, Group(s), Intervention(s), and/or Qualifier(s)
#' that are of interest for a particular study.
#'
#' At each step, the user is shown available categories and their corresponding descriptions,
#' which can be searched and sorted in the viewer pane. The user is then prompted to provide
#' the entries they want to include in the terminal. In some cases, broader drop-down menus are
#' shown allowing users to select multiple categories at once (e.g., all interventions targeting
#' the nervous system).
#'
#' @section User Input:
#' This function prompts the user to input IDs through the R console. For all
#' prompts except Section:
#' - Enter one or more IDs separated by commas (e.g., `"AA, AB, AC"`).
#' - Press `Enter` without typing anything to select ALL available IDs.
#' - Type `"none"` to exclude all IDs for that category. This input will lead to
#' an empty output table unless selecting for Qualifiers 2 or 3.
#' - Type `"quit"` to exit the filtering process early.
#'
#' @param user_table (`data.frame` | `data.table`)
#' Table containing all interventions of interest, where one corresponds to
#' CCI codes of patients. Please make sure the table contains a column named
#' `intervention_code` (`character`).
#'
#' @returns `data.table`
#' This function returns a table containing the CCI codes of interest, together
#' with their corresponding definitions.
#' For each row in the output table, the following variables are returned:
#' - `intervention_code`: CCI code that matches the filtration criteria
#' - `CCI_description`: description of the CCI code
#'
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
#' a <- cci_search(cci_lookup)
#'
#' @export
cci_search <- function(dbcon) {
  # LOOKUP TABLE
  cci_table <- read_excel("/mnt/nfs/projects/research_projects/Summer_Students/Alice/CCI_lookup.xlsx")  %>%
    as.data.frame()
  # ALL CODES
  cci_lookup <- dbGetQuery(dbcon, "SELECT * FROM lookup_cci;") %>%
    as.data.frame()
  # SEPARATE LOOKUP TABLES
  list_sections <- cci_table %>%
    filter(Field == "Section") %>%
    mutate(ID = as.integer(as.character(ID))) %>%
    select(ID, Description)
  # Separating the substrings
  cci_codes <- as.data.table(cci_lookup)
  cci_codes$Section <- substr(cci_codes$intervention_code, 1, 1)
  cci_codes$Group <- substr(cci_codes$intervention_code, 2, 3)
  cci_codes$Intervention <- substr(cci_codes$intervention_code, 4, 5)
  cci_codes$Qualifier1 <- substr(cci_codes$intervention_code, 6, 7)
  cci_codes$Qualifier2 <- substr(cci_codes$intervention_code, 8, 9)
  cci_codes$Qualifier3 <- substr(cci_codes$intervention_code, 10, 10)
  View(cci_codes)

  ####### Load correct IDs in lookup table #######
  find_sect <- function(cci_codes) {
    cli_h1("CCI Filtering")
    cli_h2("Select Section")
    print(htmlwidgets::prependContent(
      reactable(
        list_sections,
        columns = list(
          ID = colDef(
            name = "ID",
            width = 50,
            defaultSortOrder = "asc"
          )
        ),
        searchable = TRUE,
        sortable = TRUE,
        pagination = TRUE,
        striped = TRUE,
        highlight = TRUE,
        resizable = TRUE),
      htmltools::tags$h1("Section number"),
      htmltools::tags$h3("This number dictates the values of the rest of the code")
    ))
    input <- readline("Enter ID of the SECTION you want to select or type 'quit' to exit: ")
    # Invalid input, prompt user to input again until correct
    if (input == "quit") {
      return("quit")
    } else if (input == "") {
      print("You must select one ID.")
      return(find_sect(cci_codes))
    } else if (!(input %in% list_sections$ID)) {
      print("Section not valid. Please enter an ID found in the Table.")
      return(find_sect(cci_codes))
    }
    code_sect <- as.integer(as.character(input))
    sect_lookup <- cci_table %>% filter(Category == code_sect)
    # Remove non-section
    cci_codes <- cci_codes[Section == code_sect]
    View(cci_codes)
    # Inputs of other values
    for (category in categories) {
      inp_list <- find_vals(c(category), sect_lookup, code_sect, cci_codes)
      if (is.character(inp_list) && length(inp_list) == 1 && identical(inp_list, "quit")) {
        return("quit")
      }
      cci_codes <- remove_rows(inp_list, cci_codes, category)
      if (nrow(cci_codes) == 0) {
        print("No codes satisfy this criteria.")
        return("quit")
      }
    }
    return(cci_codes)
  }
  ####### Input IDs and Check Inputs #######
  find_vals <- function(category, sect_lookup, code_sect, cci_codes) {
    cli_h2(paste("Select, ", category))
    #### Reactable tables ###
    # Reactable table for dropdown table
    if (code_sect < 6 && category == "Group" || code_sect == 1 && category == "Qualifier2") {
      relavent_ids <- sect_lookup %>%
        filter(Field == category) %>%
        filter(ID %in% cci_codes[[category]]) %>%
        select(ID, Description, Include, Grouping, Placeholder, ATC_Codes)
      dropdown(relavent_ids, code_sect, category)
      input <- readline(paste("Enter ID(s) of the", toupper(c(category)),"you want to select. Press Enter to select all IDs or type 'none' for no IDs in this category. Enter the Placeholder ID to select all IDs in the grouping.Separate IDs with a coma:"))
      # Validating inputs
      if (input == "quit") {
        return("quit")
      } else if (input == "none") {
        return("none")
      }
      inp_list <- unique(toupper(trimws(strsplit(input, ",")[[1]])))
      i <- 1
      temp <- relavent_ids %>%
        mutate(Placeholder = as.integer(as.character(Placeholder)))
      while (i <= length(inp_list)) {
        item <- inp_list[i]
        if (is.numeric(item)) {
          item <- as.integer(item)
        }
        if (!(item %in% relavent_ids$ID) && !(item %in% temp)){
          print("Please input valid ID")
          return(find_vals(category, sect_lookup, code_sect, cci_codes))
        } else if (item %in% relavent_ids$Placeholder) {
          matching_ids <- relavent_ids$ID[relavent_ids$Placeholder == item]
          inp_list <- append(inp_list[-i], matching_ids, after = i - 1)
        }
        i <- i + 1
      }
      print(inp_list)
      return(unique(inp_list))
    }

    # Non drop down Reactable Tables
    if (category == "Intervention") {
      target_ids <- as.integer(as.character(cci_codes[[category]]))
      relavent_ids <- sect_lookup %>%
        filter(Field == category) %>%
        mutate(ID = as.integer(as.character(ID))) %>%
        filter(ID %in% target_ids) %>%
        select(ID, Description, Include)
    } else {
      relavent_ids <- sect_lookup %>%
        filter(Field == category) %>%
        filter(ID %in% cci_codes[[category]]) %>%
        select(ID, Description, Include)
    }
    print(htmlwidgets::prependContent(
      reactable(
        relavent_ids %>% select(ID, Description, Include),
        columns = list(
          ID = colDef(
            name = "ID",
            width = 50,
            defaultSortOrder = "asc"
          )
        ),
        searchable = TRUE,
        sortable = TRUE,
        pagination = FALSE,
        striped = TRUE,
        highlight = TRUE,
        resizable = TRUE),
      htmltools::tags$h1(c(category)),
      htmltools::tags$h3("figure out what to type")
    ))
    input <- readline(paste("Enter ID(s) of the", toupper(c(category)),"you want to select. Press Enter to select all IDs or type 'none' for no IDs in this category. Separate IDs with a coma: "))
    # Validating Inputs
    if (input == "quit") {
      return("quit")
    } else if (input == "none") {
      return("none")
    } else if (category == "Intervention") {
      inp_list <- trimws(strsplit(input, ",")[[1]])
      inp_list <- sprintf("%02d", as.integer(inp_list))
      relavent_ids$ID <- sprintf("%02d", as.integer(trimws(as.character(relavent_ids$ID))))
    } else {
      inp_list <- unique(toupper(trimws(strsplit(input, ",")[[1]])))
    }
    for (item in inp_list) {
      if (!(item %in% relavent_ids$ID)) {
        print("Please input valid ID")
        return(find_vals(category, sect_lookup, code_sect, cci_codes))
      }
    }
    return(unique(inp_list))
  }
  ####### Remove irrelevant CCI codes #######
  remove_rows <- function(inp_list, cci_codes, category) {
    if (length(inp_list) == 0) {
      print(paste("All", c(category), " IDs selected."))
      return(cci_codes)
    } else if (is.character(inp_list) && length(inp_list) == 1 && inp_list == 'none') {
      cci_codes <- cci_codes[cci_codes[[category]] == "" | is.na(cci_codes[[category]]), ]
    } else {
      cci_codes <- cci_codes[cci_codes[[category]] %in% inp_list, ]
    }
    View(cci_codes)
    return(cci_codes)
  }

  ####### Dropdown menu for the IDs and Descriptions #######
  dropdown <- function(relavent_ids, code_sect, category) {
    broad_groups <- relavent_ids %>%
      mutate(
        display_group_text = ifelse(is.na(Grouping), Description, Grouping),
        display_group_code = ifelse(is.na(Placeholder), ID, Placeholder)) %>%
      # Makes sure category dont overlap
      distinct(display_group_code, display_group_text) %>%
      mutate(display_group_code = as.integer(display_group_code))
    # Reactable broad groupings
    print(htmlwidgets::prependContent(
      reactable(
        broad_groups,
        columns = list(
          display_group_code = colDef(
            name = "ID",
            width = 50,
            defaultSortOrder = "asc"
          ),
          display_group_text = colDef(name = "Broader Categories")
        ),
        searchable = TRUE,
        sortable = TRUE,
        pagination = FALSE,
        striped = TRUE,
        highlight = TRUE,
        resizable = TRUE,
        onClick = "expand",
        # Reactable Dropdown menus
        details = function(index) {
          selected <- broad_groups$display_group_code[index]
          if (category == "Group") {
            sub_data <- relavent_ids %>%
              mutate(display_group_code = ifelse(is.na(Placeholder), ID, Placeholder)) %>%
              filter(display_group_code == selected) %>%
              select(ID, Description, Include)
          } else {
            sub_data <- relavent_ids %>%
              mutate(display_group_code = ifelse(is.na(Placeholder), ID, Placeholder)) %>%
              filter(display_group_code == selected) %>%
              select(ID, Description, Include, ATC_Codes)
          }
          reactable(
            sub_data,
            searchable = TRUE,
            sortable = TRUE,
            pagination = FALSE,
            striped = TRUE,
            highlight = TRUE,
            resizable = TRUE,
            columns = list(
              ID = colDef(name = "ID"),
              Description = colDef(name = "Description"),
              Include = colDef(name = "Include")
            )
          )
        }
      ),
      htmltools::tags$h1(paste(c(category))),
      htmltools::tags$h3("figure out what to type")
    ))
  }
  # Indexing
  categories <- list(
    "Group", "Intervention",
    "Qualifier1", "Qualifier2", "Qualifier3"
  )
  correct_codes <- find_sect(cci_codes)
  if (length(correct_codes) == 1 && correct_codes == "quit") {
    return(print("Ended."))
  }
  correct_codes$intervention_code <- trimws(toupper(as.character(correct_codes$intervention_code)))
  return(correct_codes[, c("intervention_code", "cci_long_title")])
}
