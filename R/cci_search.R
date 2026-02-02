#' @title
#' Identify relevant CCI codes for interventions of interest
#'
#' @description
#' The [Canadian Classification of Health Interventions (CCI) codes](https://www.cihi.ca/sites/default/files/document/guide-for-users-of-cci-manual-en.pdf) provide a
#' detailed classification of all inpatient interventions in Canada,
#' with more than 17,000 unique, alphanumeric codes.
#'
#' `cci_search()` faciliates the process of indentifying relevant CCI
#' codes for interventions of interest. For example, researchers may want
#' to identify all CCI codes related to diagnostic imaging performed on a
#' specific body region. The function enables this by breaking down
#' all existing CCI codes into their underlying components (see
#' hierarchical coding structure
#' [here](https://www.cihi.ca/en/cci-coding-structure), and providing a
#' step-by-step filtering approach allowing researchers to identify
#' CCI codes of interest.
#'
#' @details
#' CCI codes contain 5-10 alphanumeric characters defining the following
#' features:
#' 1) Section (1 character): Broad realm of interventions (e.g., 1 =
#' "physical/physiological therapeutic interventions")
#' 2) Group (2 characters): Target region or area of focus. In Sections 1-3,
#' this refers to anatomy site (e.g., SC = "spinal vertebrae") while in
#' others, it refers to stage of pregnancy (Section 5), mental/sensory
#' function (Section 6), intervention type (Section 7), or target
#' disease/organism (Section 8)
#' 3) Intervention (2 characters): Procedure/intervention types within a
#' given Section (e.g., 75 = "fusion")
#' 4) Qualifier 1 (2 characters): Optional details defining how (or why) the
#' intervention was performed. In Sections 3/4/7, Qualifier 1 is all that is
#' required to complete the CCI code. In other Sections (e.g., 1), it
#' represents only a part of the qualifier - the approach and technique
#' portion (e.g., LL = "using open anterior approach").
#' 5) Qualifier 2 (2 characters): Optional details, such as the tools, agents
#' or modalities used (e.g., KD = "fixation device")
#' 6) Qualifier 3 (1 character): Optional details defining the type of tissue
#' (e.g., A = "autograft") or group/strain of
#' an organism/antigen used during the intervention.
#'
#' The `cci_search()` function provides a step-by-step search allowing
#' users to extract a subset of CCI codes matching the Section, Group(s)
#' Intervention(s), and/or Qualifier(s) that are of interest for a
#' particular study.
#'
#' At each step, the user is shown available categories and their
#' corresponding descriptions, which can be searched and sorted in the
#' viewer pane. The user is then prompted to provide the entries they want
#' to include via the terminal. In some cases, broader drop-down menus are
#' shown allowing users to select multiple categories at once (e.g., all
#' interventions targeting the nervous system).
#'
#' @section Warning:
#' Note that CCI codes are structured in a nested hierarchy, where the
#' specific Groups, Interventions, and Qualifiers vary across Sections
#' (1st digit of CCI codes). The function therefore only allows users to
#' select a single Section during the first step of the filtering process.
#' If users want to search codes across different Sections, they need to run
#' this function multiple times - once per Section.
#'
#' @param dbcon (`DBIConnection`)\cr
#' A `DBI` database connection to any GEMINI database.
#' The function will query the `lookup_cci` table to identify all
#' unique CCI codes, which will then be filtered for the relevant codes
#' based on the step-by-step search criteria provided by the user via
#' the terminal.
#'
#' @returns `data.table`
#' This function returns a table containing the filtered CCI codes
#' matching the user's search criteria. For each row in the output table,
#' the following variables are returned:
#' - `intervention_code`: CCI code matching the search criteria
#' - `CCI_description`: description of the CCI code
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
#' cci_filtered <- cci_search(dbcon)
#' @references
#' https://www.cihi.ca/sites/default/files/document/guide-for-users-of-cci-manual-en.pdf
#' https://www.cihi.ca/en/cci-coding-structure
#'
#' @import htmlwidgets htmltools cli reactable
#' @importFrom tools toTitleCase
#' @importFrom rstudioapi sendToConsole
#' @export
cci_search <- function(dbcon) {
  # check input type and column name
  Rgemini:::check_input(dbcon, argtype = "DBI")

  ## CCI codes
  # Query lookup table to identify all unique CCI codes
  cci_codes <- dbGetQuery(dbcon, "SELECT intervention_code, cci_long_title FROM lookup_cci;") %>%
    data.table()
  # remove any special characters from intervention_codes
  cci_codes[, intervention_code := gsub("[^A-Za-z0-9]", "", intervention_code)]
  # remove CANCELLED code
  cci_codes <- cci_codes[!grepl("cancel", intervention_code, ignore.case = TRUE)]

  # Separate codes into their component fields
  cci_codes$section <- as.numeric(substr(cci_codes$intervention_code, 1, 1))
  cci_codes$group <- substr(cci_codes$intervention_code, 2, 3)
  cci_codes$intervention <- as.numeric(substr(cci_codes$intervention_code, 4, 5))
  cci_codes$qualifier1 <- substr(cci_codes$intervention_code, 6, 7)
  cci_codes$qualifier2 <- substr(cci_codes$intervention_code, 8, 9)
  cci_codes$qualifier3 <- substr(cci_codes$intervention_code, 10, 10)

  ## Read mapping data
  mapping_cci <- Rgemini::mapping_cci %>%
    data.table()
  # remove any leading or trailing white spaces
  mapping_cci[] <- lapply(mapping_cci, function(x) {
    if (is.character(x)) trimws(x) else x
  })
  mapping_cci[, field := tolower(field)]
  mapping_cci[mapping_cci == ""] <- NA

  # get sections
  list_sections <- mapping_cci %>%
    filter(field == "section") %>%
    mutate(ID = as.integer(as.character(idx))) %>%
    select(ID, description)

  ####### Utility functions #######
  ## get user input via terminal
  get_user_input <- function(
    category,
    numeric = FALSE,
    max_length = NULL,
    valid_inputs = NULL
  ) {
    if (is.null(max_length) || max_length > 1) {
      cat("If providing multiple values, separate IDs with a comma, or press ENTER to select all IDs.\n")
      prompt <- paste0("Enter ", toTitleCase(category), " ID(s): ")
    } else {
      prompt <- paste0("Enter ", toTitleCase(category), " ID: ")
    }

    input <- NULL
    Sys.sleep(0.1) # Brief pause to make sure table is printed first

    while (is.null(input)) {
      rstudioapi::sendToConsole("", execute = FALSE, focus = TRUE) # Returns focus to console
      input_raw <- readline(prompt = prompt)
      # Invalid input, prompt user to input again until correct
      if (grepl("quit", input_raw, ignore.case = TRUE)) {
        stop("Function exited by user.", call. = FALSE)
      } else {
        input <- trimws(strsplit(input_raw, ",")[[1]])

        if (numeric == TRUE) {
          input <- suppressWarnings(as.integer(input))
        }

        if ((!is.null(max_length) && max_length == 1 && all(is.na(input))) ||
          (!is.null(max_length) && length(input) > max_length) ||
          (!all(is.na(input)) && !is.null(valid_inputs) && !any(input %in% valid_inputs))) {
          input <- NULL
          warning(paste0(
            "User input not valid. Please enter a valid ", toTitleCase(category), " ID or type 'quit' to exit."
          ), immediate. = TRUE)
        }
      }
    }

    # if both numeric and non-numeric inputs are allowed, separate them
    if (numeric == FALSE) {
      input_all <- list()
      # extract numeric inputs
      input_all[["numeric"]] <- as.numeric(input[grepl("^[-+]?(\\d+\\.?\\d*|\\.\\d+)([eE][-+]?\\d+)?$", input)])

      # extract alphabetic inputs
      input_all[["character"]] <- input[
        grepl("^(?=.*[A-Za-z])[A-Za-z0-9_-]+$", input, perl = TRUE)
      ]

      input <- input_all
    }
    return(input)
  }

  ####### Step 1: Filter by Section #######
  cli_h1("CCI Filtering")
  cli_h2("Select SECTION")
  print(htmlwidgets::prependContent(
    reactable(
      list_sections,
      columns = list(
        ID = colDef(
          name = "ID",
          width = 50,
          defaultSortOrder = "asc"
        ),
        description = colDef(name = "Description", width = 300)
      ),
      searchable = TRUE,
      sortable = TRUE,
      pagination = TRUE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE
    ),
    htmltools::tags$h1("Section number"),
    htmltools::tags$h3("CCI Sections define the broad realm of interventions."),
    htmltools::tags$p("Only", htmltools::tags$strong("a single Section"), " can be selected since the subsequent filtering steps vary across Sections due to the nested hierarchical structure of CCI codes.")
  ))

  section_input <- get_user_input(
    category = "Section", numeric = TRUE,
    valid_input = list_sections$ID, max_length = 1
  )

  # remove any mappings/codes that are not in section of interest
  mapping_cci <- mapping_cci[section == section_input]
  cci_codes <- cci_codes[section == section_input]

  ####### Step 2-5: Filter by subsequent categories #######
  categories <- c(
    "group", "intervention",
    "qualifier1", "qualifier2", "qualifier3"
  )

  for (category in categories) {
    # category <- "qualifier3"
    definition <- paste(
      "CCI", category, "defines", case_when(
        category == "group" ~ "the region or area of focus.
        ",
        category == "intervention" ~ "generic types of healthcare actions.",
        category == "qualifier1" ~ "how (or why) the intervention was completed.",
        category == "qualifier2" ~ "the tools, agents or modalities used.",
        category == "qualifier3" ~ "the type, group, or strain of tissue, organism, or antigen used during an intervention.",
        TRUE ~ ""
      )
    )

    idx_vals <- NULL

    cli_h1(paste("Select ", toupper(category)))

    # get all unique IDs for current options
    relevant_ids <- mapping_cci[
      field == category,
      .(idx, description, include, subsection_descr, atc_codes)
    ]

    # check if current category expects numeric or alphabetic inputs
    if (class(cci_codes[[category]]) == "numeric") {
      numeric <- TRUE
      relevant_ids <- relevant_ids[, idx := as.numeric(idx)]
      relevant_ids <- relevant_ids[
        idx %in% as.numeric(cci_codes[[category]])
      ]
    } else {
      numeric <- FALSE
      relevant_ids <- relevant_ids[idx %in% cci_codes[[category]]]
    }

    relevant_ids <- relevant_ids[order(idx)]

    if (nrow(relevant_ids) > 0) {
      # Reactable table for dropdown table
      if ((section_input < 6 && category == "group") || (section_input == 1 && category == "qualifier2")) {
        # add placeholder ID (grouping ID for each broader category)
        relevant_ids[, placeholder := .GRP, by = subsection_descr]

        ####### Dropdown menu for the IDs and Descriptions #######
        broad_groups <- relevant_ids %>%
          group_by(subsection_descr) %>%
          arrange(idx) %>%
          summarize(
            range = paste(idx[1], "-", idx[length(idx)]),
            idx = unique(placeholder) # replace ID with placeholder
          ) %>%
          arrange(idx) %>%
          select(idx, range, subsection_descr) %>%
          data.table()

        # Reactable broad groupings
        print(htmlwidgets::prependContent(
          reactable(
            broad_groups,
            columns = list(
              idx = colDef(
                name = "ID",
                width = 50,
                defaultSortOrder = "asc"
              ),
              range = colDef(name = "Range", width = 100),
              subsection_descr = colDef(name = "Description", width = 300)
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
              selected <- broad_groups$idx[index]

              sub_data <- relevant_ids[
                subsection_descr == broad_groups[idx == index]$subsection_descr, .(idx, description, include)
              ]

              relevant_cols <- list(
                idx = colDef(
                  name = "ID",
                  width = 50,
                  defaultSortOrder = "asc"
                ),
                description = colDef(
                  name = "Description",
                  width = 300
                ),
                include = colDef(name = "Definition/Inclusion")
              )

              if (all(is.na(sub_data$include))) {
                sub_data <- sub_data %>% select(-include)
                relevant_cols$include <- NULL
              }

              reactable(
                sub_data,
                searchable = TRUE,
                sortable = TRUE,
                pagination = FALSE,
                striped = TRUE,
                highlight = TRUE,
                resizable = TRUE,
                columns = relevant_cols
              )
            }
          ),
          htmltools::tags$h1(toTitleCase(category)),
          htmltools::tags$h3(definition),
          htmltools::tags$h3("Please select the relevant ID(s). You can either provide numeric inputs for the broader categories OR alphabetic inputs to choose the more detailed subcategories."),
          htmltools::tags$p("Click on the arrow on the left to view all subcategories under a given category.")
        ))

        input <- get_user_input(
          category = category,
          valid_inputs = unique(c(
            relevant_ids$idx,
            relevant_ids$placeholder
          ))
        )
      } else {
        relevant_ids[, placeholder := NA]

        table <- relevant_ids %>% select(idx, description, include)
        relevant_cols <- list(
          idx = colDef(
            name = "ID",
            width = 50,
            defaultSortOrder = "asc"
          ),
          description = colDef(
            name = "Description",
            width = 300
          ),
          include = colDef(name = "Definition/Inclusion")
        )

        if (all(is.na(relevant_ids$include))) {
          table <- table %>% select(-include)
          relevant_cols$include <- NULL
        }

        # Non drop down Reactable Tables
        print(htmlwidgets::prependContent(
          reactable(
            table,
            columns = relevant_cols,
            searchable = TRUE,
            sortable = TRUE,
            pagination = FALSE,
            striped = TRUE,
            highlight = TRUE,
            resizable = TRUE
          ),
          htmltools::tags$h1(toTitleCase(category)),
          htmltools::tags$h3(definition),
          htmltools::tags$h3("Please select the relevant IDs:")
        ))

        input <- get_user_input(
          category = category,
          valid_inputs = unique(c(
            relevant_ids$idx
          )),
          numeric = numeric
        )
      }

      # get all idx values based on user's input (if any)
      if (numeric == TRUE) {
        idx_vals <- unique(sort(c(
          relevant_ids[idx %in% input]$idx
        )))
      } else {
        if (section_input == 8) {
          # in section 8, can have either numeric or alphabetic idx
          idx_vals <- unique(sort(c(
            relevant_ids[idx %in% input$character]$idx,
            relevant_ids[idx %in% input$numeric]$idx
          )))
        } else {
          idx_vals <- unique(sort(c(
            relevant_ids[idx %in% input$character]$idx,
            relevant_ids[placeholder %in% input$numeric]$idx
          )))
        }
      }

      # filter CCI table by corresponding entries
      if (!all(is.na(idx_vals)) && !all(is.null(idx_vals))) {
        cci_codes <- cci_codes[get(category) %in% idx_vals]
      }
    } else {
      cat(paste(toTitleCase(category), "not applicable. Skipping...\n"))
    }
  } # close loop for categories

  return(cci_codes)
}
