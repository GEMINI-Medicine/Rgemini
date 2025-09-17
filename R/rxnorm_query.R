### Pharmacy Mapper ###
#' Retrieve rows from GEMINI pharmacy data matching specified drug term(s).
#'
#' @param dbcon (`DBIConnection`)
#' A database connection to a GEMINI database. Requires version drm_cleandb_v3_1_0 / H4H_v5 or newer.
#' Older DBs lack `row_num` in the pharmacy table and are therefore incompatible with the RxNorm workflow.
#' @param drug_name (`character`)
#' Name of drug to search for (e.g., "amoxicillin"). Generic or brand name is accepted. Multiple drugs can be provided as a character vector. If empty, function expects `drug_class` input instead.
#' @param class_input (`character`)
#' Optional input: Can be used as an alternative to `drug_input` to search whole drug classes based on ATC code (e.g., "J05" = Antivirals for systemic use), rather than searching for individual drug names.
#' @param detailed_search (`logical`)
#' If TRUE (default), the function searches for every related concept to each selected drug, instead of just the selected drugs.
#' This will greatly expand the search and increase sensitivity, but will sometimes result in false positive matches.
#' @param return_unmatched (mandatory, default: FALSE)
#' If TRUE, the function will output a list with two vectors:
#' (1) First vector called matched_rows: All the matched entries in a long format same output as if the argument return_unmatched is FALSE.
#' (2) Second vector called unmatched_rows: all unmatched pharmacy rows (wide format with genc_id, 6 identifying columns, and row_num) based on a list of genc_ids inputted.
#' If FALSE, the function will by default output matched entries in a long format for every genc_id wiht the following columns: genc_id, search_type, raw_input, rxnorm_match
#' @param cohort (optional, default: 'all') data frame of the cohort.
#' @param return_drug_list (optional, default: FALSE) logical
#' Outputs the search drug list instead of searching
#' @return
#' For unmatched_row = FALSE
#'
#' The GEMINI pharmacy dataframe for matched rows, or NA if cancelled.
#' An additional 'rxnorm_match' column is added specifying what rxnorm matched the row to.
#'
#' @examples
#' \dontrun{
#' diabetes_orders_condensed <- rxnorm_query(
#'   dbcon = con,
#'   drug_input = c("metformin", "insulin"),
#'   return_unmatched = FALSE
#' )
#' }
#'
#' @import DBI httr jsonlite RCurl reactable RPostgreSQL
#'
#' @export

rxnorm_query <- function(dbcon, class_input = NA, drug_input = NA, cohort = NULL, return_unmatched = FALSE, detailed_search = TRUE, return_drug_list = FALSE) {
  # Assert there is at least one input else stop
  if (is.na(class_input[1]) & is.na(drug_input[1])) {
    stop("At least one of class_input or drug_input must not be NA.")
  }

  # If cohort is NULL, return_unmatched can not be TRUE
  if (is.null(cohort) & return_unmatched == TRUE) {
    stop("return_unmatched can not be TRUE if argument cohort is NULL since the number of pharmacy rows return will likely incapaciate your R session due to too much data being loaded into memory.")
  }

  if (!is.na(class_input[1])) {
    ###### CLASSIFICATION SEARCH ######
    # Find ATC classes with search_input in name or id
    call <- paste0(
      "https://rxnav.nlm.nih.gov/REST/",
      "rxclass/allClasses.json?classTypes=", "ATC1-4"
    )
    api_call_text <- httr::content(GET(call), "text", encoding = "UTF-8")
    api_call_df_raw <- as.data.frame(jsonlite::fromJSON(api_call_text, flatten = TRUE))
    all_classes <- api_call_df_raw %>%
      rename(
        class_id = rxclassMinConceptList.rxclassMinConcept.classId,
        class_name = rxclassMinConceptList.rxclassMinConcept.className,
        class_type = rxclassMinConceptList.rxclassMinConcept.classType
      )


    ###
    class_list <- data.table()
    for (class in class_input) {
      skip_to_next <- FALSE
      class_list_i <- all_classes[grep(class, paste(all_classes$class_name, all_classes$class_id),
        ignore.case = TRUE
      ), ]

      if (nrow(class_list_i) == 0) {
        # Get spelling suggestions for class names
        tryCatch({
          call <- paste0(
            "https://rxnav.nlm.nih.gov/REST/",
            "rxclass/spellingsuggestions.json?type=", "DRUG",
            "&term=", RCurl::curlEscape(class)
          )

          api_call_text <- httr::content(GET(call), "text", encoding = "UTF-8")
          api_call_df_raw <- as.data.frame(jsonlite::fromJSON(api_call_text, flatten = TRUE))

          spelling_suggestions <- api_call_df_raw %>%
            select(suggestion)

          cat(
            "\nWARNING:", class, "was skipped because no classes have this keyword.",
            "\n\tPotential spelling suggestions:", paste(spelling_suggestions$suggestion, collapse = ", "), "\n"
          )
        }, error = function(e) {
          cat("\nWARNING:", class, "was skipped because no classes have this keyword.\n")
        }, finally = {
          skip_to_next <<- TRUE
        })
      }
      if (skip_to_next) next
      class_list <- rbind(class_list, class_list_i, fill = TRUE)
    }
    if (nrow(class_list) == 0) {
      cat("\nCould not find any class matches for your search input.\n")
      return(NA)
    }

    class_list <- distinct(class_list) %>%
      arrange(class_id)
    rownames(class_list) <- NULL

    # Prompt user to select an ATC class
    print(reactable(
      class_list,
      searchable = TRUE,
      pagination = TRUE,
      defaultPageSize = 10,
      pageSizeOptions = c(10, 25, 50, 100),
      showPageSizeOptions = TRUE, # Show the dropdown for page size
      striped = TRUE,
      highlight = TRUE
    ))
    cat("\nThe table in the viewer displays the drug classes containing your search term.
        Press enter to confirm all, or enter the indexes to remove separated by commas.
        (or enter c to cancel)")
    class_input <- readline()
    if (class_input == "c") {
      cat("Cancelled.")
      return(NA)
    }
    if (class_input != "") {
      remove_indexes <- as.numeric(unlist(strsplit(class_input, ",")))
      cat("Removing", length(remove_indexes), "classes from search list...")
      class_list <- class_list %>% filter(!row_number() %in% remove_indexes)
    }
    cat("\nFinding drugs belonging to selected classes...\n")

    # Find drugs belonging to ATC classes
    class_id_list <- class_list$class_id

    drug_list <- data.table()
    term_types <- "BN+BPCK+DF+DFG+GPCK+IN+MIN+PIN+SBD+SBDC+SBDF+SBDG+SCD+SCDC+SCDF+SCDG"
    for (class_id in class_id_list) {
      skip_to_next <- FALSE
      drug_list_i <- data.table()
      tryCatch(
        {
          call <- paste0(
            "https://rxnav.nlm.nih.gov/REST/",
            "rxclass/classMembers.json?classId=", RCurl::curlEscape(class_id),
            "&relaSource=", "ATC",
            "&trans=", 0,
            "&ttys=", term_types
          )
          api_call_text <- httr::content(GET(call), "text", encoding = "UTF-8")
          api_call_df_raw <- as.data.frame(jsonlite::fromJSON(api_call_text, flatten = TRUE))

          drug_list_i <- api_call_df_raw %>%
            rename(
              rxcui = drugMemberGroup.drugMember.minConcept.rxcui,
              drug_name = drugMemberGroup.drugMember.minConcept.name
            ) %>%
            select(rxcui, drug_name) %>%
            mutate(atc_class = class_id)
        },
        error = function(e) {
          skip_to_next <<- TRUE
        }
      )
      drug_list <- rbind(drug_list, drug_list_i, fill = TRUE)
    }
    if (nrow(drug_list) == 0) {
      stop("Could not find any active drugs belonging to the selected class(es) in RxNorm")
    }
    drug_list_class <- distinct(drug_list) %>%
      group_by_at(setdiff(names(drug_list), "atc_class")) %>%
      summarise(atc_class = paste(atc_class, collapse = ", "), .groups = "drop")
  }

  if (!is.na(drug_input[1])) {
    ###### DRUG NAME SEARCH ######
    # Find drugs with the names in list
    drug_list <- data.table()
    for (drug in drug_input) {
      skip_to_next <- FALSE
      drug_list_i <- data.table()
      tryCatch(
        {
          call <- paste0(
            "https://rxnav.nlm.nih.gov/REST/",
            "rxcui.json?name=", RCurl::curlEscape(drug),
            "&allsrc=", 0,
            "&search=", 2
          )
          api_call_text <- httr::content(GET(call), "text", encoding = "UTF-8")
          drug_list_i <- as.data.frame(fromJSON(api_call_text, flatten = TRUE)) %>%
            dplyr::rename("rxcui" = "rxnormId") %>%
            mutate(
              atc_class = "MANUAL",
              drug_name = drug
            ) %>%
            select(rxcui, drug_name, atc_class) %>%
            head(1)
        },
        error = function(e) {
          # Get spelling suggestions for drug names
          tryCatch({
            call <- paste0(
              "https://rxnav.nlm.nih.gov/REST/",
              "rxclass/spellingsuggestions.json?type=", "DRUG",
              "&term=", RCurl::curlEscape(drug)
            )

            api_call_text <- httr::content(GET(call), "text", encoding = "UTF-8")
            api_call_df_raw <- as.data.frame(jsonlite::fromJSON(api_call_text, flatten = TRUE))

            spelling_suggestions <- api_call_df_raw %>%
              select(suggestion)

            cat(
              "\nWARNING:", drug, "was skipped because it could not be found.",
              "\n\tPotential spelling suggestions:", paste(spelling_suggestions$suggestion, collapse = ", "), "\n"
            )
          }, error = function(e) {
            cat("\nWARNING:", drug, "was skipped because it could not be found.\n")
          }, finally = {
            skip_to_next <<- TRUE
          })
        }
      )
      if (skip_to_next) next
      drug_list <- rbind(drug_list, drug_list_i, fill = TRUE)
    }
    if (nrow(drug_list) == 0) {
      cat("\nCould not find any matches for your search input.\n")
      return(NA)
    }
    drug_list_drug <- distinct(drug_list)
  }
  if (!is.na(drug_input[1]) & !is.na(class_input[1])) {
    cat("\n Warning Detected both drug_input and class_input. A union of the two will be used to search. Previous version was an intersect \n")
    rxcui_overlap <- union(drug_list_drug$rxcui, drug_list_class$rxcui) # It's time we don't use intersect but union here.
    drug_list_drug <- drug_list_drug %>% filter(!rxcui %in% rxcui_overlap)
    drug_list <- drug_list_class %>%
      mutate(atc_class = ifelse(rxcui %in% rxcui_overlap, paste0(atc_class, ", MANUAL"), atc_class)) %>%
      rbind(drug_list_drug) %>%
      arrange(drug_name)
  } else if (!is.na(drug_input[1])) {
    drug_list <- drug_list_drug
  } else if (!is.na(class_input[1])) {
    drug_list <- drug_list_class
  } else {
    stop("No drugs found.")
  }


  ###### RXCUI SEARCH ######
  # Prompt user to confirm the drugs
  print(reactable(
    drug_list,
    searchable = TRUE,
    pagination = TRUE,
    defaultPageSize = 10,
    pageSizeOptions = c(10, 25, 50, 100),
    showPageSizeOptions = TRUE, # Show the dropdown for page size
    striped = TRUE,
    highlight = TRUE
  ))
  cat("\nThe table in the viewer displays the drugs that will be searched.
      Press enter to confirm all, or enter the indexes to remove separated by commas.
      (or enter c to cancel)")
  drug_input <- readline()
  if (drug_input == "c") {
    cat("Cancelled.")
    return(NA)
  }
  if (drug_input != "") {
    remove_indexes <- as.numeric(unlist(strsplit(drug_input, ",")))
    cat("Removing", length(remove_indexes), "rxcui from search list...")
    drug_list <- drug_list %>% filter(!row_number() %in% remove_indexes)
  }
  if (return_drug_list == TRUE) {
    return(drug_list)
  }
  drug_list <- drug_list %>%
    select(rxcui, drug_name) %>%
    as.data.frame()

  if (detailed_search == TRUE) {
    # Optional: search for the rxcui of all concepts related to selected drugs
    cat("\nSearching for selected drugs and their related concepts...\n")
    related_drug_list <- data.table()
    term_types <- "BN+BPCK+DF+DFG+GPCK+IN+MIN+PIN+SBD+SBDC+SBDF+SBDG+SCD+SCDC+SCDF+SCDG"
    for (drug_row in 1:nrow(drug_list)) {
      skip_to_next <- FALSE
      related_drug_list_i <- data.table()
      tryCatch(
        {
          call <- paste0(
            "https://rxnav.nlm.nih.gov/REST/",
            "rxcui/", RCurl::curlEscape(drug_list[drug_row, "rxcui"]),
            "/related.json?tty=", term_types
          )
          api_call_text <- httr::content(GET(call), "text", encoding = "UTF-8")

          api_call_df_raw <- as.data.frame(rbindlist(fromJSON(api_call_text, flatten = TRUE)$relatedGroup$conceptGroup$conceptProperties))
          related_drug_list_i <- api_call_df_raw %>%
            select(rxcui) %>%
            mutate(drug_name = drug_list[drug_row, "drug_name"])
        },
        error = function(e) {
          skip_to_next <<- TRUE
        }
      )
      if (skip_to_next) next
      related_drug_list <- rbind(related_drug_list, related_drug_list_i, fill = TRUE)
      Sys.sleep(0.05) # So that not too many searches get sent
    }

    drug_list <- related_drug_list %>%
      group_by_at(setdiff(names(related_drug_list), c("drug_name"))) %>%
      dplyr::summarise(drug_name = paste(drug_name, collapse = " | "), .groups = "drop") %>%
      distinct()
  } else {
    cat("\nSearching for selected drugs....\n")
  }

  search_rxcui_list <- paste(paste0("(", drug_list$rxcui, ")"), collapse = ",")

  # Pull the pharmacy rows that match the rxcui list
  # NOTE: This will have to be changed to your database!

  # Should create a genc_id temp table if user-defined cohort is provided
  if (!is.null(cohort)) {
    cohort <- data.table(cohort)
    dbSendQuery(dbcon, "Drop table if exists genc_temp;") # Drop if exists
    dbWriteTable(dbcon, c("pg_temp", "genc_temp"), cohort[, .(genc_id)], temporary = TRUE, row.names = FALSE)
    dbSendQuery(dbcon, "Analyze genc_temp;")
  }


  query_str <- paste0(
    "WITH rc as (SELECT search_type, rxcui, raw_input FROM rxnorm_cache where active = TRUE AND rxcui in (", search_rxcui_list, ")", ")",
    " SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from pharmacy p inner join rc on p.med_id_din=rc.raw_input",
    " WHERE rc.search_type = 'med_id_din'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input, p.row_num from pharmacy p inner join rc on p.med_id_ndc=rc.raw_input",
    " WHERE rc.search_type = 'med_id_ndc'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL(SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from pharmacy p inner join rc on p.med_id_generic_name_raw=rc.raw_input",
    " WHERE rc.search_type = 'med_id_generic_name_raw'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from pharmacy p inner join rc on p.med_id_brand_name_raw=rc.raw_input",
    " WHERE rc.search_type = 'med_id_brand_name_raw'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from pharmacy p inner join rc on p.med_id_hospital_code_raw=rc.raw_input",
    " WHERE rc.search_type = 'med_id_hospital_code_raw'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from pharmacy p inner join rc on p.iv_component_type=rc.raw_input",
    " WHERE rc.search_type = 'iv_component_type'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ") ;"
  )

  cat("Searching database. This may take a few minutes...\n")
  # Pull the data from the databases
  tryCatch(
    {
      pharm_matches <- DBI::dbGetQuery(dbcon, query_str)
    },
    error = function(e) {
      print(e)
      stop("Error occured when querying database. Please re-create your database connection and try again.")
    }
  )

  cat("Finalizing. This may take a few minutes...\n")
  # Attach the name of matched drugs, combined from all matches
  final_matches <- pharm_matches %>%
    merge(drug_list, by = "rxcui", all.x = T) %>%
    dplyr::rename(rxnorm_match = drug_name) %>%
    select(-rxcui) %>%
    as.data.table() %>%
    unique()

  final_matches_n <- nrow(final_matches)


  cat("Found", final_matches_n, "matches.")

  if (return_unmatched == FALSE) {
    return(final_matches)
  } else {
    # If return_unmatched is T then we need to output

    cat("\nComputing all the unmatched rows. Warning: If you have too many genc_ids in your input this part may crash your R session due to memory issue\n")

    # Write into a temp table of all the matched rows
    dbSendQuery(dbcon, "Drop table if exists matched_rows")
    dbWriteTable(dbcon, c("pg_temp", "matched_rows"), final_matches, temporary = TRUE, row.names = FALSE)

    query_str_unmat <- paste0(
      "select genc_id,med_id_generic_name_raw,med_id_brand_name_raw, med_id_din, med_id_ndc,med_id_hospital_code_raw,iv_component_type, row_num",
      " from pharmacy p where exists (select 1 from genc_temp t where t.genc_id=p.genc_id)",
      " and not exists (select 1 from matched_rows m where m.search_type='med_id_generic_name_raw' and p.med_id_generic_name_raw=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_brand_name_raw' and p.med_id_brand_name_raw=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_din' and p.med_id_din=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_ndc' and p.med_id_ndc=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_hospital_code_raw' and p.med_id_hospital_code_raw=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='iv_component_type' and p.iv_component_type=m.raw_input)"
    )

    unmatched_rows <- dbGetQuery(dbcon, query_str_unmat) %>%
      data.table()


    output <- list()
    output$matched_rows <- final_matches
    output$unmatched_rows <- unmatched_rows

    return(output)
  }
}
