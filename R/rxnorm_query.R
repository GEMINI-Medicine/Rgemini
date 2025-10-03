#' @title
#' RxNorm Pharmacy Data Mapping Function
#'
#' @description
#' Retrieve rows from GEMINI pharmacy data matching specified drug names or drug classes.
#'
#' @details
#' RxNorm is a standardized naming system and drug thesaurus provided by the National Library of Medicine.
#' The GEMINI-RxNorm system (Waters et al., 2023) automates the use of RxNorm tools with
#' other datasets to identify drug concepts from pharmacy orders.
#' The procedure matches drug-identifying information from pharmacy data to RxNorm concept identifiers.
#' A user interface allows researchers to search for drugs,
#' returning the relevant original pharmacy data and predicted drugs through matched RxNorm concepts.
#' The GEMINI-RxNorm system significantly reduces the time required over manual pharmacy validation.
#' It can reduce an estimated 30 seconds to 5 seconds per pharmacy row,
#' and it can reduce the total number of rows needed to be manually validated by up to 99.99% (Waters et al., 2023).
#'
#' Note: The returned matches from this function should be manually and carefully validated by users.
#' Please see the vignette for the prepare_pharm_for_validation.R function for more details.

#' @param dbcon (`DBIConnection`)
#' A database connection to a GEMINI database. Requires version drm_cleandb_v3_1_0 / H4H_v5 or newer.
#' Older DBs lack `row_num` in the pharmacy table and are therefore incompatible with the RxNorm workflow.
#' @param drug_name (`character`)
#' Name of drug to search for (e.g., "amoxicillin"). Generic or brand name is accepted.
#' Multiple drugs can be provided as a character vector. If empty, function expects `drug_class` input instead.
#' @param drug_class (`character`)
#' Optional input: Can be used as an alternative to `drug_name` to search
#' whole drug classes based on ATC code (e.g., "J05" = Antivirals for systemic
#' use), rather than searching for individual drug names.
#' @param detailed_search (`logical`)
#' If TRUE (default), the function searches for every related concept to each
#' selected drug. This may increase sensitivity, but can also result in false
#' positive matches. The impact of `detailed_search = TRUE` may vary by drug
#' group. In general, we recommend using the default option but users should
#' carefully review the output for false positives.
#' Users can run `detailed_search = FALSE` at their own discretion and we
#' recommend carefully checking for potential misses when doing so.
#'
#' @param return_unmatched (`logical`)
#' If FALSE (default), the function will only return pharmacy entries that Rxnorm matched to the searched drugs.
#' If TRUE, the function will additionally return all unmatched rows,
#' allowing users to inspect pharmacy entries that Rxnorm did not match to the searched drug(s) of interest.
#' The function will return a list with 2 items:
#' 1) `matched_rows`: All the matched entries (same output as if the argument return_unmatched is FALSE).
#' 2) `unmatched_rows`: All unmatched pharmacy rows for a cohort of interest.
# `return_unmatched = TRUE` is only supported when users specify a `cohort` input to avoid memory issues.
#' @param cohort (`data.table` or `data.frame`)
#' Optional input allowing users to specify a cohort of interest.
#' Needs to include GEMINI encounter ID (`genc_id`).
#' By default, the whole GEMINI pharmacy table will be included in the Rxnorm search.
#' @param return_drug_list (`logical`)
#' Optional input allowing users to return the list of searched drugs,
#'  instead of running the search (e.g., when planning a search by ATC class).
#' @return
#' By default, `rxnorm_query` returns a `data.table` containing the rows from
#' the pharmacy table that Rxnorm matched to the drug(s) of interest.
#' Entries are returned in long format for each `genc_id`, together with the following additional columns:
#' - `search_type`: Pharmacy column where drug match was found
#' - `raw_input`: Corresponding entry in the matched row(s) of the pharmacy table
#' - `row_num`: Row ID of matched pharmacy entries
#' - `rxnorm_match`: Rxnorm output specifying which of the searched drug(s) the pharmacy entry was matched to.
#'
#' When `return_unmatched` is TRUE, the 2nd list item of the output will
#' be a `data.table` with the unmatched rows,
#' containing all columns from the pharmacy table included in the Rxnorm search.
#' @examples
#' \dontrun{
#' # Connect to DB
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass("Username: "),
#'   password = getPass("Password: ")
#' )
#'
#' # Run Rxnorm query
#' rxnorm_res <- rxnorm_query(
#'   dbcon = dbcon,
#'   drug_name = c("metformin", "insulin")
#' )
#' }
#'
#' @import DBI httr jsonlite RCurl reactable
#' @references
#' Waters R, et al. JAMIA Open, 2023. https://doi.org/10.1093/jamiaopen/ooad062
#' @export
rxnorm_query <- function(dbcon,
                         drug_name = NULL,
                         drug_class = NULL,
                         cohort = NULL,
                         return_unmatched = FALSE,
                         detailed_search = TRUE,
                         return_drug_list = FALSE) {
  # Assert there is at least one input else stop
  if (is.null(drug_class) && is.null(drug_name)) {
    stop("At least one of drug_class or drug_name must not be NA.")
  }

  # Find the correct pharmacy table to query in the current database connection
  pharmacy_table <- Rgemini:::find_db_tablename(dbcon, "pharmacy")


  # If cohort is NULL, return_unmatched can not be TRUE
  if (is.null(cohort) && return_unmatched == TRUE) {
    stop("return_unmatched cannot be TRUE if argument cohort is NULL
    since the large number of unmatched pharmacy rows would likely cause memory issues.")
  }

  if (!is.null(drug_class)) {
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
    for (class in drug_class) {
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
    drug_class <- readline()
    if (drug_class == "c") {
      cat("Cancelled.")
      return(NA)
    }
    if (drug_class != "") {
      remove_indexes <- as.numeric(unlist(strsplit(drug_class, ",")))
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

  if (!is.null(drug_name)) {
    ###### DRUG NAME SEARCH ######
    # Find drugs with the names in list
    drug_list <- data.table()
    for (drug in drug_name) {
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
  if (!is.null(drug_name) && !is.null(drug_class)) {
    cat("\n Warning: Detected both `drug_name` and `drug_class` input. A union of the two will be used to search.\n")
    rxcui_overlap <- union(drug_list_drug$rxcui, drug_list_class$rxcui)
    drug_list_drug <- drug_list_drug %>% filter(!rxcui %in% rxcui_overlap)
    drug_list <- drug_list_class %>%
      mutate(atc_class = ifelse(rxcui %in% rxcui_overlap, paste0(atc_class, ", MANUAL"), atc_class)) %>%
      rbind(drug_list_drug) %>%
      arrange(drug_name)
  } else if (!is.null(drug_name)) {
    drug_list <- drug_list_drug
  } else if (!is.null(drug_class)) {
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
  drug_name <- readline()
  if (drug_name == "c") {
    cat("Cancelled.")
    return(NA)
  }
  if (drug_name != "") {
    remove_indexes <- as.numeric(unlist(strsplit(drug_name, ",")))
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
    for (drug_row in seq_len(nrow(drug_list))) {
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

          api_call_df_raw <- as.data.frame(rbindlist(fromJSON(api_call_text,
            flatten = TRUE
          )$relatedGroup$conceptGroup$conceptProperties))
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
    "WITH rc as (SELECT search_type, rxcui, raw_input FROM rxnorm_cache where active = TRUE AND rxcui in (",
    search_rxcui_list, ")", ")",
    " SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from ",
    pharmacy_table, " p inner join rc on p.med_id_din=rc.raw_input",
    " WHERE rc.search_type = 'med_id_din'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input, p.row_num from ",
    pharmacy_table, " p inner join rc on p.med_id_ndc=rc.raw_input",
    " WHERE rc.search_type = 'med_id_ndc'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL(SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from ",
    pharmacy_table, " p inner join rc on p.med_id_generic_name_raw=rc.raw_input",
    " WHERE rc.search_type = 'med_id_generic_name_raw'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from ",
    pharmacy_table, " p inner join rc on p.med_id_brand_name_raw=rc.raw_input",
    " WHERE rc.search_type = 'med_id_brand_name_raw'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from ",
    pharmacy_table, " p inner join rc on p.med_id_hospital_code_raw=rc.raw_input",
    " WHERE rc.search_type = 'med_id_hospital_code_raw'",
    ifelse(!is.null(cohort), " and exists (select 1 from genc_temp t where t.genc_id=p.genc_id)", ""),
    ")",
    " UNION ALL (SELECT p.genc_id, rc.rxcui, rc.search_type,rc.raw_input,p.row_num from ",
    pharmacy_table, " p inner join rc on p.iv_component_type=rc.raw_input",
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
    merge(drug_list, by = "rxcui", all.x = TRUE) %>%
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

    cat("\nComputing all the unmatched rows.
    Warning: If you have too many genc_ids in your input this part may crash your R session due to memory issue\n")

    # Write into a temp table of all the matched rows
    dbSendQuery(dbcon, "Drop table if exists matched_rows")
    dbWriteTable(dbcon, c("pg_temp", "matched_rows"), final_matches, temporary = TRUE, row.names = FALSE)

    query_str_unmat <- paste0(
      "select genc_id,med_id_generic_name_raw,med_id_brand_name_raw, med_id_din, med_id_ndc,
      med_id_hospital_code_raw,iv_component_type, row_num",
      " from ", pharmacy_table, " p where exists (select 1 from genc_temp t where t.genc_id=p.genc_id)",
      " and not exists (select 1 from matched_rows m where m.search_type='med_id_generic_name_raw' and
       p.med_id_generic_name_raw=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_brand_name_raw' and
       p.med_id_brand_name_raw=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_din' and p.med_id_din=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_ndc' and p.med_id_ndc=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='med_id_hospital_code_raw' and
       p.med_id_hospital_code_raw=m.raw_input)",
      "and not exists (select 1 from matched_rows m where m.search_type='iv_component_type' and
       p.iv_component_type=m.raw_input)"
    )

    unmatched_rows <- dbGetQuery(dbcon, query_str_unmat) %>%
      data.table()


    output <- list()
    output$matched_rows <- final_matches
    output$unmatched_rows <- unmatched_rows

    return(output)
  }
}
