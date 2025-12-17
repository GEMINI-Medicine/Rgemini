#' @title Prepare RxNorm Data for Validation
#'
#' @description
#' This function processes the output returned by `rxnorm_query()`
#' into a frequency table to faciliate validation of RxNorm matches by
#' a Subject Matter Expert (SME). It retrieves previously validated
#' `"raw_input ~ rxnorm_match"` pairs (fields returned by `rxnorm_query()`)
#' from the `lookup_pharmacy_mapping` table to identify RxNorm matches
#' that have already been validated by previous projects. The function
#' additionally applies optional hierarchy filtering and small cell
#' suppression. The returned output facilitates further analyses of
#' validated pharmacy entries for the drug(s) of interest.
#' 
#' When the `rxnorm_query()` object contains unmatched rows of the
#' pharmacy table, the function returns all columns containing drug-containing
#' columns of unmatched pharmacy rows, together with their historical
#' mappings from previous projects.
#' 
#' section @warning
#' The default version of this function requires database versions
#' `drm_cleandb_v4_1_1`/`h4h_template_v5_0_1` or newer due to the
#' dependency on the `lookup_pharmacy_mapping` table. However, as
#' a workaround, users can also supply a custom
#' `lookup_pharmacy_mapping` table (e.g., based on a flat file or, 
#' for internal GEMINI staff, the pharmacy master mapping DB).
#' For HPC4Health users working with older database versions: Please
#' reach out to our team via the ticketing system if you require
#' access to the pharmacy mapping lookup table.
#' 
#' @param dbcon (`DBIConnection`)\cr
#' `DBI` connection to a GEMINI database version `drm_cleandb_v4_1_1`/
#' `h4h_template_v5_0_1` or newer. This is required to query the
#' `lookup_pharmacy_mapping` table.
#' Alternatively, users may provide a `custom_lookup` input instead
#' (see below).
#'
#' @param rxnorm_res (`data.frame`, `data.table`, `list`)\cr
#' Output returned by `rxnorm_query()` to be processed for
#' validation.
#' If the RxNorm search returned unmatched rows, this object
#' should be provided as a list containing both `matched_rows`
#' and `unmatched_rows`.
#'
#' @param hierarchy (`logical`)\cr
#' If `TRUE` (default), the function applies predefined hierarchy
#' filters to retain only the highest-priority drug-containing
#' "raw_input" per pharmacy row (i.e. hierarchy filtering is applied
#' at the level of individual drug orders).
#' The hierarchy rules prioritize drug information in the following order:
#' `med_id_generic_name_raw > med_id_brand_name_raw > med_id_hospital_code_raw > med_id_din > med_id_ndc > iv_component_type`.
#' For each pharmacy row (identified by `row_num` of the pharmacy table),
#' only the highest-priority column that was matched to the drug(s)
#' of interest is retained as the "raw_input" for that row.
#' The hierarchy rules handle potentially redundant or conflicting
#' information of different search columns for the same pharmacy order.
#' It ensures consistent drug-containing information per row and streamlines
#' the validation process. Therefore, it is recommended to set hierarchy to
#' `TRUE`. Only set to `FALSE` when applying non-standard hierarchy rules
#' or conducting further detailed investigations.
#'
#' When the `rxnorm_res` object contains unmatched rows, hierarchy filters
#' are applied to search for historical mappings.
#' For unmatched rows, hierarchy filters cannot be disabled.
#'
#' @param cell_suppression (`logical`)\cr
#' When set to `TRUE` (default), pharmacy entries associated with less than
#' 6 unique encounters are cell-suppressed and shown as "<6" in the output
#' table.
#'
#' @param outpath (`character`)\cr
#' Optional file path for saving the output files. Default is `NULL`, and
#' no file will be exported to folder.
#' If provided, two files are saved:
#' 1) an `.xlsx` file for SME review,
#' 2) an `.RDS` file for analysts (**This file should strictly be used
#' within GEMINI's HPC4Health/NORA environment and should NEVER be pushed
#' to a GitLab repository**).
#' Compared to the file for SMEs, the .RDS file for analysts contains an
#' additional column `pharm_row_num` with all `row_num` values in the
#' pharmacy table (as a list) corresponding to a given RxNorm match.
#' The IDs in the `pharm_row_num` serve as identifiers for analysts
#' to merge the validated frequency table back to the pharmacy table.
#' For example, this can be used to extract individual pharmacy orders
#' that contain the SME-validated drug entry and perform additional
#' filtering (e.g., by order date-time).
#' Note: When an `outpath` is provided, `cell_suppression` needs to be
#' set to `TRUE`.
#' 
#' @param custom_lookup (`data.frame`, `data.table`)\cr
#' Optional input allowing users to provide a custom version of
#' the `lookup_pharmacy_mapping` table (e.g., from a flat file/master
#' mapping DB). Needs to contain the following `character` columns:
#' - `search_type`: Name of column with RxNorm match
#' (e.g., `med_id_generic_name_raw`)
#' - `raw_input`: Entry from the pharmacy table RxNorm matched to the drug
#' of interest (e.g., `amoxicillin 50 mg/ml`)
#' - `rxnorm_match`: Drug of interest the pharmacy entry was matched to
#' (e.g., `amoxicillin`)
#' - `drug_group`: Broader drug group of matched drug (e.g., `antibiotic`)
#' - `project_name`: Name of the project drug match was validated for
#' 
#' If a `custom_lookup` table is provided to this function, the `dbcon`
#' argument will be ignored and the custom table will take priority over
#' the lookup table in the database.
#'
#' @return A list containing two or three data tables:
#' - sme: Frequency table for SME validation with columns `count`,
#' `search_type`, `raw_input`, `rxnorm_match`, `drug_group`,
#' `times_validated`, `SME_confirm` (for SME to confirm each
#' "raw_input ~ rxnorm_match" pair; must provide a logical value TRUE/FALSE), 
#' `SME_comments` (for SME to add comments, e.g. whether the drug should be
#' included in the study, regardless of whether the match itself is validated)
#' - analyst: Frequency table for analyst use, with columns `count`,
#' `search_type`, `raw_input`, `rxnorm_match`, `drug_group`,
#' `times_validated`, and `pharm_row_num (as a list)`
#' - unmatched_rows: Frequency table for unmatched pharmacy rows if they
#' exist in the input `rxnorm_res`. Contains pharmacy columns searched by
#' `rxnorm_query()` and additionally, `historically_mapped_to_drug` and
#' `historically_mapped_to_drug_group` (agnostic to drugs user searched
#' for), `SME_mapped_search_type`, `SME_mapped_drug`,
#' `SME_mapped_drug_group`, `SME_comment`
#'
#' Additionally, if `outpath` is provided, two files are saved to the
#' specified folder:
#' - "pharmacy_mapping_for_SME.xlsx" for SME review
#' - "pharm_res_INTERNAL_USE_ONLY.rds" for analyst to use post SME validation
#'
#' @details
#' The function takes results from `rxnorm_query()` and generates
#' structured frequency tables of pharmacy data for SME
#' review. Once the validation is complete, analysts can use the output
#' table to obtain all validated pharmacy entries for the drugs of interest
#' These are the key processing steps performed by the function:
#' - Normalizes pharmacy data by converting all text values to lowercase,
#' applying ASCII encoding, and trimming off leading and trailing white
#' spaces and periods.
#' - For `rxnorm_match` and `drug_group`, additional normalization is
#' applied to convert plural words to their singular form.
#' - Retrieves previous validation and adds the `times_validated` column
#' to the returned table indicating how many times each "raw_input ~
#' rxnorm_match" pair has previously been validated by individual
#' projects (0: never been validated, 1: validated by one project,
#' 2+: validated by two or more projects).
#' The function additionally retrieves information about the broader
#' classification of each drug from previous projects. However, please
#' note that the classification is not standardized and should be used as
#' supplementary information only.
#' - Applies hierarchy filtering on `search_type` such that only drug
#' information at the highest priority is retained for consideration per
#' row of pharmacy table (see details in parameter description of
#' `hierarchy`).
#' - Computes number of unique `genc_ids` associated with each
#' "search_type ~ raw_input ~ rxnorm_match" entry. Occurrences less
#' than 6 are suppressed to "<6".
#' - Generates a frequency table for SME to perform validation for each
#' "raw_input ~ rxnorm_match" pair (note: validation should be agnostic to
#' all other fields). Each row of the frequency table is uniquely identified 
#' by `row_id`. When an `outpath` is provided, the function saves .xlsx/.rds
#' files to the specified directory.
#' - Generates a frequency table for analyst use following SME validation. The
#' table is identical to the SME version but includes an additional column
#' `pharm_row_num`, which is a list storing `row_num` values (of the pharmacy
#' table) associated with each "search_type ~ raw_input ~ rxnorm_match" entry.
#' The `row_num` is an identifier allowing analysts to trace each entry back
#' to the original pharmacy table.
#' - If `rxnorm_query()` was executed with `return_unmatched = TRUE`, the
#' function performs a secondary search on unmatched rows. The secondary
#' search matches each drug-containing field (i.e. search_type) with existing
#' mappings in the `pharmacy_master_mapping` database in order of
#' hierarchy `med_id_generic_name_raw > med_id_brand_name_raw > med_id_hospital_code_raw > med_id_din > med_id_ndc > iv_component_type`.
#' The highest priority match is returned per row, along with the match's
#' corresponding drug_group. The search is not specific to the drug searched
#' by `rxnorm_query()`. Instead, it is a broad search against all existing
#' drug mappings found in the `lookup_pharmacy_mapping` table (or
#' user-provided custom lookup table). Users may need to apply
#' filters (e.g. via regex) and manual mapping to identify if any entries in
#' the unmatched rows may contain the drug(s) of interest.
#'
#' @examples
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' # Connect to GEMINI database
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass("Username: "),
#'   password = getPass("Password: ")
#' )
#' rxnorm_res <- rxnorm_query(
#'  dbcon, drug_name = c("warfarin"),
#'  return_unmatched = FALSE
#' )
#' 
#' ## Prepare RxNorm output for validation
#' res <- prepare_pharm_for_validation(
#'   dbcon = dbcon,
#'   rxnorm_res = rxnorm_res,
#'   hierarchy = TRUE,
#'   cell_suppression = TRUE,
#'   outpath = "/user_path/")
#'
#' res$sme # frequency table to be validated by SME
#' res$analyst # frequency table with `pharm_row_num` to be used by analysts after SME validation.
#' 
#' ## For internal GEMINI staff only:
#' ## Provide custom mapping lookup table based on master mapping DB
#' # connect to master mapping DB
#'  pharm_db <- DBI::dbConnect(drv,
#'    dbname = "pharmacy_mapping",
#'    host = "domain_name.ca",
#'    port = 1234,
#'    user = getPass("Username: "),
#'    password = getPass("Password: ")
#' )
#' # query master mapping table
#' pharm_mapping <- dbGetQuery(
#'  pharm_db, "SELECT * FROM pharmacy_master_mapping;"
#' ) %>%
#'  data.table()
#' 
#' res <- prepare_pharm_for_validation(
#'  rxnorm_res = rxnorm_res,
#'  custom_lookup = pharm_mapping # no `dbcon` input required
#' )
#' }
#'
#' @seealso `vignette("pharmacy_mapping", package = "Rgemini")`
#' @import openxlsx
#' @export
#'
prepare_pharm_for_validation <- function(
  dbcon = NULL,
  rxnorm_res,
  hierarchy = TRUE,
  cell_suppression = TRUE,
  outpath = NULL,
  custom_lookup = NULL
  ) {

  ## One of either dbcon or custom_lookup is required
  if (is.null(dbcon) && is.null(custom_lookup)) {
    stop(paste0(
      "Please provide one of either `dbcon` or ",
      "`custom_lookup` as a function input."
      ))
  }

  ## Get mapping lookup table
  if (!is.null(custom_lookup)) {
    # custom table, if provided this takes priority over DB lookup
    cat(paste(
      "\nThe user-provided `custom_lookup` table will be used",
      "to obtain previously validated pharmacy mappings.\n\n"
    ))
    check_input(
      custom_lookup,
      c("data.table", "data.frame"),
      colnames = c(
        "search_type", "raw_input", "rxnorm_match",
        "drug_group", "project_name"
      )
    )
    masterfile <- custom_lookup %>% data.table()
  } else if (!is.null(dbcon)) {
    # based on DB lookup table
    check_input(dbcon, argtype = "DBI")
    masterfile <- dbGetQuery(
      dbcon, "SELECT * from lookup_pharmacy_mapping"
    ) %>% data.table()
  } 

  masterfile[, (c("rxnorm_match", "drug_group")) := lapply(.SD, normalize_text, lemma = TRUE),
    .SDcols = c("rxnorm_match", "drug_group")
  ] # for 'rxnorm_match' 'drug_group' set lemma=T to additionally convert to singular
  masterfile[, (setdiff(names(masterfile), c("rxnorm_match", "drug_group"))) := lapply(.SD, normalize_text),
    .SDcols = setdiff(names(masterfile), c("rxnorm_match", "drug_group"))
  ] # normalize text for remaining cols

  ## CHECK AND STANDARDIZE INPUT RXNORM DATA
  check_input(rxnorm_res, c("data.table", "data.frame", "list"))
  if (any(class(rxnorm_res) == "list")) {
    unmatch <- rxnorm_res$unmatched_rows %>% data.table()
    unmatch[, (names(unmatch)) := lapply(.SD, normalize_text)]
    rxnorm_res <- rxnorm_res$matched_rows %>% data.table()
  } else {
    rxnorm_res <- as.data.table(rxnorm_res) # coerce to datatable
  }
  rxnorm_res[, rxnorm_match := normalize_text(rxnorm_match, lemma = TRUE)] # for 'rxnorm_match' set lemma=T
  # to additionally convert to singular
  rxnorm_res[, (setdiff(names(rxnorm_res), "rxnorm_match")) := lapply(.SD, normalize_text),
    .SDcols = setdiff(names(rxnorm_res), "rxnorm_match")
  ] # normalize text for the rest
  # make sure expected columns exist
  check_input(rxnorm_res, "data.table", colnames = c("genc_id", "search_type", "raw_input", "row_num", "rxnorm_match"))

  ################################# HANDLE RXNORM-MATCHED ROWS #################################
  ## EXTRACT RELEVANT DRUG-CONTAINING INFORMATION WITH OR WITHOUT HIERARCHY RULES
  if (hierarchy) {
    cat(paste0(
      "Applying hierarchy filtering to `search-type` at the level of individual pharmacy orders (recommended): ",
      "Only records at the highest `search_type` level will be returned per `row_num` of the pharmacy table. ",
      "See function documentation for detailed hierarchy rules.\n\n"
    ))
    rxnorm_res_hier <- rxnorm_res[, hier_search_type := fcase(
      search_type == "med_id_generic_name_raw", 1,
      search_type == "med_id_brand_name_raw", 2,
      search_type == "med_id_hospital_code_raw", 3,
      search_type == "med_id_din", 4,
      search_type == "med_id_ndc", 5,
      search_type == "iv_component_type", 6,
      search_type == "order_description", 7 # NOTE: this field will be added to rxnorm_query
    )] %>% .[, .SD[hier_search_type == min(hier_search_type)], by = .(genc_id, row_num)]
    rxnorm_res_final <- rxnorm_res_hier[,
    .(rxnorm_match = paste0(unique(sort(trimws(unlist(strsplit(rxnorm_match, " \\|+"))))), collapse = " || ")),
      by = .(genc_id, row_num, search_type, raw_input)
    ]
  } else {
    message(paste0("Skipping order-level hierarchy filtering based on user's input...",
                   "\nAll drug-containing records identified by RxNorm will be returned, including ",
                   "possibly redundant or conflicting records associated with the same pharmacy order.",
                   "\n Only set `hierarchy=FALSE` when applying non-standard hierarchy rules or ",
                   "conducting further order-level investigations.\n"))
    rxnorm_res_final <- rxnorm_res
  }

  ## CALCULATE OCCURRENCE FREQUENCIES
  freq_tab <- rxnorm_res_final[, .(
    count = length(unique(genc_id)),
    pharm_row_num = list(row_num) # storing row_num as a list for ease of extraction
  ), by = .(search_type, raw_input, rxnorm_match)]
  ## GET PREVIOUS VALIDATED `raw_input ~ rxnorm_match pairs` FROM MASTER FILE
  times_valid <- inner_join(
    freq_tab,
    masterfile[, .(raw_input, rxnorm_match, project_name)],
    by = c("raw_input", "rxnorm_match"), relationship = "many-to-many"
  ) %>%
    group_by(raw_input, rxnorm_match) %>% # note: only raw_input and rxnorm_match are relevant, agnostic to search_type
    summarise(
      times_validated = ifelse(
        length(unique(project_name)) >= 2, "2+",
        as.character(length(unique(project_name)))
      ), .groups = "drop"
    )

  ## GET VALIDATED `drug_group` CORRESPONDING TO `rxnorm_match` FROM MASTER FILE
  drug_grp <- inner_join(
    freq_tab,
    masterfile[, .(rxnorm_match, drug_group)],
    by = "rxnorm_match", relationship = "many-to-many"
  ) %>%
    group_by(rxnorm_match) %>% # note: only rxnorm_match is relevant for merging in drug_group
    summarise(
      drug_group = paste(unique(drug_group), collapse = " || "), .groups = "drop"
    )

  ## MERGE MASTERFILE INFO TO FREQUENCY TABLE & CLEANUP
  freq_tab <- freq_tab %>%
    left_join(drug_grp, by = "rxnorm_match") %>%
    left_join(times_valid, by = c("raw_input", "rxnorm_match")) %>%
    arrange(-count) %>% # arrange data by frequency
    mutate(
      row_id = 1:nrow(freq_tab), # add row_id as persistent identifier to track accidental row deletion after SME mapping
      times_validated = ifelse(is.na(times_validated), 0, times_validated) # set unvalidated to 0
    )

  if (cell_suppression) {
    freq_tab[, count := ifelse(count < 6, "<6", as.character(count))]
  } # suppress counts <6
  freq_tab_sme <- freq_tab %>%
    mutate(
      SME_confirm = as.logical(NA), # create columns for SME to fill out # confirm raw_input~rxnorm_match strictly
      SME_comment = as.character(NA) # here is for things like whether that entry should be included to the project
      # (i.e. the raw_input~rxnorm_match could be correct, but PI is not interested in including entry in the project)
    ) %>%
    .[, .(
      row_id, count, search_type, raw_input, rxnorm_match,
      drug_group, times_validated, SME_confirm, SME_comment
    )] # order columns
  freq_tab_ana <- freq_tab %>%
    .[, .(
      row_id, count, search_type, raw_input, rxnorm_match,
      drug_group, times_validated, pharm_row_num
    )] # order columns
  ################################# HANDLE UNMATCH ROWS WHEN EXIST #################################
  if (exists("unmatch", inherits = FALSE)) {
    cat(paste0("Based on user input, `rxnorm_res` contains unmatched rows from the pharmacy table. ",
               "A secondary search will be performed to match each rxnorm-unmatched row with existing ",
               "mappings in `pharmacy_master_mapping` following standard hierarchy rules. ",
               "The secondary search is not specific to user's drug(s) of interest. Users should perform ",
               "further filtering (e.g. via regex) and manual mapping to identify drug(s) of interest.\n"))
    search_col <- c(
      "med_id_generic_name_raw", "med_id_brand_name_raw", "med_id_hospital_code_raw",
      "med_id_din", "med_id_ndc", "iv_component_type"
    ) # "order_description" tbd

    # search for each rxnorm-unmatched row against previously mapped drugs
    unmatch$index <- 1:nrow(unmatch)
    res <- as.data.table(matrix(ncol = 0, nrow = 0)) # initialize res

    for (col in search_col) { # sequential inner join to implement hierarchy rules
      new_match <- merge(
        unmatch[!index %in% res$index, ],
        masterfile[, .(raw_input, rxnorm_match, drug_group)] %>% distinct(),
        by.x = col, by.y = "raw_input",
        all = FALSE
      )
      res <- rbind(res, new_match)
    }
    # collapse >1 matches
    res <- res[, .(
      rxnorm_match = paste(unique(sort(trimws(unlist(strsplit(gsub("manual$", "", rxnorm_match), " \\|+"))))),
       collapse = " || "), # if rxnorm_match contains only "manual", not return
      drug_group = paste(unique(drug_group), collapse = " || ")
    ), by = c("index", "genc_id", search_col)]

    # combine rows that have been previously mapped with rows that have never been mapped
    unmatch_checked <- bind_rows(res, unmatch[!index %in% res$index, ]) %>%
      data.table() %>%
      filter(!if_all(all_of(search_col), ~ is.na(.) | . == "")) # occasionally all search_cols are NA in
      # the pharmacy table - remove since no real information generate frequency table for unmatched rows & cleanup
      unmatch_freq_tab <- unmatch_checked %>%
        group_by(across(all_of(c(search_col, "rxnorm_match", "drug_group")))) %>%
        summarise(count = length(unique(genc_id)), .groups = "drop") %>%
        data.table() %>%
        arrange(-count) %>%
        mutate(
          row_id = 1:length(count),
          historically_mapped_to_drug = rxnorm_match,
          historically_mapped_to_drug_group = drug_group,
          SME_mapped_search_type = as.character(NA),
          SME_mapped_drug = as.character(NA),
          SME_mapped_drug_group = as.character(NA),
          SME_comment = as.character(NA)
        ) %>%
        .[, .SD, .SDcols = c(
          "row_id", "count", search_col, "historically_mapped_to_drug",
          "historically_mapped_to_drug_group", "SME_mapped_search_type", "SME_mapped_drug",
          "SME_mapped_drug_group", "SME_comment"
        )]

    if (cell_suppression == TRUE) {
      unmatch_freq_tab[, count := ifelse(count < 6, "<6", as.character(count))]
    } # suppress counts <6
  }

  ################################# RETURN RESULTS #################################
  if (exists("unmatch_freq_tab", inherits = FALSE)) {
    res_final <- list(
      sme = freq_tab_sme,
      analyst = freq_tab_ana,
      unmatched_rows = unmatch_freq_tab
    )
  } else {
    res_final <- list(
      sme = freq_tab_sme,
      analyst = freq_tab_ana
    )
  }
  if (!is.null(outpath)) {
    if (!cell_suppression) {
      stop(paste0("Based on user input, cell_suppression is set to FALSE.",
            "\nResults cannot be exported without small cell suppression to protect privacy.",
            "\nTo write results to folder, please set `cell_suppression=TRUE`"))
    }
    # Return xlsx worksheet
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "rxnorm_matches")
    openxlsx::writeData(wb, "rxnorm_matches", freq_tab_sme, withFilter = TRUE)

    if (exists("unmatch_freq_tab", inherits = FALSE)) {
      openxlsx::addWorksheet(wb, "rxnorm_unmatched_entries")
      openxlsx::writeData(wb, "rxnorm_unmatched_entries", unmatch_freq_tab, withFilter = TRUE)
    }
    # get current date (to be appended to file name)
    date <- format(Sys.Date(), "%Y%m%d")
    # save output
    openxlsx::saveWorkbook(wb,
      file.path(paste0(outpath, "/pharmacy_mapping_for_SME_", date, ".xlsx")),
      overwrite = TRUE
    )
    cat(
      "\n- Pharmacy mapping for SME validation is saved to:",
      paste0(outpath, "/pharmacy_mapping_for_SME_", date, ".xlsx\n")
    )
    # Return .RDS object for analyst to track pharmacy row_num (never send this to SME)
    saveRDS(res_final, file.path(paste0(outpath, "/pharm_res_INTERNAL_USE_ONLY_", date, ".rds")))
    cat(
      "\n- Pharmacy mapping for INTERNAL USE by analyst is saved to:",
      paste0(outpath, "/pharm_res_INTERNAL_USE_ONLY_", date, ".rds")
    )
  }
  return(res_final)
}
