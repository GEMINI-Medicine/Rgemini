#' @title
#' Cohort creation
#'
#' @description
#' This function creates a cohort data table based on user-specified inclusion/
#' exclusion criteria. It also returns a table showing the cohort size (e.g.,
#' number of encounters) that were included/excluded at each
#' step of the cohort creation.
#'
#' @param cohort (`list`)
#' A list where each item corresponds to a filtered `data.table`/`data.frame`
#' object that contains the cohort at a given step of the cohort
#' inclusions/exclusions. The function will automatically combine the
#' inclusion/exclusion steps in a sequential manner, and will then count the
#' number of entries that remain after each criterion.
#' For example, if you have a `data.table` object called `data`: To obtain a
#' cohort of encounters that are female and older than 65 years, you can use:
#' `cohort = list(data[gender == "F"], data[age > 65])`. In this case, the
#' returned cohort inclusion/exclusion table will contain 2 rows listing the
#' number of encounters that are 1) female, and 2) female **AND** older than 65
#' years (= final cohort).
#' Note that if `data` is a `data.frame`, you will need to filter the relevant
#' rows as follows:
#' `cohort = list(data[data\$gender == "F", ], data[data\$age > 65, ])`
#'
#' @param labels (`character`)
#' Vector containing a description for each inclusion/exclusion step (needs to
#' be in the same order as corresponding list items in `cohort` input).
#'
#' @param exclusion_flag (`logical`)
#' A vector indicating whether a given cohort creation step should be
#' interpreted as an exclusion (rather than an inclusion). If `TRUE` the
#' corresponding entries will be removed and the number (%) of rows that were
#' removed (rather than included) will be shown.
#'
#' By default, all cohort steps will be interpreted as inclusion steps.
#'
#' @param show_prct (`logical`)
#' Flag indicating whether to show percentage values (default = `TRUE`).
#' If `FALSE`, only raw counts will be shown. Note that the percentages
#' always reflect the % change relative to the N in the *previous*
#' inclusion/exclusion step.
#'
#' @param group_var (`character`)
#' Optional: Name of a grouping variable (e.g., hospital). If provided, cohort
#' numbers will be stratified by each level of the grouping variable (in
#' addition to overall cohort numbers).
#'
#' @param ...
#' Additional parameters that will be passed to `prettyNum` for additional
#' formatting of numbers (e.g., `big.mark = ","`).
#'
#' @return
#' A list with 2 items:
#' 1) `cohort_data`: `data.table` containing all entries in the final cohort
#' (after applying all inclusions/exclusions)
#' 2) `cohort_steps`: `data.table` showing the number (and %) of entries
#' that were included/excluded at each step of the cohort creation.
#'
#' @examples
#' # create dummy data
#' my_data <- Rgemini::dummy_ipadmdad(10000, n_hospitals = 5)
#'
#' # convert to data.table for easy filtering
#' my_data <- data.table::setDT(my_data)
#'
#' # run cohort_creation
#' my_cohort <- cohort_creation(
#'   cohort = list(
#'     my_data,
#'     my_data[gender == "F"],
#'     my_data[age > 65],
#'     my_data[grepl("^7", discharge_disposition)]
#'   ),
#'   labels = c(
#'     "All GEMINI encounters",
#'     "Gender = Female",
#'     "Age > 65",
#'     "In-hospital death"
#'   ),
#'   exclusion_flag = c(FALSE, FALSE, FALSE, TRUE),
#'   group_var = "hospital_num" # optional: stratify by hospital
#' )
#'
#' # get data table containing all entries in final cohort
#' cohort_data <- my_cohort[[1]]
#'
#' # print table with N (%) at each inclusion/exclusion step
#' print(my_cohort[[2]])
#'
#' @export
cohort_creation <- function(
    cohort,
    labels,
    exclusion_flag = NULL,
    show_prct = TRUE,
    group_var = NULL,
    ...) {
  ## if no exclusion flags provided, interpret all steps as "inclusions"
  if (is.null(exclusion_flag)) {
    exclusion_flag <- c(rep(FALSE, length(cohort)))
  }

  ## check user input
  Rgemini:::check_input(cohort, "list")
  Rgemini:::check_input(labels, "character")
  Rgemini:::check_input(list(exclusion_flag, show_prct), "logical")
  if (!is.null(group_var)) {
    Rgemini:::check_input(group_var, "character")
    ## check if group columns exist in all cohort list items
    lapply(cohort, function(cohort) {
      check_input(cohort, "data.table", colnames = group_var)
    })
  }

  if (length(cohort) != length(labels) || length(cohort) != length(exclusion_flag)) {
    stop("The `cohort`, `labels`, and `exclusion_flag` (if provided) inputs need to have the same length.")
  }

  ## Initialize a flag for cell suppression warning
  # (to avoid redundant warnings)
  warning_shown <- FALSE

  ## Create new cohort list according to inclusion/exclusion criteria
  # in the new list, each list item reflects the combination of the current
  # and ALL previous steps of inclusions/exclusions
  # Note: This part assumes that all inclusions/exclusions are applied
  # sequentially and are combined by AND
  cohort_clean <- list(cohort[[1]]) # baseline cohort

  # identify key columns to speed up merging
  key_col <- Reduce(intersect, lapply(cohort, colnames))

  for (i in seq(2, length(cohort))) {
    if (exclusion_flag[i] == TRUE) {
      # use anti_join for exclusion steps to remove excluded entries
      cohort_clean[[i]] <- anti_join(cohort_clean[[i - 1]], cohort[[i]], by = key_col)
    } else {
      # for inclusion steps, identify intersection between subsequent steps
      cohort_clean[[i]] <- merge(cohort_clean[[i - 1]], cohort[[i]], by = key_col)
    }
  }

  create_cohort <- function(cohort, exclusion_flag, group_var, ...) {
    ## get number of rows at each cohort creation step (= usually number of unique genc_ids)
    N <- sapply(cohort, nrow)

    ## calculate change in N between steps
    cohort_tab <- data.table(N, previous_n = lag(N))
    cohort_tab[, `%` := 100 * N / previous_n]

    ## for any steps with exclusion_flag = TRUE, show removal as -n (-X%)
    cohort_tab[exclusion_flag == TRUE & !is.na(previous_n), N := -(previous_n - N)]
    cohort_tab[exclusion_flag == TRUE & !is.na(previous_n), `%` := -(100 - `%`)]

    ## combine N (%, if show_prct = TRUE)
    cohort_tab[, `N (%)` := ifelse(
      !is.na(`%`) & show_prct == TRUE, paste0(prettyNum(N, ...), " (", round(`%`, 1), "%)"), prettyNum(N, ...)
    )]

    ## apply cell suppression for N < 6 (unless N = 0)
    if (nrow(cohort_tab[abs(N) < 6 & N != 0]) > 0) {
      cohort_tab[abs(N) < 6 & N != 0, `N (%)` := "N < 6"]
      if (warning_shown == FALSE) {
        warning(paste(
          "Some cells in the returned cohort inclusion/exclusion table have fewer than 6 counts.",
          "These cells have been suppressed (\"N < 6\").",
          "If it is possible to backcalculate the cell count based on other rows, please remove",
          "the suppressed cells when disseminating your work."
        ), immediate. = TRUE)
        warning_shown <<- TRUE
      }
    }

    cohort_tab <- cohort_tab[, .(`N (%)`)]

    ## add row with final cohort number
    # (only needed if last step was showing exclusion)
    if (exclusion_flag[length(exclusion_flag)] == TRUE) {
      cohort_tab <- rbind(
        cohort_tab,
        data.table(`N (%)` = prettyNum(nrow(cohort[[length(cohort)]]), ...))
      )
    }

    if (!is.null(group_var)) {
      if (length(unique(unique(cohort[[1]][[group_var]]))) == 1) {
        colnames(cohort_tab) <- as.character(unique(cohort[[1]][[group_var]]))
      }
    }

    return(cohort_tab)
  }


  ## create table for overall cohort
  cohort_tab <- create_cohort(cohort_clean, exclusion_flag, group_var, ...)

  ## add columns by subgroup (if group_var specified)
  if (!is.null(group_var)) {
    groups <- sort(unique(cohort_clean[[1]][[group_var]]))
    grouped_list <- list()
    for (k in seq_along(groups)) {
      grouped_list[[k]] <- lapply(cohort_clean, function(x) x[get(group_var) == groups[k]])
    }
    cohort_tab_grouped <- lapply(
      grouped_list, create_cohort,
      exclusion_flag = exclusion_flag, group_var = group_var, ...
    )
    cohort_tab_grouped <- bind_cols(cohort_tab_grouped)
    cohort_tab <- cbind(cohort_tab, cohort_tab_grouped)
  }

  ## add column with inclusion/exclusion step number
  steps <- c(ifelse(
    exclusion_flag == TRUE,
    paste("Excl. ", ave(seq_along(exclusion_flag), exclusion_flag, FUN = seq_along), sep = ""),
    paste("Incl. ", ave(seq_along(exclusion_flag), exclusion_flag, FUN = seq_along), sep = "")
  ), if (exclusion_flag[length(exclusion_flag)] == TRUE) "")

  ## if last step is an exclusion, add row showing N for final cohort
  labels <- c(labels, if (exclusion_flag[length(exclusion_flag)] == TRUE) "Final cohort")

  cohort_tab <- cbind(
    steps,
    labels,
    cohort_tab
  )

  ## Fix column names
  colnames(cohort_tab)[1:3] <- c("", "Cohort creation step", ifelse(show_prct == TRUE, "N (%)", "N"))
  if (!is.null(group_var)) {
    colnames(cohort_tab)[3] <- paste("Overall", colnames(cohort_tab)[3])
  }

  ## Remove key column assignment
  setkey(cohort_clean[[length(cohort_clean)]], NULL)

  ## returns list with 2 items:
  # 1) final cohort data after all inclusion/exclusion steps have been applied
  # 2) table showing cohort creation numbers at each inclusion/exclusion step
  return(list(
    cohort_data = cohort_clean[[length(cohort_clean)]],
    cohort_steps = cohort_tab
  ))
}
