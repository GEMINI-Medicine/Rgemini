#' @title
#' Cohort creation
#'
#' @description
#' This function creates a table showing the cohort size (e.g., number of
#' encounters) that were included/excluded at each step of the cohort creation.
#'
#' @param cohort (`list`)
#' A list of cohort tables at each step of the cohort inclusion/exclusion steps.
#' The function will count the number of rows in each list item to determine the
#' cohort size at each step.
#'
#' @param labels (`character`)
#' Vector containing a description for each inclusion/exclusion step (needs to
#' be in the same order as corresponding list items in `cohort` input).
#'
#' @param exclusion_flag (`logical`)
#' A vector indicating whether a given cohort creation step should be
#' shown as an exclusion. If `TRUE` the number and percentage of rows that
#' were removed (rather than included) will be shown.
#'
#' By default, all cohort steps will be interpreted as inclusion steps.
#'
#' @param show_prct (`logical`)
#' Flag indicating whether to show percentage values. If `FALSE`, only raw
#' counts will be shown.
#'
#' @param group_var (`character`)
#' Name of a grouping variable. If provided, cohort numbers will be stratified
#' by each level of the grouping variable (in addition to overall cohort
#' numbers).
#'
#' @param ...
#' Additional parameters that will be passed to `prettyNum` for additional
#' formatting of numbers (e.g., `big.mark = ","`).
#'
#' @return
#' A data.table showing the cohort numbers as a count (and percentage) at each
#' step of the cohort creation.
#'
#' @examples
#' my_data <- Rgemini::dummy_ipadmdad(10000, n_hospitals = 5) %>% data.table()
#'
#' cohort_table <- cohort_creation(
#'   cohort = list(
#'     my_data,
#'     my_data[gender == "F"],
#'     my_data[gender == "F" & age >= 65],
#'     my_data[gender == "F" & age >= 65 & !grepl("^7", discharge_disposition)]
#'   ),
#'   labels = c(
#'     "All GEMINI encounters",
#'     "Gender = Female",
#'     "Age >= 80",
#'     "In-hospital death"
#'   ),
#'   exclusion_flag = c(FALSE, FALSE, FALSE, TRUE),
#'   group_var = "hospital_num"
#' )
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
    Rgemini:::check_input(group_var, "charater")
  }

  if (length(cohort) != length(labels) | length(cohort) != length(exclusion_flag)) {
    stop("The `cohort`, `labels`, and `exclusion_flag` (if provided) inputs need to have the same length.")
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
        colnames(cohort_tab) <- paste(group_var, "=", as.character(unique(cohort[[1]][[group_var]])))
      }
    }

    return(cohort_tab)
  }


  ## create table for overall cohort
  cohort_tab <- create_cohort(cohort, exclusion_flag, group_var, ...)

  ## add columns by subgroup (if group_var specified)
  if (!is.null(group_var)) {
    groups <- unique(cohort[[1]][[group_var]])
    grouped_list <- list()
    for (i in groups) {
      grouped_list[[i]] <- lapply(cohort, function(x) x[get(group_var) == i])
    }
    cohort_tab_grouped <- lapply(
      grouped_list, create_cohort,
      exclusion_flag = exclusion_flag, group_var = group_var, ...
    )
    cohort_tab_grouped <- do.call(cbind, cohort_tab_grouped)
    cohort_tab <- cbind(cohort_tab, cohort_tab_grouped)
  }

  ## add column with inclusion/exclusion step number
  steps <- c(ifelse(
    exclusion_flag == TRUE,
    paste("Excl. ", ave(seq_along(exclusion_flag), exclusion_flag, FUN = seq_along), sep = ""),
    paste("Incl. ", ave(seq_along(exclusion_flag), exclusion_flag, FUN = seq_along), sep = "")
  ), if (exclusion_flag[length(exclusion_flag)] == TRUE) "")

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

  return(cohort_tab)
}
