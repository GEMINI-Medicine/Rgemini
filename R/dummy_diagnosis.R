#' @title
#' Generate Simulated Diagnosis Data Table
#'
#' @description
#' This function generates simulated data table resembling `ipdiagnosis` or `erdiagnosis` tables that can be used for testing or demonstration purposes.
#' It internally calls `sample_icd()` function to sample ICD-10 codes and
#' accepts arguments passed to `sample_icd()` for customizing the sampling scheme.
#' It is a copied, edited version of the `dummy_diag` function from the Rgemini package.
#'
#' @details
#' To ensure simulated table resembles "ip(er)diagnosis" table, the following characteristics are applied to fields:
#'
#' - `genc_id`: Numerical identification of encounters starting from 1. The number of unique encounters is defined by `nid`. The total number of rows is defined by `nrow`,
#'   where the number of rows for each encounter is random, but each encounter has at least one row.
#' - `hospital_num`: Numerical identification of hospitals from 1 to 5. All rows of an encounter are linked to a single hospital
#' - `diagnosis_code`: "ipdiagnosis" table only. Simulated ICD-10 diagnosis codes. Each encounter can be associated with multiple diagnosis codes in long format.
#' - `diagnosis_type`: "ipdiagnosis" table only. The first row of each encounter is consistently assigned to the diagnosis type "M".
#'                            For the remaining rows, if `diagnosis_type` is specified by users, diagnosis types are sampled randomly from values provided;
#'                            if `diagnosis_type` is NULL, diagnosis types are sampled from ("1", "2", "3", "4", "5", "6", "9", "W", "X", and "Y"), with sampling probability proportionate to their prevalence in the "ipdiagnosis" table.
#' - `diagnosis_cluster`: "ipdiagnosis" table only. Proportionally sampled from values that have a prevalence of more than 1% in the "diagnosis_cluster" field of the "ipdiagnosis" table, which are ("", "A", "B").
#' - `diagnosis_prefix`: "ipdiagnosis" table only. Proportionally sampled from values that have a prevalence of more than 1% in the "diagnosis_prefix" field of the "ipdiagnosis" table, which are ("", "N", "Q", "6").
#' - `er_diagnosis_code`: "erdiagnosis" table only. Simulated ICD-10 diagnosis codes. Each encounter can be associated with multiple diagnosis codes in long format.
#' - `er_diagnosis_type`: "erdiagnosis" table only. Proportionally sampled from values that have a prevalence of more than 1% in the "er_diagnosis_type" field of the "erdiagnosis" table, which are ("", "M", "9", "3", "O").
#'
#'
#' @note The following fields `(er)diagnosis_code`, `(er)diagnosis_type`, `diagnosis_cluster`, `diagnosis_prefix` are simulated independently.
#' Therefore, the simulated combinations may not reflect the interrelationships of these fields in actual data.
#' For example, specific diagnosis codes may be associated with specific diagnosis types, diagnosis clusters, or diagnosis prefix in reality.
#' However, these relationships are not maintained for the purpose of generating dummy data.
#' Users require specific linkages between these fields should consider customizing the output data or manually generating the desired combinations.
#'
#' @param n (`integer`)\cr Number of unique encounter IDs (`genc_id`) to simulate. Value must be greater than 0.
#' 
#' @param n_hospitals (`integer`)\cr Number of hospitals to simulate in the resulting data table
#' 
#' @param cohort (`data.frame`)\cr Optional, the administrative data frame containing `genc_id` and `hospital_num` information to be used in the output
#' 
#' @param ipdiagnosis (`logical`)\cr Default to "TRUE" and returns simulated "ipdiagnosis" table.
#' If FALSE, returns simulated "erdiagnosis" table.
#' See tables in [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#'
#' @param diagnosis_type (`character vector`)\cr The type(s) of diagnosis to return.
#' Possible diagnosis types are ("M", 1", "2", "3", "4", "5", "6", "9", "W", "X", and "Y").
#' Regardless of `diagnosis_type` input, the `ipdiagnosis` table is defaulted to always return type "M" for the first row of each encounter.
#'
#' @param ... Additional arguments for ICD code sampling scheme. See `sample_icd()` for details.
#'
#' @return (`data.table`)\cr A data table containing simulated data of
#' `genc_id`, `(er)_diagnosis_code`, `(er)_diagnosis_type`, `hospital_num`,
#' and other fields found in the respective diagnosis table.
#'
#' @export
#'
#' @examples
#'
#' ### Simulate an erdiagnosis table for 5 unique subjects with total 20 records:
#' \dontrun{
#' set.seed(1)
#' erdiag <- dummy_diag(n = 50, n_hospitals = 2, ipdiagnosis = F)
#' }
#' 
#' ### Simulate an erdiagnosis table including data from `cohort`
#' cohort <- dummy_ipadmdad_copy()
#' erdiag <- dummy_diagnosis(cohort = cohort)
#'
#' ### Simulate an ipdiagnosis table with diagnosis codes starting with "E11":
#' \dontrun{
#' set.seed(1)
#' ipdiag <- dummy_diagnosis(n = 50, n_hospitals = 20, ipdiagnosis = T, pattern = "^E11")
#' }
#'
#' ### Simulate a ipdiagnosis table with random diagnosis codes in diagnosis type 3 or 6 only:
#' \dontrun{
#' set.seed(1)
#' ipdiag <- dummy_diagnosis(n = 50, n_hospitals = 10, diagnosis_type = (c("3", "6"))) %>%
#'   filter(diagnosis_type != "M") # remove default rows with diagnosis_type="M" from each ID
#' }


dummy_diagnosis <- function(n = 1000, n_hospitals = 10, cohort = NULL, cohort_type = "admdad", ipdiagnosis = TRUE, diagnosis_type = NULL, seed = NULL, ...) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    
    #### get data.tables with  `genc_id` and `hospital_num` ####
    # average number of repeats is 3.9, but `df1` and `df2` will be joined
    # df2 will have 2.9 + 1 repeats on average
    avg_repeats <- ifelse(ipdiagnosis, 9.05, 2.92)
    include_prop <- ifelse(ipdiagnosis, 1, 0.82)
    if (is.null(cohort)) {
        df2 <- generate_id_hospital(nid = n, n_hospitals = n_hospitals, avg_repeats = avg_repeats, seed = seed)
    } else {
        # consider if `cohort` is IP or `er` data
        # if it is `er` then include all encounters from it
        if(cohort_type != "admdad" && ipdiagnosis == FALSE) {
            include_prop <- 1
        }
        cohort <- as.data.table(cohort)
        df2 <- generate_id_hospital(cohort = cohort, avg_repeats = avg_repeats, include_prop = include_prop, seed = seed)
        # only include the genc_id and hospital_num columns from `cohort`
        df2 <- df2[, c("genc_id", "hospital_num")]
    }

    # get all the unique genc_ids
    df1 <- df2 %>%
        distinct(genc_id, .keep_all = TRUE) %>%
        mutate(diagnosis_type = "M") # ensure each id has a type M diagnosis
        
    if (!is.null(diagnosis_type)) {
        df2[, diagnosis_type := sample(diagnosis_type, size = .N, replace = TRUE)]
    } else {
        df2[, diagnosis_type := sample(c("1", "2", "3", "4", "5", "6", "9", "W", "X", "Y"),
            size = .N, replace = TRUE,
            prob = c(0.43, 0.07, 0.40, 0.005, 0.0002, 0.002, 0.07, 0.02, 0.0006, 0.00003)
        )]
    }
    
    # total number of rows in dummy data table
    nrow <- nrow(df1) + nrow(df2)
    
    ##### sample `diagnosis_codes` #####
    # combine `df1` with "M" diagnosis types and `df2` with other diagnosis types
    dummy_data <- rbind(df1, df2) %>%
        mutate(
            diagnosis_code = sample_icd(n = nrow, ...),
            diagnosis_cluster = sample(c("", "A", "B"), size = nrow, replace = TRUE, prob = c(0.92, 0.07, 0.01)),
            diagnosis_prefix = sample(c("", "N", "Q", "6"), size = nrow, replace = TRUE, prob = c(0.9, 0.05, 0.02, 0.01))
        )
        
    if (ipdiagnosis == FALSE) {
        if (!is.null(diagnosis_type)) {
            er_diagnosis_type <- sample(diagnosis_type, size = nrow, replace = TRUE)
        } else {
            er_diagnosis_type <- sample(c("", "M", "9", "3", "O"), size = nrow, replace = TRUE, prob = c(0.53, 0.38, 0.06, 0.02, 0.01))
        }
    dummy_data <- dummy_data %>%
        dplyr::select(-diagnosis_cluster, -diagnosis_prefix, -diagnosis_type) %>%
        mutate(er_diagnosis_type = er_diagnosis_type) %>%
        rename(er_diagnosis_code = diagnosis_code)
  }

  return(dummy_data[order(dummy_data$genc_id)])
}
