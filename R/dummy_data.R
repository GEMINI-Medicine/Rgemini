#' @title
#' Simulate ICD-10 Diagnosis Codes
#' @description
#' This function simulates ICD-10 diagnosis codes at random or by user specified pattern.
#'
#' @param n (`integer`)\cr Number of ICD codes to simulate.
#'
#' @param source (`string`)\cr The source of the ICD coding to sample from.
#' Default to "comorbidity" the 2011 version of ICD-10 codes implemented in
#' the R [comorbidity](https://ellessenne.github.io/comorbidity/index.html) package.
#' If `source` is `icd_lookup`, ICD-10-CA codes will be sampled from the
#' `lookup_icd10_ca_description` table in the GEMINI database,
#' see details in [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' @param dbcon (`DBIConnection`)\cr
#' A database connection to any GEMINI database. Required when `source` is `icd_lookup`.
#'
#' @param pattern (`string`)\cr A valid regex expression that specifies the
#' desired pattern that the returned ICD codes should be matched with.
#'
#' @return (`vector`)\cr A vector of ICD diagnostic codes.
#'
#' @export
#'
#' @examples
#' ### Simulate 100 ICD-10 codes based on the 2011 version.
#' \dontrun{ sample_icd(100, source="comorbidity") }
#'
#' ### Simulate 100 ICD-10 codes starting with "C2" or "E10" based on the 2011 version.
#' \dontrun{ sample_icd(100, source="comorbidity", pattern="^C2|^E10") }
#'
#' ### Simulate 50 ICD-10-CA codes based on codes found in the `lookup_icd10_ca_description` table
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'                         dbname = "db",
#'                         host = "172.XX.XX.XXX",
#'                         port = 1234,
#'                         user = getPass("Enter user:"),
#'                         password = getPass("password"))
#' sample_icd(50, source="icd_lookup", dbcon=dbcon)
#' }
#'
sample_icd <- function(n = 1, source = "comorbidity", dbcon = NULL, pattern = NULL) {
  switch(source,
         comorbidity = {
           comorb <- comorbidity::icd10_2011 %>% as.data.table()
           if (!is.null(pattern)) {
             comorb <- comorb[grepl(toupper(pattern), Code.clean)]
           }
           if (nrow(comorb) > 0) {
             sample(x = comorb$Code.clean, size = n, replace = TRUE)
           } else {
             stop("No matching diagnoses found for the specified pattern")
           }
         },
         icd_lookup = {
           if (!is.null(dbcon)) {
             lookup <- RPostgreSQL::dbGetQuery(dbcon, "SELECT diagnosis_code  FROM lookup_icd10_ca_description where type != 'category'") %>% as.data.table()

             if (!is.null(pattern)) {
               lookup <- lookup[grepl(toupper(pattern), diagnosis_code)]
             }

             if (nrow(lookup) > 0) {
               sample(x = lookup$diagnosis_code, size = n, replace = TRUE)
             } else {
               stop("No matching diagnoses found for the specified pattern")
             }
           } else {
             stop("Invalid input for 'dbcon' argument. Database connection is required for sampling from `lookup_icd10_ca_to_ccsr` table\n")
           }
         }
  )
}


#' @title
#' Generate Simulated Diagnosis Data Table
#'
#' @description
#' This function generates simulated data table resembling `ipdiagnosis` or `erdiagnosis` tables
#' that can be used for testing or demonstration purposes.
#' It internally calls `sample_icd()` function to sample ICD-10 codes and
#' accepts arguments passed to `sample_icd()` for customizing the sampling scheme.
#'
#' @details
#' To ensure simulated table resembles "ip(er)diagnosis" table, the following characteristics are applied to fields:
#'
#' \itemize{
#'  \item{`genc_id`: } {Numerical identification of encounters starting from 1. The number of unique encounters is defined by `nid`. The total number of rows is defined by `nrow`,
#'   where the number of rows for each encounter is random, but each encounter has at least one row.}
#'  \item{`hospital_num`: }{Numerical identification of hospitals from 1 to 5. All rows of an encounter are linked to a single hospital}
#'  \item{`diagnosis_code`: }{"ipdiagnosis" table only. Simulated ICD-10 diagnosis codes. Each encounter can be associated with multiple diagnosis codes in long format.}
#'  \item{`diagnosis_type`: }{"ipdiagnosis" table only. The first row of each encounter is consistently assigned to the diagnosis type "M".
#'                            For the remaining rows, if `diagnosis_type` is specified by users, diagnosis types are sampled randomly from values provided;
#'                            if `diagnosis_type` is NULL, diagnosis types are sampled from ("1", "2", "3", "4", "5", "6", "9", "W", "X", and "Y"), with sampling probability proportionate to their prevalence in the "ipdiagnosis" table.}
#'  \item{`diagnosis_cluster`: }{"ipdiagnosis" table only. Proportionally sampled from values that have a prevalence of more than 1% in the "diagnosis_cluster" field of the "ipdiagnosis" table, which are ("", "A", "B").}
#'  \item{`diagnosis_prefix`: }{"ipdiagnosis" table only. Proportionally sampled from values that have a prevalence of more than 1% in the "diagnosis_prefix" field of the "ipdiagnosis" table, which are ("", "N", "Q", "6").}
#'  \item{`er_diagnosis_code`: }{"erdiagnosis" table only. Simulated ICD-10 diagnosis codes. Each encounter can be associated with multiple diagnosis codes in long format. }
#'  \item{`er_diagnosis_type`: }{"erdiagnosis" table only. Proportionally sampled from values that have a prevalence of more than 1% in the "er_diagnosis_type" field of the "erdiagnosis" table, which are ("", "M", "9", "3", "O").}
#' }
#'
#' @note The following fields `(er)diagnosis_code`, `(er)diagnosis_type`, `diagnosis_cluster`, `diagnosis_prefix` are simulated independently.
#' Therefore, the simulated combinations may not reflect the interrelationships of these fields in actual data.
#' For example, specific diagnosis codes may be associated with specific diagnosis types, diagnosis clusters, or diagnosis prefix in reality.
#' However, these relationships are not maintained for the purpose of generating dummy data.
#' Users require specific linkages between these fields should consider customizing the output data or manually generating the desired combinations.
#'
#' @param nid (`integer`)\cr Number of unique encounter IDs (`genc_id`) to simulate. Value must be greater than 0.
#'
#' @param nrow (`integer`)\cr Total number of rows of the simulated long format diagnosis table. Value must be greater than or equal to that in `nid`.
#'
#' @param ipdiagnosis (`logical`)\cr Default to "TRUE" and returns simulated "ipdiagnosis" table.
#' If FALSE, returns simulated "erdiagnosis" table.
#' See tables in [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
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
#' ### Simulate a erdiagnosis table for 5 unique subjects with total 20 records:
#' \dontrun{ set.seed(1)
#'           erdiag <- dummy_diag(nid=5, nrow=20, ipdiagnosis=F) }
#'
#' ### Simulate a ipdiagnosis table with diagnosis codes starting with "E11":
#' \dontrun{ set.seed(1)
#'           ipdiag <- dummy_diag(nid=5, nrow=20, ipdiagnosis=T, pattern ="^E11") }
#'
#' ### Simulate a ipdiagnosis table with random diagnosis codes in diagnosis type 3 or 6 only:
#' \dontrun{ set.seed(1)
#'           ipdiag <- dummy_diag(nid=5, nrow=20, diagnosis_type=(c("3", "6"))) %>%
#'              filter(diagnosis_type!="M") # remove default rows with diagnosis_type="M" from each ID
#' }
#'
#' ### Simulate a ipdiagnosis table with ICD-10-CA codes:
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'                         dbname = "db",
#'                         host = "172.XX.XX.XXX",
#'                         port = 1234,
#'                         user = getPass("Enter user:"),
#'                         password = getPass("password"))
#'
#' set.seed(1)
#' ipdiag <- dummy_diag(nid=5, nrow=20, ipdiagnosis=T, dbcon=dbcon, source="icd_lookup")}
#'

dummy_diag <- function(nid = 5, nrow = 50, ipdiagnosis = TRUE, diagnosis_type = NULL, ...) {

  df1 <- data.table(genc_id = 1:nid, diagnosis_type = "M") # ensure each id has a type M diagnosis

  if (!is.null(diagnosis_type)) {
    df2 <- data.table(
      genc_id = sample(1:nid, size = (nrow - nid), replace = TRUE),
      diagnosis_type = sample(diagnosis_type, size = (nrow - nid), replace = TRUE)
    )
  } else {
    df2 <- data.table(
      genc_id = sample(1:nid, size = (nrow - nid), replace = TRUE),
      diagnosis_type = sample(c("1", "2", "3", "4", "5", "6", "9", "W", "X", "Y"),
                              size = (nrow - nid), replace = TRUE,
                              prob = c(0.43, 0.07, 0.40, 0.005, 0.0002, 0.002, 0.07, 0.02, 0.0006, 0.00003)
      )
    )
  }

  dummy <- rbind(df1, df2) %>%
    left_join(data.table(genc_id = 1:nid, hospital_num = sample(1:5, size = nid, replace = TRUE)), by = "genc_id") %>%
    mutate(
      diagnosis_code = sample_icd(n = nrow, ...),
      diagnosis_cluster = sample(c("", "A", "B"), size = nrow, replace = TRUE, prob = c(0.92, 0.07, 0.01)),
      diagnosis_prefix = sample(c("", "N", "Q", "6"), size = nrow, replace = TRUE, prob = c(0.9, 0.05, 0.02, 0.01))
    )

  if (ipdiagnosis==FALSE) {

    if(!is.null(diagnosis_type)){
      er_diagnosis_type <- sample(diagnosis_type, size = nrow, replace = TRUE)
    } else {
      er_diagnosis_type <- sample(c("", "M", "9", "3", "O"), size = nrow, replace = TRUE, prob = c(0.53, 0.38, 0.06, 0.02, 0.01))
    }

    dummy <- dummy %>%
      dplyr::select(-diagnosis_cluster, -diagnosis_prefix, -diagnosis_type) %>%
      mutate(er_diagnosis_type = er_diagnosis_type) %>%
      rename(er_diagnosis_code = diagnosis_code)
  }

  return(dummy[order(dummy$genc_id)])
}


#' @title
#' Generated simulated lab data
#'
#' @description
#' Designed to mimic the most important elements of the GEMINI lab table as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' @param id (`numeric`)\cr
#' A single identifier that is repeated to match the length of `value`.
#'
#' @param omop (`character`)\cr
#' Codes corresponding to OMOP concept identifiers.
#'
#' @param value (`numeric`)\cr
#' Simulated result values for each lab test measurement.
#'
#' @param unit (`character`)\cr
#' Units corresponding to the particular lab test as defined by `omop`. It is repeated to match the length of `value`.
#'
#' @param mintime (`character`)\cr
#' In the format yyyy-mm-dd hh:mm. Earliest recorded test performed time.
#'
#' @return (`data.table`)\cr
#' With the columns, `id`, `omop`, `value`, `unit`, and `collection_date_time` as described above.
#'
#' @export
#'
#' @examples
#' lab <- dummy_lab(1, 3024641, c(7,8,15,30), "mmol/L", "2023-01-02 08:00")
#'
dummy_lab <- function(id, omop, value, unit, mintime){
  res <- data.table(
    genc_id = rep(id, length(value)),
    test_type_mapped_omop = omop,
    result_value = value,
    result_unit = rep(unit, length(value)),
    collection_date_time =  format(as.POSIXct(mintime, tz = "UTC") + sample(0:(24*60*60 - 1), size=length(value), replace = TRUE),"%Y-%m-%d %H:%M")
  )
  return(res)
}


#' @title
#' Generated simulated administrative data
#'
#' @description
#' Designed to partially mimic the `admdad` table as defined in the
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5).
#'
#' @param id (`numeric`)\cr
#' A single identifier that is repeated to match the length of `value`.
#'
#' @param admtime (`character`)\cr
#' In the format yyyy-mm-dd hh:mm. Corresponds to the admission time of the encounter.
#'
#' @return (`data.table`)\cr
#' With the columns `id` and `admission_date_time` as described above.
#'
#' @export
#'
#' @examples
#' admdad <- dummy_admdad(1, "2023-01-02 00:00")
#'
dummy_admdad <- function(id, admtime){
  res <- data.table(
    genc_id = id,
    admission_date_time = format(as.POSIXct(admtime, tz = "UTC"),"%Y-%m-%d %H:%M")
  )
  return(res)
}


#' @title
#' Dummy ipdiagnosis data
#'
#' @description
#' This mimics the GEMINI ipdiagnosis table. And is generated with the `dummy_diag` function.
#' See the [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)
#' for details.
#'
#' @keywords internal
#' @name dummy_ipdiagnosis
#' @docType data
#'
NULL


#' @title
#' Dummy daily census data
#'
#' @description
#' Used to generate mock output for the `daily_census` vignette.
#'
#' @keywords internal
#' @name dummy_census
#' @docType data
#'
NULL


#' @title
#' Dummy CCSR data
#'
#' @description
#' Used to generate mock output for the `icd_to_ccsr` vignette.
#'
#' @keywords internal
#' @name dummy_ccsr
#' @docType data
#'
NULL
