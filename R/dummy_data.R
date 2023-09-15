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
#' Simulate ipadmdad data
#' 
#' @description
#' This function creates a dummy dataset with a subset of variables that
#' are contained in the GEMINI "ipadmdad" table (see details in
#' [GEMINI Data Repository Dictionary](https://drive.google.com/uc?export=download&id=1iwrTz1YVz4GBPtaaS9tJtU0E9Bx1QSM5)).
#' 
#' The function simulates patient populations that differ across hospitals. That
#' is, patient characteristics like `age`, `gender`, and `discharge_disposition`
#' are simulated separately for each hospital, with a different, randomly drawn
#' distribution mean (i.e., random intercepts).
#' 
#' All other variables are simulated independently of each other, i.e., there is
#' no correlation between patient characteristics (e.g., age and discharge
#' disposition) that may exist in real data. 
#'
#' @param n (`integer`)\cr Total number of encounters (`genc_ids`) to be
#' simulated.
#'
#' @param n_hospitals (`integer`)\cr
#' Number of hospitals to be simulated. Total number of `genc_ids` will be split
#' up psuedo-randomly between hospitals to ensure roughly equal sample size at
#' each hospital.
#'
#' @param fisc_years (`numeric`)\cr
#' A numeric vector containing the time period, specified as fiscal years. For
#' example, `c(2015, 2019)` generates data from 2015-04-01 to 2019-04-01. 
#' 
#' @param plot_hist (`logical`)\cr
#' Whether or not to plot a histogram of all simulated variables. 
#'
#' @return (`data.table`)\cr A data.table object similar to the "ipadmdad" table
#' containing the following fields:
#' - 
#' - 
#' - 
#'
#' @importFrom sn rsn
#' @export
#'
#' @examples
#' ### Simulate 10,000 encounters from 10 hospitals for 2 years from 2018-2020.
#' \dontrun{ 
#' dummy_ipadmdad(10000, n_hospitals = 10, time_period = c(2018, 2020)) }
#'
#' 
#' }
#'
dummy_ipadmdad <- function(n = 1000, 
                           n_hospitals = 10, 
                           time_period = c(2015, 2023),
                           plot_hist = TRUE) {
  
  
  ############### CHECKS: Make sure n_encounters is at least n_hospitals * length(fisc_years)
  
  ############### PREPARE OUTPUT TABLE ###############
  ## create all combinations of hospitals and fiscal years
  hospital_num <- seq(1,n_hospitals,1)
  year <- seq(time_period[1],time_period[2],1)
  
  data <- expand.grid(hospital_num = hospital_num, year = year) %>% data.table()
  
  # randomly draw number of encounters per hospital*year combo 
  # make sure they add up to desired total number of encounters
  data[ , n := rmultinom(1, n_encounters, rep.int(1 / nrow(data), nrow(data)))]
  sum(data$n)
  
  # blow up row number according to encounter per combo 
  data <- data[rep(seq(nrow(data)), data$n),]
  
  # turn year variable into actual date by randomly drawing date_time
  add_random_datetime <- function(year) {
    start_date <- paste0(year, "-04-01 00:00 UTC") # start each fisc year on Apr 1
    end_date <- paste0(year+1, "-03-31 23:59 UTC")   # end of fisc year
    
    random_datetime <- format(as.POSIXct(runif(length(year), as.POSIXct(start_date), as.POSIXct(end_date))), format = "%Y-%m-%d %H:%M") # 
    return(random_datetime)
  }
  
  data[, discharge_date_time := add_random_datetime(year)]
  
  # add genc_id from 1-n
  data <- data[order(discharge_date_time),]
  data[ , genc_id := seq(1,nrow(data),1)]
  

  ############### DEFINE VARIABLE DISTRIBUTIONS ###############
  ## AGE
  # create left-skewed distribution, truncated from 18-110
  age_distr <- function(n=10000, xi=95, omega=30, alpha=-10) {
    
    age <- rsn(n, xi, omega, alpha)
    
    # truncate at [18, 110]
    age <- as.integer(age[age >= 18 & age <= 110])
    
  }

  
  
  
  ############### ADD VARIABLES CLUSTERED BY HOSPITAL ###############
  # Any encounter characteristics (e.g., age/gender/discharge disposition) are
  # simulated as being clustered by hospital (i.e., each hospital will be
  # simulated as random intercept, i.e., different location parameter)
  add_vars <- function(hosp_data) {
    
    ## AGE 
    # create new age distribution for each hospital where location parameter xi
    # varies to create a random intercept by site
    age <- age_distr(xi = rnorm(1,95,5))
    hosp_data$age <- sample(age,nrow(hosp_data),replace=TRUE)
    
    
    
    
    
    
    return(hosp_data)
  }
  
  
  
  # note: split data by hospital before running foverlaps to avoid working with massive tables
  cohort_hospitals <- split(data, data$hospital_num)
  data_all <- lapply(cohort_hospitals, add_vars#,
                     #time_period_start = time_period_start,
                     #time_period_end = time_period_end,
                     #group_var = group_var,
                     #scu_exclude = scu_exclude
  )
  
  
  ##  Combine all
  data <- do.call(rbind, data_all)
  
  
  
  
  ############### PLOT HISTOGRAMS ###############
  if (plot_hist){
    
    
    
  }
  
  
  ## Select relevant output variables
  data <- data[ , .(genc_id, hospital_num, discharge_date_time, age)]

  
  
    
  
  return(data)
  
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

#' @title
#' Dummy ipadmdad data
#'
#' @description
#' Used to generate a subset of variables from the ipadmdad table. Simulates
#' clustering by hospital where patient populations have different
#' characteristics at different sites (i.e., simulated data can be used to test
#' random intercept models).
#'
#' @keywords internal
#' @name dummy_ipadmdad
#' @docType data
#'
NULL
