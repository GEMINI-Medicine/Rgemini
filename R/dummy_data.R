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
#' see details in the [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
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
#' \dontrun{
#' sample_icd(100, source = "comorbidity")
#' }
#'
#' ### Simulate 100 ICD-10 codes starting with "C2" or "E10" based on the 2011 version.
#' \dontrun{
#' sample_icd(100, source = "comorbidity", pattern = "^C2|^E10")
#' }
#'
#' ### Simulate 50 ICD-10-CA codes based on codes found in the `lookup_icd10_ca_description` table
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "domain_name.ca",
#'   port = 1234,
#'   user = getPass("Enter user:"),
#'   password = getPass("password")
#' )
#' sample_icd(50, source = "icd_lookup", dbcon = dbcon)
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
        lookup <- RPostgreSQL::dbGetQuery(
          dbcon,
          "SELECT diagnosis_code FROM lookup_icd10_ca_description where type != 'category'"
        ) %>% as.data.table()

        if (!is.null(pattern)) {
          lookup <- lookup[grepl(toupper(pattern), diagnosis_code)]
        }

        if (nrow(lookup) > 0) {
          sample(x = lookup$diagnosis_code, size = n, replace = TRUE)
        } else {
          stop("No matching diagnoses found for the
          specified pattern")
        }
      } else {
        stop("Invalid input for 'dbcon' argument. Database connection
        is required for sampling from `lookup_icd10_ca_to_ccsr` table\n")
      }
    }
  )
}


#' @title
#' Generate Simulated Diagnosis Data Table
#'
#' @description
#' This function generates simulated data table resembling `ipdiagnosis`
#' or `erdiagnosis` tables that can be used for testing or demonstration purposes.
#' It internally calls `sample_icd()` function to sample ICD-10 codes and
#' accepts arguments passed to `sample_icd()` for customizing the sampling scheme.
#'
#' @details
#' To ensure simulated table resembles "ip(er)diagnosis" table, the following characteristics are applied to fields:
#'
#' - `genc_id`: Numerical identification of encounters starting from 1.
#' The number of unique encounters is defined by `n`. The total number of rows is defined by `nrow`,
#'   where the number of rows for each encounter is random, but each encounter has at least one row.
#' - `hospital_num`: Numerical identification of hospitals from 1 to 5.
#' All rows of an encounter are linked to a single hospital
#' - `diagnosis_code`: "ipdiagnosis" table only. Simulated ICD-10 diagnosis codes.
#' Each encounter can be associated with multiple diagnosis codes in long format.
#' - `diagnosis_type`: "ipdiagnosis" table only.
#' The first row of each encounter is consistently assigned to the diagnosis type "M".
#' For the remaining rows, if `diagnosis_type` is specified by users,
#' diagnosis types are sampled randomly from values provided;
#' if `diagnosis_type` is NULL, diagnosis types are sampled from
#' ("1", "2", "3", "4", "5", "6", "9", "W", "X", and "Y"),
#' with sampling probability proportionate to their prevalence in the "ipdiagnosis" table.
#' - `diagnosis_cluster`: "ipdiagnosis" table only.
#' Proportionally sampled from values that have a prevalence of more than 1%
#' in the "diagnosis_cluster" field of the "ipdiagnosis" table, which are ("", "A", "B").
#' - `diagnosis_prefix`: "ipdiagnosis" table only.
#' Proportionally sampled from values that have a prevalence of more than 1%
#' in the "diagnosis_prefix" field of the "ipdiagnosis" table, which are ("", "N", "Q", "6").
#' - `er_diagnosis_code`: "erdiagnosis" table only.
#' Simulated ICD-10 diagnosis codes.
#' Each encounter can be associated with multiple diagnosis codes in long format.
#' - `er_diagnosis_type`: "erdiagnosis" table only.
#' Proportionally sampled from values that have a prevalence of more than 1%
#' in the "er_diagnosis_type" field of the "erdiagnosis" table, which are ("", "M", "9", "3", "O").
#'
#'
#' @note The following fields `(er)diagnosis_code`, `(er)diagnosis_type`, `diagnosis_cluster`, `diagnosis_prefix`
#' are simulated independently.
#' Therefore, the simulated combinations may not reflect the interrelationships of these fields in actual data.
#' For example, specific diagnosis codes may be associated with specific diagnosis types,
#' diagnosis clusters, or diagnosis prefix in reality.
#' However, these relationships are not maintained for the purpose of generating dummy data.
#' Users require specific linkages between these fields should consider customizing
#' the output data or manually generating the desired combinations.
#'
#' @param nid (`integer`)\cr Number of unique encounter IDs (`genc_id`) to simulate. Value must be greater than 0.
#'
#' @param n_hospitals (`integer`)\cr Number of hospitals to simulate in the resulting data table
#'
#' @param cohort (`data.frame`)\cr Optional, the administrative data frame containing `genc_id`
#' and `hospital_num` information to be used in the output. `cohort` takes precedence over parameters `nid` and
#' `n_hospitals`: when `cohort` is not NULL, `nid` and `n_hospitals` are ignored.
#'
#' @param ipdiagnosis (`logical`)\cr Default to "TRUE" and returns simulated "ipdiagnosis" table.
#' If FALSE, returns simulated "erdiagnosis" table.
#' See tables in [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#'
#' @param diagnosis_type (`character vector`)\cr The type(s) of diagnosis to return.
#' Possible diagnosis types are
#' ("M", 1", "2", "3", "4", "5", "6", "9", "W", "X", and "Y"). Regardless of `diagnosis_type` input,
#' the `ipdiagnosis` table is defaulted to always return type "M" for the first row of each encounter.
#'
#' @param seed (`integer`)\cr Optional, a number to assign the seed to.
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
#' erdiag <- dummy_diag(nid = 50, n_hospitals = 2, ipdiagnosis = F)
#' }
#'
#' ### Simulate an erdiagnosis table including data from `cohort`
#' cohort <- dummy_ipadmdad()
#' erdiag <- dummy_diag(cohort = cohort)
#'
#' ### Simulate an ipdiagnosis table with diagnosis codes starting with "E11":
#' \dontrun{
#' set.seed(1)
#' ipdiag <- dummy_diag(nid = 50, n_hospitals = 20, ipdiagnosis = T, pattern = "^E11")
#' }
#'
#' ### Simulate a ipdiagnosis table with random diagnosis codes in diagnosis type 3 or 6 only:
#' \dontrun{
#' set.seed(1)
#' ipdiag <- dummy_diag(nid = 50, n_hospitals = 10, diagnosis_type = (c("3", "6"))) %>%
#'   filter(diagnosis_type != "M") # remove default rows with diagnosis_type="M" from each ID
#' }
#'
#' ### Simulate a ipdiagnosis table with ICD-10-CA codes:
#' \dontrun{
#' drv <- dbDriver("PostgreSQL")
#' dbcon <- DBI::dbConnect(drv,
#'   dbname = "db",
#'   host = "172.XX.XX.XXX",
#'   port = 1234,
#'   user = getPass("Enter user:"),
#'   password = getPass("password")
#' )
#'
#' set.seed(1)
#' ipdiag <- dummy_diag(nid = 5, n_hospitals = 2, ipdiagnosis = T, dbcon = dbcon, source = "icd_lookup")
#' }
#'
dummy_diag <- function(
  nid = 1000, n_hospitals = 10, cohort = NULL, ipdiagnosis = TRUE, diagnosis_type = NULL, seed = NULL, ...
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  #### get data.tables with  `genc_id` and `hospital_num` ####
  # `ipdiagnosis`: average number of repeats is 9.05
  # `df1` and `df2` will be joined, so `df2` has 8.05 repeats on average
  # `df` has one repeat per genc_id
  # `erdiagnosis` has 3.92 repeats per genc_id on average
  # the average repeats in `df2` is 2.92 for `erdiagnosis`
  avg_repeats <- ifelse(ipdiagnosis, 8.05, 2.92)
  if (is.null(cohort)) {
    df2 <- generate_id_hospital(nid = nid, n_hospitals = n_hospitals, avg_repeats = avg_repeats, seed = seed)
  } else {
    cohort <- as.data.table(cohort)
    df2 <- generate_id_hospital(
      cohort = cohort,
      avg_repeats = avg_repeats,
      include_prop = 1,
      seed = seed
    )
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
  n_rows <- nrow(df1) + nrow(df2)

  ##### sample `diagnosis_codes` #####
  # combine `df1` with "M" diagnosis types and `df2` with other diagnosis types
  dummy_data <- rbind(df1, df2) %>%
    mutate(
      diagnosis_code = sample_icd(n = n_rows, ...),
      diagnosis_cluster = sample(c("", "A", "B"),
        size = n_rows,
        replace = TRUE,
        prob = c(0.92, 0.07, 0.01)
      ),
      diagnosis_prefix = sample(c("", "N", "Q", "6"),
        size = n_rows,
        replace = TRUE,
        prob = c(0.9, 0.05, 0.02, 0.01)
      )
    )

  if (ipdiagnosis == FALSE) {
    if (!is.null(diagnosis_type)) {
      er_diagnosis_type <- sample(diagnosis_type, size = n_rows, replace = TRUE)
    } else {
      er_diagnosis_type <- sample(c("", "M", "9", "3", "O"),
        size = n_rows, replace = TRUE, prob = c(0.53, 0.38, 0.06, 0.02, 0.01)
      )
    }
    dummy_data <- dummy_data %>%
      dplyr::select(-diagnosis_cluster, -diagnosis_prefix, -diagnosis_type) %>%
      mutate(er_diagnosis_type = er_diagnosis_type) %>%
      rename(er_diagnosis_code = diagnosis_code)
  }

  return(dummy_data[order(dummy_data$genc_id)])
}

#' @title
#' Simulate ipadmdad data
#'
#' @description
#' This function creates a dummy dataset with a subset of variables that
#' are contained in the GEMINI "ipadmdad" table (see details in
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/)).
#'
#' The simulated encounter-level variables that are returned by this function
#' are currently: Admission date-time, discharge date-time, age, gender,
#' discharge disposition, transfer to an alternate level of care (ALC), and ALC
#' days. The distribution of these simulated variables roughly mimics the real
#' distribution of each variable observed in the GIM cohort from 2015-2022.
#' Admission date-time is simulated in conjunction with discharge date-time to
#' mimic realistic length of stay. All other variables are simulated
#' independently of each other, i.e., there is no correlation between age,
#' gender, discharge disposition etc. that may exist in the real data. One
#' exception to this is `number_of_alc_days`, which is only > 0 for entries
#' where `alc_service_transfer_flag == TRUE` and the length of ALC is capped at
#' the total length of stay.
#'
#' The function simulates patient populations that differ across hospitals. That
#' is, patient characteristics are simulated separately for each hospital, with
#' a different, randomly drawn distribution mean (i.e., random intercepts).
#' However, the degree of hospital-level variation simulated by this function
#' is arbitrary and does not reflect true differences between hospitals in the
#' real GEMINI dataset.
#'
#' @param nid (`integer`)\cr Total number of encounters (`genc_ids`) to be
#' simulated.
#'
#' @param n_hospitals (`integer`)\cr
#' Number of hospitals to be simulated. Total number of `genc_ids` will be split
#' up pseudo-randomly between hospitals to ensure roughly equal sample size at
#' each hospital.
#'
#' @param time_period (`numeric`)\cr
#' A numeric vector containing the time period, specified as fiscal years
#' (starting in April each year). For example, `c(2015, 2019)` generates data
#' from 2015-04-01 to 2020-03-31.
#'
#' @return (`data.frame`)\cr A data.frame object similar to the "ipadmdad" table
#' containing the following fields:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `hospital_num` (`integer`): Hospital ID
#' - `admission_date_time` (`character`): Date-time of admission in YYYY-MM-DD HH:MM format
#' - `discharge_date_time` (`character`): Date-time of discharge in YYYY-MM-DD HH:MM format
#' - `age` (`integer`): Patient age
#' - `gender` (`character`): Patient gender (F/M/O for Female/Male/Other)
#' - `discharge_disposition` (`integer`): All valid categories according to DAD
#' abstracting manual 2022-2023
#'    - 4: Home with Support/Referral
#'    - 5: Private Home
#'    - 8: Cadaveric Donor (does not exist in GEMINI data)
#'    - 9: Stillbirth (does not exist in GEMINI data)
#'    - 10: Transfer to Inpatient Care
#'    - 20: Transfer to ED and Ambulatory Care
#'    - 30: Transfer to Residential Care
#'    - 40: Transfer to Group/Supportive Living
#'    - 90: Transfer to Correctional Facility
#'    - 61: Absent Without Leave (AWOL)
#'    - 62: Left Against Medical Advice (LAMA)
#'    - 65: Did not Return from Pass/Leave
#'    - 66: Died While on Pass/Leave
#'    - 67: Suicide out of Facility (does not exist in GEMINI data)
#'    - 72: Died in Facility
#'    - 73: Medical Assistance in Dying (MAID)
#'    - 74: Suicide in Facility
#' - `alc_service_transfer_flag` (`character`): Variable indicating whether
#' patient was transferred to an alternate level of care (ALC) during their
#' hospital stay. Coding is messy and varies across sites. Possible values are:
#'    - Missing: `NA`, `""`
#'    - True: `"TRUE"/"true"/"T"`, `"y"/"Y"`, `"1"/"99"`, `"ALC"`
#'    - False: `"FALSE"/"false"`, `"N"`, `"0"`, `"non-ALC"`
#' Some entries with missing `alc_service_transfer_flag` can be inferred based
#' on value of `number_of_alc_days` (see below)
#' - `number_of_alc_days` (`integer`): Number of days spent in ALC (rounded to
#' nearest integer). If `number_of_alc_days = 0`, no ALC occurred;
#' if `number_of_alc_days > 0`, ALC occurred.
#' Note that days spent in ALC should usually be < length of
#' stay. However, due to the fact that ALC days are rounded up, it's possible
#' for `number_of_alc_days` to be larger than `los_days_derived`.
#'
#' @importFrom sn rsn
#' @importFrom MCMCpack rdirichlet
#' @importFrom lubridate ymd_hm
#' @export
#'
#' @examples
#' # Simulate 10,000 encounters from 10 hospitals for fiscal years 2018-2020.
#' ipadmdad <- dummy_ipadmdad(nid = 10000, n_hospitals = 10, time_period = c(2018, 2020))
#'
dummy_ipadmdad <- function(nid = 1000,
                           n_hospitals = 10,
                           time_period = c(2015, 2023),
                           seed = NULL) {
  ############### CHECKS: Make sure n is at least n_hospitals * length(time_period)
  if (nid < n_hospitals * length(time_period)) {
    stop("Invalid user input.
    Number of encounters `nid` should at least be equal to `n_hospitals` * `length(time_period)`")
  }

  # set the seed if the input provided is not NULL
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ############### PREPARE OUTPUT TABLE ###############
  ## create all combinations of hospitals and fiscal years
  hospital_num <- seq(1, n_hospitals, 1)
  year <- seq(time_period[1], time_period[2], 1)

  data <- expand.grid(hospital_num = as.integer(hospital_num), year = year) %>% data.table()

  # randomly draw number of encounters per hospital*year combo
  data[, n := rmultinom(1, nid, rep.int(1 / nrow(data), nrow(data)))]

  # blow up row number according to encounter per combo
  data <- data[rep(seq_len(nrow(data)), data$n), ]

  # turn year variable into actual date by randomly drawing date_time
  add_random_datetime <- function(year) {
    start_date <- paste0(year, "-04-01 00:00 UTC") # start each fisc year on Apr 1
    end_date <- paste0(year + 1, "-03-31 23:59 UTC") # end of fisc year

    random_date <- as.Date(round(runif(length(year),
      min = as.numeric(as.Date(start_date)),
      max = as.numeric(as.Date(end_date))
    )))

    random_datetime <- format(as.POSIXct(random_date + dhours(sample_time_shifted(length(year),
      xi = 19.5, omega = 6.29, alpha = 0.20
    )), tz = "UTC"), format = "%Y-%m-%d %H:%M")

    return(random_datetime)
  }

  data[, admission_date_time := add_random_datetime(year)]

  # add genc_id from 1-n
  data <- data[order(admission_date_time), ]
  data[, genc_id := as.integer(seq(1, nrow(data), 1))]


  ############### DEFINE VARIABLE DISTRIBUTIONS ###############
  ## AGE
  # create left-skewed distribution, truncated from 18-110
  age_distr <- function(nid = 10000, xi = 95, omega = 30, alpha = -10) {
    age <- rsn(nid, xi, omega, alpha)

    # truncate at [18, 110]
    age <- as.integer(age[age >= 18 & age <= 110])
  }


  ############### ADD VARIABLES CLUSTERED BY HOSPITAL ###############
  # Any encounter characteristics (e.g., age/gender/discharge disposition) are
  # simulated as being clustered by hospital (i.e., each hospital will be
  # simulated as random intercept, i.e., different location parameter)
  add_vars <- function(hosp_data) {
    n_enc <- nrow(hosp_data)

    ## AGE
    # create new age distribution for each hospital where location parameter xi
    # varies to create a random intercept by site
    age <- age_distr(xi = rnorm(1, 95, 5))
    hosp_data[, age := sample(age, n_enc, replace = TRUE)]

    ## Gender (F/M/Other)
    prob <- data.table(
      "gender" = c("F", "M", "O"),
      "p" = c(.501, .498, 0.001 + 1e-5)
    ) # add small constant to Os to ensure it's not rounded to 0 below
    # Introduce random hospital-level variability
    prob[, p := t(rdirichlet(1, alpha = prob$p / 0.005))] # 0.005 = level of variability
    hosp_data[, gender := sample(prob$gender, n_enc,
      replace = TRUE,
      prob$p / sum(prob$p)
    )]
    # make sure probs add up to 1 (see addition of constant above)

    ## DISCHARGE DISPOSITION
    prob <- data.table(
      "discharge_disposition" = c(4, 5, 8, 9, 10, 20, 30, 40, 61, 62, 65, 66, 67, 72, 73, 74, 90),
      "p" = c(
        .275, .386, 0, 0, .143, 0.002, .045, .040, .001 + 1e-5, .028, 0.001 + 1e-5, 0.001 + 1e-5,
        0, .079, .001 + 1e-5, 0.001 + 1e-5, .001
      )
    ) # add small constant to Os to ensure it's not rounded to 0 below
    prob[, p := t(rdirichlet(1, alpha = prob$p / 0.005))] # 0.005 = level of hospital-level variability
    hosp_data[, discharge_disposition := as.integer(sample(prob$discharge_disposition, n_enc,
      replace = TRUE, prob$p / sum(prob$p)
    ))] # make sure probs add up to 1 (see addition of constant above)

    ## Simulate LOS to derive discharge_date_time
    # create right-skewed distribution with randomly drawn offset by site]
    hosp_data[, los := {
      mean_hosp <- rnorm(1, mean = 1.27, sd = 0.11)
      rlnorm(.N, meanlog = mean_hosp, sdlog = 1.38)
    }, by = hospital_num] # hospital-level variation in distribution

    hosp_data[, discharge_date_time := format(
      round_date(as.POSIXct(admission_date_time, tz = "UTC") +
        ddays(los), unit = "days") +
        dhours(sample_time_shifted(.N, xi = 11.37, omega = 4.79, alpha = 1.67, max = 28, seed = seed)),
      format = "%Y-%m-%d %H:%M", tz = "UTC"
    )]

    # if `discharge_date_time` ends up before `admission_date_time`
    hosp_data[, los := as.numeric(difftime(ymd_hm(discharge_date_time), ymd_hm(admission_date_time), units = "days"))]
    hosp_data[los < 0, discharge_date_time := format(ymd_hm(discharge_date_time) + days(1), "%Y-%m-%d %H:%M")]
    # handle sampling edge case with negative los

    ## Alternate level of care (ALC) & days spent in ALC
    # ALC flag
    prob <- data.table(
      "alc_service_transfer_flag" = c("FALSE", "TRUE", NA),
      "p" = c(.85, .11, .04)
    )
    prob[, p := t(rdirichlet(1, alpha = prob$p / 0.05))] # 0.05 = level of variability
    hosp_data[, alc_service_transfer_flag := sample(prob$alc_service_transfer_flag, n_enc,
      replace = TRUE,
      prob$p / sum(prob$p)
    )] # make sure probs add up to 1 (see addition of constant above)

    # Days spent in ALC (as integer)
    # If ALC = FALSE, ALC days are either coded as 0 or NA (random across sites)
    hosp_data[alc_service_transfer_flag == "FALSE", number_of_alc_days := sample(c(0, NA), 1, prob = c(.8, .2))]
    # If ALC = TRUE, ALC days are drawn from uniform distribution between 0 and LOS
    # (divided by 1.5 because ALC should be < LOS)
    # Note: because ALC is rounded UP, this results in some entries where ALC > LOS
    # (especially for cases with short LOS); this mimics entries we find in our real data as well
    hosp_data[alc_service_transfer_flag == "TRUE", number_of_alc_days := ceiling(runif(.N, 0, ceiling(los / 1.5)))]
    # for cases where number_of_alc_days != NA,
    # alc_service_transfer_flag is NA anywhere from 0-100% by site (mostly 0 or 100, but some in-between),
    # so let's mimic that
    hosp_data[
      genc_id %in% hosp_data[!is.na(number_of_alc_days)][
        sample(.N, size = round(sample(c(0, .25, .50, .75, 1), prob = c(.59, .05, .05, .01, .3), 1) * .N)), "genc_id"
      ],
      alc_service_transfer_flag := NA
    ]

    # randomly recode values referring to FALSE/TRUE to simulate real messiness of ALC coding
    coding <- t(
      data.table(
        code1 = c("FALSE", "TRUE"),
        code2 = c("0", "1"),
        code3 = c("0", "99"),
        code4 = c("N", "Y"),
        code5 = c("n", "y"),
        code6 = c("false", "true"),
        code7 = c("non-ALC", "ALC"),
        code8 = c(NA, "Y")
      ) # this is intentional, some sites only code "true", everything else is missing...
    )
    code <- sample(seq_len(nrow(coding)), 1)

    hosp_data[alc_service_transfer_flag == FALSE, alc_service_transfer_flag := coding[code, 1]]
    hosp_data[alc_service_transfer_flag == TRUE, alc_service_transfer_flag := coding[code, 2]]
    # code missing as NA or "" (randomly per site)
    hosp_data[is.na(alc_service_transfer_flag), alc_service_transfer_flag := sample(c(NA, ""), 1, prob = c(.8, .2))]

    return(hosp_data)
  }


  # note: split data by hospital before running foverlaps to avoid working with massive tables
  cohort_hospitals <- split(data, data$hospital_num)
  data_all <- lapply(cohort_hospitals, add_vars)


  ##  Combine all
  data <- do.call(rbind, data_all)

  ## Select relevant output variables
  data <- data[order(genc_id), .(
    genc_id,
    hospital_num,
    admission_date_time,
    discharge_date_time,
    age,
    gender,
    discharge_disposition,
    alc_service_transfer_flag,
    number_of_alc_days
  )]

  # Return as data.frame (instead of data.table) as this is what SQL queries return
  data <- as.data.frame(data)

  return(data)
}


#' @title
#' Generated simulated lab data
#'
#' @description
#' Designed to mimic the most important elements of the GEMINI lab table as defined in the
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
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
#' lab <- dummy_lab(1, 3024641, c(7, 8, 15, 30), "mmol/L", "2023-01-02 08:00")
#'
dummy_lab <- function(id, omop, value, unit, mintime) {
  res <- data.table(
    genc_id = rep(id, length(value)),
    test_type_mapped_omop = omop,
    result_value = value,
    result_unit = rep(unit, length(value)),
    collection_date_time = format(as.POSIXct(mintime, tz = "UTC") +
      sample(0:(24 * 60 * 60 - 1),
        size = length(value),
        replace = TRUE
      ), "%Y-%m-%d %H:%M")
  )
  return(res)
}


#' @title
#' Generated simulated administrative data
#'
#' @description
#' Designed to partially mimic the `admdad` table as defined in the
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
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
dummy_admdad <- function(id, admtime) {
  res <- data.table(
    genc_id = id,
    admission_date_time = format(as.POSIXct(admtime, tz = "UTC"), "%Y-%m-%d %H:%M")
  )
  return(res)
}

#' @title
#' Generate simulated radiology data
#'
#' @description
#' This function creates a dummy dataset with a subset of variables that
#' are contained in the GEMINI "radiology" table, as seen in
#' [GEMINI Data Repository Dictionary](https://geminimedicine.ca/the-gemini-database/).
#'
#' This function only simulates modalities used in Our Practice Report (OPR) - CT, MRI,
#' Ultrasound. It does not cover all modalities seen in the actual "radiology" data table.
#'
#' @param nid (`integer`)\cr Number of unique encounter IDs to simulate.
#' Encounter IDs may repeat to simulate multiple radiology tests,
#' resulting in a data table with more rows than `nid`.
#'
#' @param n_hospitals(`integer`)\cr The number of hospitals to simulate.
#' It is optional if `cohort` is provided.
#'
#' @param time_period (`vector`)\cr A numeric or character vector containing the data range of the data
#' by years or specific dates in either format: ("yyyy-mm-dd", "yyyy-mm-dd") or (yyyy, yyyy)
#' The start date and end date will be (yyyy-01-01 and yyyy-12-31) if (yyyy, yyyy)
#' is the date range format provided. Optional when `cohort` is provided.
#'
#' @param cohort  (`data.frame | data.table`)\cr Optional, data frame with the following columns:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `hospital_num` (`integer`): Hospital ID
#' - `admission_date_time` (`character`): Date and time of IP admission in YYYY-MM-DD HH:MM format
#' - `discharge_date_time` (`character`): Date and time of IP discharge in YYYY-MM-DD HH:MM format.
#' When `cohort` is not NULL, `nid`, `n_hospitals`, and `time_period` are ignored.
#'
#' @param seed (`integer`)\cr Optional, a number to be used to set the seed for reproducible results.
#'
#' @return (`data.table`)\cr A `data.table` object similar that contains the following fields:
#' - `genc_id` (`integer`): GEMINI encounter ID
#' - `modality_mapped` (`character`): Imaging modality: either MRI, CT, or Ultrasound.
#' - `ordered_date_time` (`character`): The date and time the radiology test was ordered
#' - `performed_date_time` (`character`): The date and time the radiology test was performed
#' @examples
#' cohort <- dummy_ipadmdad()
#' dummy_radiology(cohort = cohort)
#' dummy_radiology(n = 1000, n_hospitals = 10, time_period = c(2020, 2023))
#'
#' @export

dummy_radiology <- function(
  nid = 1000, n_hospitals = 10, time_period = c(2015, 2023), cohort = NULL, seed = NULL
) {
  ####### checks for valid inputs #######
  if (!is.null(cohort)) { # if `cohort` is provided
    check_input(cohort,
      c("data.frame", "data.table"),
      colnames = c("genc_id", "hospital_num", "admission_date_time", "discharge_date_time"),
      coltypes = c("integer", "integer", "", "")
    )
    if (!all(check_date_format(c(cohort$admission_date_time, cohort$discharge_date_time)))) {
      stop("An invalid date input was provided in cohort.")
    }
  } else { # when `cohort` is not provided
    check_input(list(nid, n_hospitals), "integer")
    
    #  check if time_period is provided/has both start and end dates
    if (is.null(time_period) | is.na(time_period) || length(time_period) != 2) {
      stop("Please provide time_period") # check for date formatting
    } else if (!check_date_format(time_period[1]) || !check_date_format(time_period[2])) {
      stop("Time period is in the incorrect date format, please fix")
    }

    if (as.Date(time_period[1]) > as.Date(time_period[2])) {
      print("Time period needs to end later than it starts")
      stop()
    }

    if (nid < n_hospitals) {
      print("Number of encounters must be greater than or equal to the number of hospitals")
      stop()
    }
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (!is.null(cohort)) {
    ####### if `cohort` is provided, create `df1` based on it #######
    cohort <- as.data.table(cohort)
    cohort$admission_date_time <- as.POSIXct(cohort$admission_date_time,
      format = "%Y-%m-%d %H:%M",
      tz = "UTC"
    )

    cohort$discharge_date_time <- as.POSIXct(cohort$discharge_date_time,
      format = "%Y-%m-%d %H:%M",
      tz = "UTC"
    )

    cohort$los <- as.numeric(difftime(
      cohort$discharge_date_time,
      cohort$admission_date_time,
      units = "hours"
    ))

    # generate df1 based on cohort
    df1 <- generate_id_hospital(cohort = cohort, include_prop = 1, avg_repeats = 4.5, seed = seed)

    nid <- uniqueN(df1$genc_id)
    n_hospitals <- uniqueN(df1$hospital_num)

    ####### sample the gap between IP admission and ordered date time #######
    # in days
    admit_order_gap <- round(rlnorm(nrow(df1), meanlog = 0.817, sdlog = 1.215) - 0.8)

    ####### Set the ordered date time #######
    # get ordered date
    df1[, ordered_date := as.Date(admission_date_time) + ddays(admit_order_gap)]

    # sample ordered time
    df1[, ordered_time := sample_time_shifted(.N, xi = 7.9, omega = 8.8, alpha = 4.5, min = 4, max = 30, seed = seed)]

    ####### get ordered date time by combining ordered date and time #######
    df1[, ordered_date_time := ordered_date + dhours(ordered_time)]

    # ensure that `ordered_date_time` is not after `discharge_date_time`
    # re-sample bad values
    while (nrow(df1[ordered_date_time >= discharge_date_time, ]) > 0) {
      df1[
        ordered_date_time >= discharge_date_time,
        ordered_date_time := as.Date(admission_date_time) +
          dhours(sample_time_shifted(.N, xi = 7.9, omega = 8.8, alpha = 4.5, min = 4, max = 30))
      ]
    }

    # sample gap between ordered and performed date time, in hours
    # maximum perform gap is the difference between IP discharge and `ordered_date_time`
    # this prevents `perform_date_time` from being after `discharge_date_time`
    df1[, max_perform_gap := as.numeric(
      difftime(discharge_date_time, ordered_date_time, units = "hours")
    )]

    df1[, perform_gap := rlnorm_trunc(
      .N,
      meanlog = 1.3, sdlog = 1.9, min = 0, max = max_perform_gap
    )]

    df1[sample(nrow(df1), round(0.06 * nrow(df1))), perform_gap := 0] # set some values to 0

    ####### Get performed date time by adding `perform_gap` to `ordered_date_time` #######
    df1[, performed_date_time := ordered_date_time + dhours(perform_gap)]

    # performed date time should not be after discharge
    # re-sample it
    df1[performed_date_time > discharge_date_time, performed_date_time := ordered_date_time +
      dhours(
        rlnorm_trunc(
          .N,
          meanlog = 1.3, sdlog = 1.9, min = 0, max = as.numeric(
            difftime(discharge_date_time, ordered_date_time, units = "hours")
          )
        )
      )]

    df1[performed_date_time > discharge_date_time, performed_date_time := discharge_date_time]

    df1[, ordered_date_time := substr(as.character(ordered_date_time), 1, 16)]
    df1[, performed_date_time := substr(as.character(performed_date_time), 1, 16)]
  } else {
    ####### if `cohort` is not provided, use parameters to get `df1` #######
    # check that `time_period` is valid
    time_period <- as.character(time_period)

    # get the start and end date
    if (grepl("^\\d{4}$", time_period[1])) {
      start_date <- as.Date(paste0(time_period[1], "-01-01"))
    } else {
      start_date <- as.Date(time_period[1])
    }

    if (grepl("^\\d{4}$", time_period[1])) {
      end_date <- as.Date(paste0(time_period[2], "-01-01"))
    } else {
      end_date <- as.Date(time_period[2])
    }

    # get a data table with hospital ID and encounter ID
    df1 <- generate_id_hospital(nid, n_hospitals, avg_repeats = 4.5, seed = seed)
    df1[, num_id_repeat := .N, by = genc_id]

    ####### sample `ordered_date_time` #######
    # get ordered time
    df1[, ordered_time := sample_time_shifted(
      nrow = nrow(df1), xi = 7.9, omega = 8.8, alpha = 4.5, min = 4, max = 30, seed = seed
    )]

    # for each `genc_id`, sample a minimum and maximum ordered date
    # this is the time range where they will have radiology scans
    df1[, min_ordered_date := as.Date(round(runif(1,
      min = as.numeric(start_date),
      max = as.numeric(end_date)
    ))), by = genc_id]

    # set time range for ordered date times
    # ensure it is not greater than `end_date`
    df1[, max_ordered_date := as.Date(min_ordered_date) +
      ddays(ceiling(rlnorm(1, meanlog = 1.33, sdlog = 1.66))),
    by = genc_id
    ]

    # Get a longer range of values for genc_id with more repeats
    df1[num_id_repeat >= 4, max_ordered_date := as.Date(min_ordered_date) +
      ddays(ceiling(rlnorm(1, meanlog = 2.27, sdlog = 1.36))),
    by = genc_id
    ]

    # ensure `max_ordered_date` it does not exceed `end_date`
    df1[max_ordered_date > end_date, max_ordered_date := end_date]
    # protect from `min_ordered_date` being after `max_ordered_date`
    # set it to either the median date range or start date in the range
    df1[min_ordered_date > max_ordered_date, min_ordered_date := max(start_date, min_ordered_date + 4)]

    # uniformly sample an ordered date during the range
    df1[, ordered_date := as.Date(round(
      runif(.N, min = as.numeric(min_ordered_date), max = as.numeric(max_ordered_date))
    ))]

    # get `ordered_date_time` by comibining the date and time
    df1[, ordered_date_time := ymd(df1$ordered_date) + dhours(ordered_time)]

    ####### Sample `performed_date_time` #######
    # based on ordered date time
    # sample delay time (hours) between ordered and performed
    # add delay time to ordered date time
    df1[, perform_gap := ifelse(rbinom(.N, 1, 0.06),
      0,
      rlnorm(.N, meanlog = 1.3, sdlog = 1.9)
    )]

    df1[, performed_date_time := ordered_date_time + dhours(perform_gap)]

    # remove seconds from date times and turn it into a string
    df1[, ordered_date_time := substr(as.character(ordered_date_time), 1, 16)]
    df1[, performed_date_time := substr(as.character(performed_date_time), 1, 16)]
  }

  # only include the relevant columns from `df1`
  df1 <- df1[, c("genc_id", "hospital_num", "ordered_date_time", "performed_date_time")]

  ####### Get `modality_mapped` #######
  # probabilities of included modalities
  prob <- data.table(
    "modality_mapped" = c("CT", "MRI", "Ultrasound"),
    "p" = c(0.6, 0.1, 0.3)
  )
  # Introduce random hospital-level variability in modality proportions
  # 0.005 = level of variability
  df1[, p := list(list(as.numeric(t(rdirichlet(1, alpha = prob$p / 0.005))))), by = hospital_num]

  df1[, modality_mapped := sapply(p, function(v) {
    base::sample(prob$modality_mapped, 1, replace = TRUE, prob = v / (sum(v)))
  })]

  # hospitals without MRI
  # In real data, not all hospitals have an MRI machine on site.
  # Randomly select hospitals without MRI (~10%) and replace modality with CT and Ultrasound
  hosp_no_mri <- sample(unique(df1$hospital_num), round(n_hospitals * 0.1), replace = FALSE)

  df1[hospital_num %in% hosp_no_mri, p_no_mri := as.numeric(runif(1, 0.55, 0.75)), by = hospital_num]
  df1[hospital_num %in% hosp_no_mri, modality_mapped := sample(
    c("CT", "Ultrasound"),
    .N,
    prob = c(.SD[1, p_no_mri], 1 - .SD[1, p_no_mri]),
    replace = TRUE
  ),
  by = hospital_num
  ]

  # return final data table without the extra columns
  df1 <- df1[, -c("p", "p_no_mri")]

  return(df1[order(df1$genc_id)])
}
