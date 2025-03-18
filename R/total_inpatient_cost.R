# Function that derives cost of hospitalization based on
# Resource Intensity Weights (RIW), which represents weighted costs relative to
# average inpatient cost. The actual cost amount can be inferred based on CMG
# methodology year (by year, based on when the data were pulled) and needs to
# be adjused for (healthcare specific) inflation.
# The suggested general approach used in the Pediatric Complexity project:
# 1. Create a scv file for CPWC(CHSC) for each hospital number and year, based
# on cihi info
# 2. This CSV file then needs to be merged with other GEMINI tables as follows:
# 3. Merge ipcmg into your cohort by genc_id, assess missingness of methodology
#    year and riw_15
# 4. Methodology_year has some missing data. If methodology_year is empty, but
#    there is another row that is otherwise identical (in terms of cmg,
#    diagnosis_for_cmg_assessment, comorbidity_level, riw_inpatient_atypical_
#    indicator), impute the methodology_year from that row.
# 5. Methodology_year also has some values that are >1 year different than 
#    calendar year, calling into question data quality. We use the same
#    process as above to impute methodology years in rows where methodology_
#    year and calendar year are > 1 year apart.
# 6. Report all records excluded due to missing values required for total cost
#    calculation (i.e. RIW).
# 7. Mergein CHSC data by hospital_num
# 8. Multiply riw_15 by CHSC
# 9. Adjust for healthcare-specific inflation
# Once the method has been developed, we should triangulate the results with
# some publicly reported values (probably from CIHI) to make sure we are in the
# ballpark.

