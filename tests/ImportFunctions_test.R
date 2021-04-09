rm(list = ls())

DIR_BASE <- "/Users/samualmacdonald/Documents/causality_ra_work"
DIR_DATA <- paste(DIR_BASE, "data/ISARIC_2020_007_SHRAPNEL_KIDN/Data - 11DEC2020", sep = '/')
DIR_SOURCE <- paste(DIR_BASE, 'repositories/CovidClinicalDataProcessor/R', sep = '/')
FILE_DATA_DICT <- paste(DIR_DATA, '..', 'IDDO SDTM Data Dictionary_2020-10-26.xlsx', sep = '/')

# load package...
devtools::load_all(paste(DIR_BASE, 'repositories/CovidClinicalDataProcessor', sep = '/'))

library("readxl")

# get list of files
df_data_dict <- read_excel(FILE_DATA_DICT) %>% as_tibble() %>% select(c('Domain', 'Domain Name')) %>% distinct()

##########################
### test one at a time ###
##########################
demog.file.name <- paste(DIR_DATA, 'DM.csv', sep = '/') # Demographics
microb.file.name <- paste(DIR_DATA, 'MB.csv') # Microbiology Specimen
symptoms.file.name = paste(DIR_DATA, 'SA.csv', sep = '/') # Clinical and Adverse Events
pregnancy.file.name = paste(DIR_DATA, 'RP.csv', sep = '/') # Reproductive system findings
ICU.file.name = paste(DIR_DATA, 'HO.csv', sep = '/') # Healthcare encounters 
treatment.file.name = paste(DIR_DATA, 'IN.csv', sep = '/') # Treatments and Interventions
vit_sign.file.name = paste(DIR_DATA, 'VS.csv', sep = '/') # Vital Signs
outcome.file.name = paste(DIR_DATA, 'DS.csv', sep = '/') # Disposition
laboratory.file.name = paste(DIR_DATA, 'LB.csv', sep = '/') # Laboratory Results

date_cdisc_received = as_date("2021-01-01") # !!!

# test 1
df_data <- process.all.data(
  demog.file.name,
  microb.file.name=NA, # checkit out later
  symptoms.file.name = NA, 
  pregnancy.file.name = NA,
  ICU.file.name = NA, 
  treatment.file.name = NA, 
  vit_sign.file.name = NA, 
  outcome.file.name = NA, 
  laboratory.file.name = laboratory.file.name, 
  minimum.comorb=100, 
  minimum.sympt=100, 
  minimum.treatments = 1000, 
  dtplyr.step = FALSE, 
  date_pull = date_cdisc_received 
)

# test for all data sources. 
# ensure dtplyr isn't activate until the end. 