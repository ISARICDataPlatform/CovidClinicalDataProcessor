rm(list = ls())
devtools::load_all('../../CovidClinicalDataProcessor')

# library(dplyr)
# library(dtplyr)
# library(forcats)
# library(lubridate)
# library(purrr)
# library(stringr)
# library(tibble)
# library(tidyfast)
# library(tidyr)
# library(tidyselect)
# library(tidyverse)
# library(import)
# import::from(data.table,as.data.table)
# import::from(data.table,fread)
# import::from(glue,glue)
# import::from(stats,runif)

# library("tidyverse")

library("readxl")

DIR_BASE <- "/Users/samualmacdonald/Documents/causality_ra_work"
DIR_DATA <- paste(DIR_BASE, "data/ISARIC_2020_007_SHRAPNEL_KIDN/Data - 11DEC2020", sep = '/')
DIR_SOURCE <- paste(DIR_BASE, 'repositories/CovidClinicalDataProcessor/R', sep = '/')
FILE_DATA_DICT <- paste(DIR_DATA, '..', 'IDDO SDTM Data Dictionary_2020-10-26.xlsx', sep = '/')

# source dependancies
# source(paste(DIR_SOURCE, 'ImportFunctions.R', sep = '/'))
# source(paste(DIR_SOURCE, 'PreprocessingFunction.R', sep = '/'))

# get list of files 
df_data_dict <- read_excel(FILE_DATA_DICT) %>% as_tibble() %>% select(c('Domain', 'Domain Name')) %>% distinct()

demog.file.name <- paste(DIR_DATA, 'DM.csv', sep = '/') # Demographics
# microb.file.name <- paste(DIR_DATA, 'MB.csv') # Microbiology Specimen
symptoms.file.name = paste(DIR_DATA, 'SA.csv', sep = '/') # Clinical and Adverse Events
# pregnancy.file.name = NA # !!! 
# ICU.file.name = NA # !!! 
treatment.file.name = paste(DIR_DATA, 'IN.csv', sep = '/') # Treatments and Interventions   
vit_sign.file.name = paste(DIR_DATA, 'VS.csv', sep = '/') # Vital Signs
# outcome.file.name = NA # !!! 
laboratory.file.name = paste(DIR_DATA, 'LB.csv', sep = '/') # Laboratory Results       

# test 1
process.all.data(demog.file.name,  # !!! consider replacing filename arguments with dictionary
                 microb.file.name=NA, 
                 symptoms.file.name = symptoms.file.name, 
                 pregnancy.file.name = NA,
                 ICU.file.name = NA, 
                 treatment.file.name = treatment.file.name, 
                 vit_sign.file.name = vit_sign.file.name, 
                 outcome.file.name = NA, 
                 laboratory.file.name = laboratory.file.name, 
                 minimum.comorb=100, 
                 minimum.sympt=100, 
                 minimum.treatments = 1000, 
                 dtplyr.step = FALSE)

# test for all data sources. 
# ensure dtplyr isn't activate until the end. 