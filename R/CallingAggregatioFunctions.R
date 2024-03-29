
#load packages (some of these are not being used but we can delete them later)
library(RColorBrewer)
#library(incidence)
library(shiny)
library(shinydashboard)
library(magrittr)
library(lattice)
library(stringr)
library(plyr)
library(FSA)
library(DescTools)
library(vcd)
library(rcompanion)
library(ggplot2)
library(sgr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(Hmisc)
library(RColorBrewer)
library(dtplyr) 
library(data.table)
library(tidyfast)
library(naniar)
library(shinyWidgets)
library(viridis)
library(hrbrthemes)
library(splitstackshape)
library(glue)
library(lubridate)
library(grid)
library(gtable)
library(gridExtra)








####
folder <- "C:/Users/baruj003/Desktop/21/working_R/oxford/CovidClinicalDataProcessor"
setwd(folder)

folder <- "C:/Users/marti/OneDrive/Documents/ISARIC/data/2021-04-07/aggregated"
setwd(folder)

folder <- "C:/Users/marti/OneDrive/Documents/ISARIC/data/2021-05-24/2021-05-24"
setwd(folder)

load("prepr.tbl.all.rda")
input.tbl<-prepr.tbl_v5 %>% filter(date_start<="2021-09-20"| is.na(date_start))

input.tbl<-random.prepr.tbl

input.tbl<-prepr.tbl
backup<-input.tbl

#####cleaning dataset for the report/dashboard
folder <- "C:/Users/marti/OneDrive/Documents/ISARIC/data/2021-05-24/2021-05-24/aggregated"
setwd(folder)
memory.limit(size=120000)



folder <- "C:/Users/K/OneDrive - Nexus365/ISARIC-dashboard/CovidClinicalDataProcessor/data"
setwd(folder)


#####################tables pre inclusion criteria

summary_input_overall<-summary.input.overall.prep(input.tbl)
save(summary_input_overall, file = "summary_input_overall.rda")


patient.by.country.input <- patient.by.country.prep(input.tbl)
save(patient.by.country.input, file="patient_by_country_input.rda")

data_map <- patient.by.country.map.prep(input.tbl)
save(data_map, file ="data_map.rda")

data_country_input=number_by_region(input.tbl)
save(data_country_input,file="data_country_input.rda")

summary_country_date_input=month_by_region(input.tbl)
save(summary_country_date_input,file="summary_country_date_input.rda")





###################inclusion criteria
input.tbl<-input.tbl%>%
  filter((clin_diag_covid_19==TRUE & is.na(cov_det_id)) | cov_det_id=="POSITIVE")

patient.site.time.map.input<-patient.site.time.map.prep(input.tbl)
save(patient.site.time.map.input, file = "patient_site_time_map_input.rda")

#################after inclusion criteria

case.def.input <- patient.by.case.def.prep_new(input.tbl)
save(case.def.input, file ="data_case_def_input.rda")

summary_input<-summary.input.prep(input.tbl)
save(summary_input, file = "summary_input.rda") 

age.pyramid.input <- age.pyramid.prep(input.tbl)
save(age.pyramid.input, file ="age_pyramid_input.rda")

symptom.prevalence.input<-symptom.prevalence.prep(input.tbl)
save(symptom.prevalence.input, file = "symptom_prevalence_input.rda")

symptom.upset.input<-symptom.upset.prep(input.tbl)
save(symptom.upset.input, file = "symptom_upset_input.rda")



comorbidity.prevalence.input<-comorbidity.prevalence.prep(input.tbl)
save(comorbidity.prevalence.input, file = "comorbidity_prevalence_input.rda")

comorbidity.upset.input<-comorbidity.upset.prep(input.tbl)
save(comorbidity.upset.input, file = "comorbidity_upset_input.rda")

treatment.use.proportion.input<-treatment.use.proportion.prep(input.tbl)
save(treatment.use.proportion.input, file = "treatment_use_proportion_input.rda")

treatment.upset.input<-treatment.upset.prep(input.tbl)
save(treatment.upset.input, file = "treatment_upset_input.rda")

icu.treatment.use.proportion.input<-icu.treatment.use.proportion.prep(input.tbl)
save(icu.treatment.use.proportion.input, file = "icu_treatment_use_proportion_input.rda")

icu.treatment.upset.input<-treatment.icu.upset.prep(input.tbl)
save(icu.treatment.upset.input, file = "icu_treatment_upset_input.rda")


patient.characteristic.table<-patient.characteristic.prep(input.tbl)
save(patient.characteristic.table, file = "patient_characteristic_table.rda")

outcome.age.sex.table<-outcome.age.sex.prep(input.tbl)
save(outcome.age.sex.table, file = "outcome_age_sex_table.rda")

symptoms.table<-symptoms.prep(input.tbl)
save(symptoms.table, file = "symptoms_table.rda")

comorbidity.table<-comorbidity.prep(input.tbl)
save(comorbidity.table, file = "comorbidity_table.rda")

treatments.table<-treatments.prep(input.tbl)
save(treatments.table, file = "treatment_table.rda")

key.times.table<-key.times.prep(input.tbl)
save(key.times.table, file = "key_times.rda")

length.of.stay.sex.input <- length.of.stay.sex.prep(input.tbl)
save(length.of.stay.sex.input, file="length_of_stay_sex_input.rda")

length.of.stay.age.input <- length.of.stay.age.prep(input.tbl)
save(length.of.stay.age.input, file="length_of_stay_age_input.rda")

admission.to.icu.input <- admission.to.icu.prep(input.tbl)
save(admission.to.icu.input, file="admission_to_icu_input.rda")

length.of.stay.icu.input <- length.of.stay.icu.prep(input.tbl)
save(length.of.stay.icu.input, file='length_of_stay_icu_input.rda')

status.by.time.after.admission.input <- status.by.time.after.admission.prep(input.tbl)
save(status.by.time.after.admission.input, file="status_by_time_after_admission_input.rda")


#outcome by admision date
outcome_admission_date_input <- outcome.admission.date.prep(input.tbl) 
save(outcome_admission_date_input, file ="outcome_admission_date_input.rda")

#vital signs
data_plot_vs_resp <- func_plots_vs_resp(input.tbl)
data_plot_vs_hr <- func_plots_vs_hr(input.tbl)
data_plot_vs_temp <- func_plots_vs_temp(input.tbl)
data_plot_vs_sysbp <- func_plots_vs_sysbp(input.tbl)
data_plot_vs_oxysat <- func_plots_vs_oxysat(input.tbl)
data_plot_vs_oxysat_therapy <- func_plots_vs_oxysat_therapy(input.tbl)

save(data_plot_vs_resp, file ="data_plot_vs_resp.rda")
save(data_plot_vs_hr, file ="data_plot_vs_hr.rda")
save(data_plot_vs_temp, file ="data_plot_vs_temp.rda")
save(data_plot_vs_sysbp, file ="data_plot_vs_sysbp.rda")
save(data_plot_vs_oxysat, file ="data_plot_vs_oxysat.rda")
save(data_plot_vs_oxysat_therapy, file ="data_plot_vs_oxysat_therapy.rda")

#Lab data
data_plot_lab_crp <- func_plot_lab_crp(input.tbl)  
data_plot_lab_lym <- func_plot_lab_lym(input.tbl)
data_plot_lab_neut <- func_plot_lab_neut(input.tbl)
data_plot_lab_wbc <- func_plot_lab_crp(input.tbl)
data_plot_lab_urean <- func_plot_lab_urean(input.tbl)
data_plot_lab_pt <- func_plot_lab_pt(input.tbl)
data_plot_lab_alt <- func_plot_lab_alt(input.tbl)
data_plot_lab_aptt <- func_plot_lab_aptt(input.tbl)
data_plot_lab_bili <- func_plot_lab_bili(input.tbl)
data_plot_lab_ast <- func_plot_lab_ast(input.tbl)

save(data_plot_lab_crp, file ="data_plot_lab_crp.rda")
save(data_plot_lab_lym, file ="data_plot_lab_lym.rda")
save(data_plot_lab_neut, file ="data_plot_lab_neut.rda")
save(data_plot_lab_wbc, file ="data_plot_lab_wbc.rda")
save(data_plot_lab_urean, file ="data_plot_lab_urean.rda")
save(data_plot_lab_pt, file ="data_plot_lab_pt.rda")
save(data_plot_lab_alt, file ="data_plot_lab_alt.rda")
save(data_plot_lab_aptt, file ="data_plot_lab_aptt.rda")
save(data_plot_lab_bili, file ="data_plot_lab_bili.rda")
save(data_plot_lab_ast, file ="data_plot_lab_ast.rda")


#Comorbidity plots by age
data_plot_comorbid_asthma <- func_plot_comorbid_asthma(input.tbl)
data_plot_comorbid_malignant_neoplasm <-func_plot_comorbid_malignant_neoplasm(input.tbl)
data_plot_comorbid_obesity <- func_plot_comorbid_obesity(input.tbl)
data_plot_comorbid_diabetes <-func_plot_comorbid_diabetes(input.tbl)
data_plot_comorbid_dementia <-func_plot_comorbid_dementia(input.tbl)
data_plot_comorbid_smoking <-func_plot_comorbid_smoking(input.tbl)
data_plot_comorbid_hypertension <- func_plot_comorbid_hypertension(input.tbl)

save(data_plot_comorbid_asthma, file ="data_plot_comorbid_asthma.rda")
save(data_plot_comorbid_malignant_neoplasm, file ="data_plot_comorbid_malignant_neoplasm.rda")
save(data_plot_comorbid_obesity, file ="data_plot_comorbid_obesity.rda")
save(data_plot_comorbid_diabetes, file ="data_plot_comorbid_diabetes.rda")
save(data_plot_comorbid_dementia, file ="data_plot_comorbid_dementia.rda")
save(data_plot_comorbid_smoking, file ="data_plot_comorbid_smoking.rda")
save(data_plot_comorbid_hypertension, file ="data_plot_comorbid_hypertension.rda")


#Symptoms by age
data_plot_symptoms_history_of_fever <- func_plot_symptoms_history_of_fever(input.tbl)
data_plot_symptoms_cough <-func_plot_symptoms_cough(input.tbl)
data_plot_symptoms_cough_fever<-func_plot_symptoms_cough_fever(input.tbl)
data_plot_symptoms_shortness_of_breath<-func_plot_symptoms_shortness_of_breath(input.tbl)
data_plot_symptoms_cought_fever_shortness_of_breath<-func_plot_symptoms_cought_fever_shortness_of_breath(input.tbl)
data_plot_symptoms_upper_respiratory_tract_symptoms<-func_plot_symptoms_upper_respiratory_tract_symptoms(input.tbl)
data_plot_symptoms_altered_consciousness_confusion<-func_plot_symptoms_altered_consciousness_confusion(input.tbl)
data_plot_symptoms_constitutional<-func_plot_symptoms_constitutional(input.tbl)
data_plot_symptoms_vomiting_nausea<-func_plot_symptoms_vomiting_nausea(input.tbl)
data_plot_symptoms_diarrhoea <-func_plot_symptoms_diarrhoea(input.tbl)
data_plot_symptoms_abdominal_pain <- func_plot_symptoms_abdominal_pain(input.tbl)

save(data_plot_symptoms_history_of_fever, file ="data_plot_symptoms_history_of_fever.rda")
save(data_plot_symptoms_cough, file ="data_plot_symptoms_cough.rda")
save(data_plot_symptoms_cough_fever, file ="data_plot_symptoms_cough_fever.rda")
save(data_plot_symptoms_shortness_of_breath, file ="data_plot_symptoms_shortness_of_breath.rda")
save(data_plot_symptoms_cought_fever_shortness_of_breath, file ="data_plot_symptoms_cought_fever_shortness_of_breath.rda")
save(data_plot_symptoms_upper_respiratory_tract_symptoms, file ="data_plot_symptoms_upper_respiratory_tract_symptoms.rda")
save(data_plot_symptoms_altered_consciousness_confusion, file ="data_plot_symptoms_altered_consciousness_confusion.rda")
save(data_plot_symptoms_constitutional, file ="data_plot_symptoms_constitutional.rda")
save(data_plot_symptoms_vomiting_nausea, file ="data_plot_symptoms_vomiting_nausea.rda")
save(data_plot_symptoms_diarrhoea, file ="data_plot_symptoms_diarrhoea.rda")
save(data_plot_symptoms_abdominal_pain, file ="data_plot_symptoms_abdominal_pain.rda")

#Heatmap
data_plot_heatmap <- symptom.heatmap(data = input.tbl, admission.symptoms = admission.symptoms, asterisks = vector())

save(data_plot_heatmap, file ="data_plot_heatmap.rda")

####new plots by region

comorb_South_Asia_input=plot_by_region(input.tbl,"comorb","South_Asia")
comorb_Sub_Saharan_Africa_input=plot_by_region(input.tbl,"comorb","Sub-Saharan_Africa")
comorb_Europe_and_Central_Asia_input=plot_by_region(input.tbl,"comorb","Europe_and_Central_Asia")
comorb_Middle_East_and_North_Africa_input=plot_by_region(input.tbl,"comorb","Middle_East_and_North_Africa")
comorb_Latin_America_and_Caribbean_input=plot_by_region(input.tbl,"comorb","Latin_America_and_Caribbean")
comorb_East_Asia_and_Pacific_input=plot_by_region(input.tbl,"comorb","East_Asia_and_Pacific")
comorb_North_America_input=plot_by_region(input.tbl,"comorb","North_America")

save(comorb_South_Asia_input,file="comorb_South_Asia_input.rda")
save(comorb_Sub_Saharan_Africa_input,file="comorb_Sub_Saharan_Africa_input.rda")
save(comorb_Europe_and_Central_Asia_input,file="comorb_Europe_and_Central_Asia_input.rda")
save(comorb_Middle_East_and_North_Africa_input,file="comorb_Middle_East_and_North_Africa_input.rda")
save(comorb_Latin_America_and_Caribbean_input,file="comorb_Latin_America_and_Caribbean_input.rda")
save(comorb_East_Asia_and_Pacific_input,file="comorb_East_Asia_and_Pacific_input.rda")
save(comorb_North_America_input,file="comorb_North_America_input.rda")



symptoms_South_Asia_input=plot_by_region(input.tbl,"symptoms","South_Asia")
symptoms_Sub_Saharan_Africa_input=plot_by_region(input.tbl,"symptoms","Sub-Saharan_Africa")
symptoms_Europe_and_Central_Asia_input=plot_by_region(input.tbl,"symptoms","Europe_and_Central_Asia")
symptoms_Middle_East_and_North_Africa_input=plot_by_region(input.tbl,"symptoms","Middle_East_and_North_Africa")
symptoms_Latin_America_and_Caribbean_input=plot_by_region(input.tbl,"symptoms","Latin_America_and_Caribbean")
symptoms_East_Asia_and_Pacific_input=plot_by_region(input.tbl,"symptoms","East_Asia_and_Pacific")
symptoms_North_America_input=plot_by_region(input.tbl,"symptoms","North_America")

save(symptoms_South_Asia_input,file="symptoms_South_Asia_input.rda")
save(symptoms_Sub_Saharan_Africa_input,file="symptoms_Sub_Saharan_Africa_input.rda")
save(symptoms_Europe_and_Central_Asia_input,file="symptoms_Europe_and_Central_Asia_input.rda")
save(symptoms_Middle_East_and_North_Africa_input,file="symptoms_Middle_East_and_North_Africa_input.rda")
save(symptoms_Latin_America_and_Caribbean_input,file="symptoms_Latin_America_and_Caribbean_input.rda")
save(symptoms_East_Asia_and_Pacific_input,file="symptoms_East_Asia_and_Pacific_input.rda")
save(symptoms_North_America_input,file="symptoms_North_America_input.rda")
