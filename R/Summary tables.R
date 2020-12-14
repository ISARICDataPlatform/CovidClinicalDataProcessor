folder <- "C:/Users/marti/OneDrive/Documents/ISARIC/data/2020-11-09"
setwd(folder)
input.tbl<- read.csv("ISVARIC_dash_db_20201118_preprocess.csv")
dat <- readRDS("ISVARIC_dash_db_20201208_preprocess.rds")
input.tbl<-prepr.tbl%>%
  as.data.frame()
memory.limit(size=50000)

#' Prepare Table1. Patient characteristics
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export patient.characteristic.table
#' 
patient.characteristic.prep <- function(input.tbl){

  tot=nrow(input.tbl)  
  
  size_cohort <- input.tbl %>%
    mutate(Description="Size of cohort")%>%
    tabyl(Description)%>%
    rename(value=n)%>%
    select(Description,value)
  
  by_sex<-input.tbl %>%
    mutate(Description=slider_sex)%>%
    mutate(Description=replace(Description,is.na(Description)|Description=="","Unknown"))%>%
    mutate(count=1)%>%
    group_by(Description)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(value=round(n/tot,digit=2))%>%
    mutate(value=paste0(n," (",value, ")"))%>%
    select(Description,value)
  
  by_outcome<-input.tbl%>%
    mutate(Description=slider_outcome)%>%
    mutate(Description=replace(Description,is.na(Description),"Unknown"))%>%
    mutate(count=1)%>%
    group_by(Description)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(value=round(n/tot,digit=2))%>%
    mutate(value=paste0(n," (",value, ")"))%>%
    select(Description,value)

  
  by_age<-input.tbl%>%
    mutate(Description=as.character(slider_agegp10))%>%
    mutate(Description=case_when(Description=="90+" |
                                   Description=="80-89" |
                                   Description=="70-79" ~ "70+",
                                is.na(Description)~'Unknown',
                              TRUE~Description))%>%
    mutate(Description=replace(Description,is.na(Description),"Unknown"))%>%
    mutate(count=1)%>%
    group_by(Description)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(value=round(n/tot,digit=2))%>%
    mutate(value=paste0(n," (",value, ")"))%>%
    select(Description,value)

  
  by_icu<-input.tbl%>%
    mutate(Description=slider_icu_ever)%>%
    mutate(Description=case_when(is.na(Description)~"Unknown",
                                 Description==FALSE~"Absent",
                                  TRUE~"Present"))%>%
    mutate(count=1)%>%
    group_by(Description)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(value=round(n/tot,digit=2))%>%
    mutate(value=paste0(n," (",value, ")"))%>%
    select(Description,value)

 
out<-rbind(size_cohort,c('',''),c('By sex',''),by_sex,c('',''),c('By outcome status',''),by_outcome,c('',''),
                    c('By age group',''), by_age,c('',''),c('Admitted to ICU/HDU?',''),by_icu  )  
  
}

patient.characteristic.table<-patient.characteristic.prep(input.tbl)
save(patient.characteristic.table, file = "patient.characteristic.table.rda")


#' Prepare Table2. Outcome by age and sex
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export outcome.age.sex.table
#' 
outcome.age.sex.prep <- function(input.tbl){
  
  Variable<-c(
    'age',
    '0-9',
    '10-19',
    '20-29',
    '30-39',
    '40-49',
    '50-59',
    '60-69',
    '70+' ,
    '',
    'Sex',
    'Female',
    'Male')
  Variable<-data.frame(Variable)
  
  sex<-input.tbl %>%
    mutate(Variable=slider_sex)%>%
    filter(!(is.na(Variable)| Variable=="")) %>%
    mutate(count=1)%>%
    group_by(slider_outcome)%>%
    mutate(tot = sum(count)) %>%
    ungroup()%>%
    group_by(Variable,slider_outcome, tot)%>%
    group_by(Variable,slider_outcome,tot)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = Variable, names_from = slider_outcome,  values_from = prop)%>%
    select("Ongoing care", Death, Discharge, LFTU)%>%
    #select(Variable, Death, Discharge, LFTU)%>%
    ungroup()
  
  
  age <- input.tbl %>%
    select(slider_agegp10,slider_outcome)%>%
    mutate(slider_agegp10=as.character(slider_agegp10))%>%
    mutate(Variable=case_when(slider_agegp10=="90+" |
                                slider_agegp10=="80-89" |
                                slider_agegp10=="70-79" ~ "70+",
                              TRUE~slider_agegp10))%>%
    filter(!is.na(Variable))%>%
    mutate(count=1)%>%
    group_by(slider_outcome)%>%
    mutate(tot = sum(count)) %>%
    ungroup()%>%
    group_by(Variable,slider_outcome, tot)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = Variable, names_from = slider_outcome,  values_from = prop)%>%
    select("Ongoing care", Death, Discharge, LFTU)
    #select(Variable, Death, Discharge, LFTU)

    
 out<-rbind(age,sex)%>%
   full_join(Variable)%>%
   arrange(factor(Variable, levels=c('age',
                                     '0-9',
                                     '10-19',
                                     '20-29',
                                     '30-39',
                                     '40-49',
                                     '50-59',
                                     '60-69',
                                     '70+' ,
                                     '',
                                     'Sex',
                                     'Female',
                                     'Male')))
 
}

outcome.age.sex.table<-outcome.age.sex.prep(input.tbl)
save(outcome.age.sex.table, file = "outcome.age.sex.table.rda")


#' Prepare Table3. symptoms
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export outcome.age.sex.table
#' 
symptoms.prep <- function(input.tbl){
  
  tot=nrow(input.tbl)
  
  data<-select(input.tbl, c(starts_with("symptoms_"))) %>%
    pivot_longer(starts_with("symptoms_"), names_to = "symptom", values_to = "value")
  out<-data%>%
    mutate(value=case_when(is.na(value)~"Unknown",
                           value==FALSE~"Absent",
                           TRUE~"Present"))%>%
    mutate(count=1)%>%
    group_by(symptom,value)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = symptom, names_from = value,  values_from = prop)%>%
    select(symptom, Present, Absent, Unknown)%>%
    ungroup()
  
  data<-data%>%filter(value==TRUE)%>%tabyl(symptom)%>%select(-c(percent))
  nice.symptom.mapper <- tibble(symptom = unique(data$symptom)) %>%
    mutate(nice.symptom = map_chr(symptom, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    })) %>%
    mutate(nice.symptom = case_when(nice.symptom=="Altered consciousness confusion" ~ "Altered consciousness/confusion",
                                    nice.symptom=="Cough bloody sputum haemoptysis" ~ "Cough with bloody sputum/haemoptysis",
                                    nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
                                    TRUE ~ nice.symptom))%>%
    left_join(data)
    
  out<-out%>%
    #lazy_dt(immutable = TRUE) %>%
    left_join(nice.symptom.mapper) %>%
    rename(Symptoms=nice.symptom)%>%
    arrange(desc(n))%>%
    select(Symptoms,Present, Absent, Unknown)%>%
    as_tibble() 
    
 }

symptoms.table<-symptoms.prep(input.tbl)
save(symptoms.table, file = "symptoms.table.rda")

#' Prepare Table4. comorbidities
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export comorbidity.table
#' 
comorbidity.prep <- function(input.tbl){
  
  tot=nrow(input.tbl)
  data<-select(input.tbl, c(starts_with("comorbid_"))) %>%
    pivot_longer(starts_with("comorbid_"), names_to = "comorbidity", values_to = "value")
  
  out<-data%>%
    mutate(value=case_when(is.na(value)~"Unknown",
                           value==FALSE~"Absent",
                           TRUE~"Present"))%>%
    mutate(count=1)%>%
    group_by(comorbidity,value)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = comorbidity, names_from = value,  values_from = prop)%>%
    ungroup()
  
  data<-data%>%filter(value==TRUE)%>%tabyl(comorbidity)%>%select(-c(percent))
  nice.comorbidity.mapper <- tibble(comorbidity = unique(out$comorbidity)) %>%
    mutate(nice.comorbidity = map_chr(comorbidity, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      #temp2
    })) %>%
    mutate(nice.comorbidity = case_when(nice.comorbidity=="Aids hiv" ~ "HIV/AIDS",
                                        nice.comorbidity=="Chronic including congenital cardiac disease" ~ "Chronic cardiac disease",
                                        TRUE ~ nice.comorbidity))%>%
    left_join(data)%>%
    as.data.frame()
    
  
  out2<-out %>%
    #lazy_dt(immutable = TRUE) %>%
    left_join(nice.comorbidity.mapper) %>%
    arrange(desc(n))%>%
    #rename(Comorbidities=comorbidity)%>%
    select("Comorbidities"=nice.comorbidity,Present, Absent, Unknown)%>%
    as_tibble() 
  
}

comorbidity.table<-comorbidity.prep(input.tbl)
save(comorbidity.table, file = "comorbidity.table.rda")


#' Prepare Table5. Prevalence of treatments
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export treatments.table
#' 
treatments.prep <- function(input.tbl){
  
  tot=nrow(input.tbl)
  
  data<-select(input.tbl, c(starts_with("treat_"))) %>%
    pivot_longer(starts_with("treat_"), names_to = "treatment", values_to = "value")
  out<-data%>%
    mutate(value=case_when(is.na(value)~"Unknown",
                           value==FALSE~"Absent",
                           TRUE~"Present"))%>%
    mutate(count=1)%>%
    group_by(treatment,value)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = treatment, names_from = value,  values_from = prop)%>%
    ungroup()
  data<-data%>%filter(value==TRUE)%>%tabyl(treatment)%>%select(-c(percent))
  nice.treatment.mapper <- tibble(treatment = unique(out$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))%>%
    left_join(data)
  
  out %>%
    #lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    arrange(desc(n))%>%
    select("Treatments"=nice.treatment,Present, Absent, Unknown)%>%
    #rename(Treatments=treatment)%>%
    as_tibble() 
  
}

treatments.table<-treatments.prep(input.tbl)
save(treatments.table, file = "treatment.table.rda")


#' Prepare Table6. key times variable
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export key.times.table
#' 
key.times.prep <- function(input.tbl){
  
  key_time<-c(
    'dur_ho',
    't_son_ad',
    '',
    't_ad_icu',
    'dur_icu',
    ' ',
    't_ad_imv',
    'dur_imv',
    '   ',
    't_ad_niv',
    'dur_niv')
  key_time<-data.frame(key_time)

  data<-select(input.tbl, c(starts_with("t_"))) %>%
    pivot_longer(c(starts_with("t_")), names_to = "key_time", values_to = "value")
  
  out<-select(input.tbl, c(starts_with("dur_"))) %>%
    pivot_longer(c(starts_with("dur_")), names_to = "key_time", values_to = "value")%>%
    rbind(data)%>%
    filter(!(is.na(value)|value>330|value<0))%>%
    group_by(key_time)%>%
    summarise(mean=mean(value,na.rm=T),
              sd=sd(value,na.rm=T),
              median=median(value,na.rm=T),
              iqr=IQR(value,na.rm=T))%>%
    mutate(mean=round(mean,digit=1))%>%
    mutate(sd=round(sd,digit=1))%>%
    full_join(key_time)%>%
    arrange(factor(key_time, levels=c('dur_ho',
                                       't_son_ad',
                                       '',
                                       't_ad_icu',
                                       'dur_icu',
                                       ' ',
                                       't_ad_imv',
                                       'dur_imv',
                                       '   ',
                                       't_ad_niv',
                                       'dur_niv')))%>%
    mutate(key_time=case_when(key_time=='dur_ho'~'Length of hospital stay',
                              key_time=='t_son_ad'~'Symptom onset to admission',
                              key_time=='t_ad_icu'~'Admission to ICU entry',
                              key_time=='dur_icu'~'Duration of ICU',
                              key_time=='t_ad_imv'~'Admission to IMV',
                              key_time=='dur_imv'~'Duration of IMV',
                              key_time=='t_ad_niv'~'Admission to NIV',
                              key_time=='dur_niv'~'Duration of NIV'))%>%
    rename("Time (in days)"=key_time)%>%
    rename("Mean (observed)"=mean)%>%
    rename("SD (observed)"=sd)%>%
    rename("Median (observed)"=median)%>%
    rename("IQR (observed)"=iqr)

}

key.times.table<-key.times.prep(input.tbl)
save(key.times.table, file = "key.times.rda")





  