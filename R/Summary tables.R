folder <- "C:/Users/marti/OneDrive/Documents/ISARIC/data/2020-11-09"
setwd(folder)
input.tbl<- read.csv("ISVARIC_dash_db_20201118_preprocess.csv")
dat <- readRDS("ISVARIC_dash_db_20201208_preprocess.rds")
input.tbl<-dat%>%
  as.data.frame()
memory.limit(size=70000)

#' Prepare Table1. Patient characteristics
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export patient.characteristic.table
#' 
patient.characteristic.prep <- function(input.tbl){
  
  
  size_cohort <- input.tbl %>%
    mutate(Description="Size of cohort")%>%
    tabyl(Description)%>%
    rename(value=n)%>%
    select(Description,value)
  
  by_sex<-input.tbl %>%
    mutate(Description=slider_sex)%>%
    mutate(Description=replace(Description,is.na(Description),"Unknown"))%>%
    tabyl(Description)%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    mutate(value=paste0(n, " (", percent,")"))%>%
    select(Description,value)
  
  by_outcome<-input.tbl%>%
    mutate(Description=slider_outcome)%>%
    mutate(Description=replace(Description,is.na(Description),"Unknown"))%>%
    tabyl(Description)%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    mutate(value=paste0(n, " (", percent,")"))%>%
    select(Description,value)

  
  by_age<-input.tbl%>%
    mutate(Description=as.character(slider_agegp10))%>%
    mutate(Description=replace(Description,is.na(Description),"Unknown"))%>%
    tabyl(Description)%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    mutate(value=paste0(n, " (", percent,")"))%>%
    select(Description,value)%>%
    select(Description,value)

  
  by_icu<-input.tbl%>%
    mutate(Description=slider_icu_ever)%>%
    mutate(Description=replace(Description,is.na(Description),"Unknown"))%>%
    tabyl(Description)%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    mutate(value=paste0(n, " (", percent,")"))%>%
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
  
  
  age <- input.tbl %>%
    select(slider_agegp10,slider_outcome)%>%
    mutate(slider_agegp10=as.character(slider_agegp10))%>%
    mutate(Variable=case_when(slider_agegp10=="90+" |
                                slider_agegp10=="80-89" |
                                slider_agegp10=="70-79" ~ "70+",
                              TRUE~slider_agegp10))%>%
    filter(!is.na(Variable))%>%
    tabyl(Variable,slider_outcome)%>%
    adorn_percentages("col")%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    adorn_ns(position ="front")%>%
    select("Ongoing care", Death, Discharge, LFTU)
  
    
  
  sex<-input.tbl %>%
    mutate(Variable=slider_sex)%>%
    filter(!is.na(Variable))%>%
    tabyl(Variable,slider_outcome)%>%
    adorn_percentages("col")%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    adorn_ns(position ="front")%>%
    select("Ongoing care", Death, Discharge, LFTU)
    
  out<-rbind(c('age','','','','',''),
             age,
             c('','','','','',''),
             c('sex','','','','',''),
             sex  )
  
  
}

outcome.age.sex.table<-outcome.age.sex.prep(input.tbl)
save(outcome.age.sex.table, file = "outcome.age.sex.table.rda")


#' Prepare Table2. symptoms
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export outcome.age.sex.table
#' 
symptoms.prep <- function(input.tbl){
  
  tot=nrow(input.tbl)
  
  data_vs_age <- select(prepr.tbl, c(starts_with("vs"),slider_agegp10)) %>%
    pivot_longer(starts_with("vs"), names_to = "vital_sign", values_to = "value")
  
  out<-select(input.tbl, c(starts_with("symptoms_"))) %>%
    pivot_longer(starts_with("symptoms_"), names_to = "Symptoms", values_to = "value")%>%
    mutate(value=case_when(is.na(value)~"Unknown",
                           value==FALSE~"Absent",
                           TRUE~"Present"))%>%
    mutate(count=1)%>%
    group_by(Symptoms,value)%>%
    mutate(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop2=paste0(n," (",prop, ")"))%>%
    #as.data.table() %>%
    select(Symptoms,value,prop2)%>%
    pivot_wider(id_cols = Symptoms, names_from = value,  values_from = prop2)
  
  
  %>%
    select(c(dataset,prop,variable))%>%
    spread(dataset,prop, fill=NA, convert=FALSE, drop=TRUE, sep=NULL)%>%
    arrange(variable)
    
    
    
    
    
    summarise(sum())%>%
    ungroup()%>%
    
    select(starts_with("symptoms_"))%>%
    as.data.table()%>%
    melt(id)%>%
    as.data.frame()%>%
    mutate(value = ifelse(is.na(value), 0,1))%>%
    mutate(denom=1)
  tot<-q_symptoms%>%
    group_by(variable)%>%
    summarise(known= sum(value, na.rm=T),denom=sum(denom, na.rm=T))%>%
    mutate(dataset="Total")
  q_symptoms<-q_symptoms%>%
    group_by(dataset,variable)%>%
    summarise(known= sum(value, na.rm=T),denom=sum(denom, na.rm=T))%>%
    bind_rows(tot)%>%
    mutate(prop=round(known/denom*100, digits=1))%>%
    mutate(prop=paste0(known," (",prop, "%", ")"))%>%
    select(c(dataset,prop,variable))%>%
    spread(dataset,prop, fill=NA, convert=FALSE, drop=TRUE, sep=NULL)%>%
    arrange(variable)
  
  out <- input.tbl %>%
    select(slider_agegp10,slider_outcome)%>%
    mutate(slider_agegp10=as.character(slider_agegp10))%>%
    mutate(Variable=case_when(slider_agegp10=="90+" |
                                slider_agegp10=="80-89" |
                                slider_agegp10=="70-79" ~ "70+",
                              TRUE~slider_agegp10))%>%
    filter(!is.na(Variable))%>%
    tabyl(Variable,slider_outcome)%>%
    adorn_percentages("col")%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    adorn_ns(position ="front")%>%
    select("Ongoing care", Death, Discharge, LFTU)
  
  
  
  sex<-input.tbl %>%
    mutate(Variable=slider_sex)%>%
    filter(!is.na(Variable))%>%
    tabyl(Variable,slider_outcome)%>%
    adorn_percentages("col")%>%
    adorn_pct_formatting(rounding = "half up", digits = 0, affix_sign = FALSE) %>%
    adorn_ns(position ="front")%>%
    select("Ongoing care", Death, Discharge, LFTU)
  
  out<-rbind(c('age','','','','',''),
             age,
             c('','','','','',''),
             c('sex','','','','',''),
             sex  )
  
  
}

outcome.age.sex.table<-outcome.age.sex.prep(input.tbl)
save(outcome.age.sex.table, file = "outcome.age.sex.table.rda")







character<-flextable (patient.characteristic.table)
character<-align(character,align = "center")
character<-align(character,align = "center", part="header")
ft_ov<-align(ft_ov,j=1, align="right")
ft_ov<-bold(ft_ov,part="header")
ft_ov<-bold(ft_ov,j=1,i=1:4,part="body")
ft_ov<-bold(ft_ov,i=c(1,4),part="body")
ft_ov 
  