
#' Aggregate data for case enrolment over time by site
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr tidyr
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @return A \code{tibble} containing the input data for the comorbidity prevalence plot
#' @export patient.enrolment.site.time.map.prep


patient.site.time.map.prep <- function(input.tbl){
  
  patient.site.time.map.input   <- input.tbl %>%
    tabyl(date_start,siteid_final)
  
}

patient.site.time.map.input<-patient.site.time.map.prep(input.tbl)
save(patient.site.time.map.input, file = "patient.site.time.map.input.rda")

#' Aggregate data for comorbidity prevalence plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr tidyr
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @return A \code{tibble} containing the input data for the comorbidity prevalence plot
#' @export comorbidity.prevalence.prep


comorbidity.prevalence.prep <- function(input.tbl){
  
  comorbidity.prevalence.input <- input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, slider_icu_ever, any_of(starts_with("comorb")), lower.age.bound, upper.age.bound) %>%
    as.data.table() %>%
    pivot_longer(any_of(starts_with("comorb")), names_to = "comorbidity", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, comorbidity, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
    summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present)))%>%
    as.data.frame()
   
  
  
  nice.comorbidity.mapper <- tibble(comorbidity = unique(comorbidity.prevalence.input$comorbidity)) %>%
    mutate(nice.comorbidity = map_chr(comorbidity, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      #temp2
    })) %>%
    mutate(nice.comorbidity = case_when(nice.comorbidity=="Aids hiv" ~ "HIV/AIDS",
                                        TRUE ~ nice.comorbidity))%>%
    as.data.frame()
  
  comorbidity.prevalence.input %>%
    lazy_dt(immutable = TRUE) %>%
    full_join(nice.comorbidity.mapper) %>%
    as_tibble()
    
}
comorbidity.prevalence.input<-comorbidity.prevalence.prep(input.tbl)
save(comorbidity.prevalence.input, file = "comorbidity.prevalence.input.rda")


#' Aggregate data for comorbidities upset plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @param max.comorbidities The plot will display only the n most common comorbidities, this parameter is n
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the comorbidities upset plot
#' @export comorbidity.upset.prep
comorbidity.upset.prep <- function(input.tbl, max.comorbidities = 5){
  # (max.comorbidities is the n to list; this will be the n most frequent)
  # just the comorbidity columns
  
  data2 <- input.tbl %>%
    select(usubjid, starts_with("comorb"))
  
  n.comorb <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.comorb+1), names_to = "Condition", values_to = "Present") %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
      }
    })) 
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Condition) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    ungroup() %>%
    filter(Condition != "other_mhyn") %>%
    arrange(desc(Present)) %>%
    slice(1:max.comorbidities) %>%
    pull(Condition)
  
  
  nice.comorbidity.mapper <- tibble(comorbidity = unique(most.common)) %>%
    mutate(nice.comorbidity = map_chr(comorbidity, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    })) %>%
    mutate(nice.comorbidity = case_when(comorbidity=="Aids hiv" ~ "HIV/AIDS",
                                        TRUE ~ nice.comorbidity))
  
  
  top.n.conditions.tbl <- input.tbl %>%
    dplyr::select(usubjid, matches(most.common)) %>%
    pivot_longer(2:(length(most.common)+1), names_to = "Condition", values_to = "Present") %>%
    left_join(nice.comorbidity.mapper, by=c("Condition" = "comorbidity")) %>%
    select(-Condition) %>%
    filter(!is.na(Present)) %>%
    group_by(usubjid) %>%
    dplyr::summarise(Conditions = list(nice.comorbidity), Presence = list(Present)) %>%
    dplyr::mutate(conditions.present = map2(Conditions, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Conditions, -Presence)
  
  slider.join <- input.tbl %>% select(usubjid, starts_with("slider"), lower.age.bound, upper.age.bound)
  
  top.n.conditions.tbl <- top.n.conditions.tbl %>% left_join(slider.join)
  
  comorbidity.upset.input <- top.n.conditions.tbl %>% 
    mutate(condstring = map_chr(conditions.present, function(cp){
      paste(sort(cp), collapse = "-")
    })) %>%
    select(-conditions.present) %>%
    group_by(condstring, 
             slider_sex, 
             slider_country,
             slider_icu_ever,
             slider_outcome,
             slider_monthyear,
             slider_agegp10,
             lower.age.bound,
             upper.age.bound) %>% 
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(which.present = map(condstring, function(x){
      out <- str_split(x, "-")
      if(out == ""){
        character()
      } else {
        unlist(out)
      }
    })) %>%
    select(-condstring)
  
  comorbidity.upset.input
  
}

comorbidity.upset.input<-comorbidity.upset.prep(input.tbl)
save(comorbidity.upset.input, file = "comorbidity.upset.input.rda")



#' Aggregate data for treatment use proportion plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr tidyr
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @return A \code{tibble} containing the input data for the treatment use proportion plot
#' @export treatment.use.proportion.prep
treatment.use.proportion.prep <- function(input.tbl){
  
  treatment.use.proportion.input <- input.tbl %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, slider_icu_ever, any_of(starts_with("treat")), lower.age.bound, upper.age.bound) %>%
    as.data.table() %>%
    pivot_longer(any_of(starts_with("treat")), names_to = "treatment", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, treatment, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
    summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present))) %>%
    as_tibble()
  
  nice.treatment.mapper <- tibble(treatment = unique(treatment.use.proportion.input$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))
  
  treatment.use.proportion.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    as_tibble()
}


treatment.use.proportion.input<-treatment.use.proportion.prep(input.tbl)
save(treatment.use.proportion.input, file = "treatment.use.proportion.input.rda")








#' Aggregate data for treatments upset plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @param max.treatments The plot will display only the n most common treatments, this parameter is n
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the treatments upset plot
#' @export treatment.upset.prep
treatment.upset.prep <- function(input.tbl, max.treatments = 5){
  
  
  data2 <- input.tbl %>%
    select(usubjid, starts_with("treat"))
  
  n.treat <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.treat+1), names_to = "Treatment", values_to = "Present") %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
      }
    })) 
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Treatment) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    ungroup() %>%
    arrange(desc(Present)) %>%
    slice(1:max.treatments) %>%
    pull(Treatment)
  
  
  nice.treatment.mapper <- tibble(treatment = unique(most.common)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))
  
  
  top.n.treatments.tbl <- input.tbl %>%
    dplyr::select(usubjid,starts_with("treat"))%>%
    dplyr::select(usubjid, matches(most.common)) %>%
    pivot_longer(2:(length(most.common)+1), names_to = "Treatment", values_to = "Present") %>%
    left_join(nice.treatment.mapper, by=c("Treatment" = "treatment")) %>%
    select(-Treatment) %>%
    filter(!is.na(Present)) %>%
    group_by(usubjid) %>%
    dplyr::summarise(Treatments = list(nice.treatment), Presence = list(Present)) %>%
    dplyr::mutate(treatments.present = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence)
  
  slider.join <- input.tbl %>% select(usubjid, starts_with("slider"), lower.age.bound, upper.age.bound)
  
  top.n.treatments.tbl <- top.n.treatments.tbl %>% left_join(slider.join)
  
  treatment.upset.input <- top.n.treatments.tbl %>% 
    mutate(condstring = map_chr(treatments.present, function(cp){
      paste(sort(cp), collapse = "-")
    })) %>%
    select(-treatments.present) %>%
    group_by(condstring, 
             slider_sex, 
             slider_country,
             slider_icu_ever,
             slider_outcome,
             slider_monthyear,
             slider_agegp10,
             lower.age.bound,
             upper.age.bound) %>% 
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(which.present = map(condstring, function(x){
      out <- str_split(x, "-")
      if(out == ""){
        character()
      } else {
        unlist(out)
      }
    })) %>%
    select(-condstring)
  
  treatment.upset.input
  
}



treatment.upset.input<-treatment.upset.prep(input.tbl)
save(treatment.upset.input, file = "treatment.upset.input.rda")



#' Aggregate data for treatment ICU use proportion plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr tidyr
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @return A \code{tibble} containing the input data for the treatment use proportion plot
#' @export treatment.use.proportion.prep



icu.treatment.use.proportion.prep <- function(input.tbl){
  
  
  icu.treatment.use.proportion.input <- input.tbl %>%
      select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, slider_icu_ever, any_of(starts_with("icu_treat")), lower.age.bound, upper.age.bound) %>%
      as.data.table() %>%
      pivot_longer(any_of(starts_with("icu_treat")), names_to = "treatment", values_to = "present") %>%
      lazy_dt(immutable = TRUE) %>%
      group_by(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, treatment, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
      summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present))) %>%
      as_tibble()
    
  
  nice.treatment.mapper <- tibble(treatment = unique(icu.treatment.use.proportion.input$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))
  
  icu.treatment.use.proportion.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    as_tibble()
}

icu.treatment.use.proportion.input<-icu.treatment.use.proportion.prep(input.tbl)
save(icu.treatment.use.proportion.input, file = "icu.treatment.use.proportion.input.rda")




#' Aggregate data for ICU treatments upset plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @param max.treatments The plot will display only the n most common treatments, this parameter is n
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the treatments upset plot
#' @export treatment.upset.prep
treatment.icu.upset.prep <- function(input.tbl, max.treatments = 5){
  
  
  data2 <- input.tbl %>%
    select(usubjid, starts_with("icu_treat"))
  
  n.treat <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.treat+1), names_to = "Treatment", values_to = "Present") %>%
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
      }
    })) 
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Treatment) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    ungroup() %>%
    arrange(desc(Present)) %>%
    slice(1:max.treatments) %>%
    pull(Treatment)
  
  
  nice.treatment.mapper <- tibble(treatment = unique(most.common)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))
  
  
  top.n.treatments.tbl <- input.tbl %>%
    dplyr::select(usubjid,starts_with("icu_treat"))%>%
    dplyr::select(usubjid, matches(most.common)) %>%
    pivot_longer(2:(length(most.common)+1), names_to = "Treatment", values_to = "Present") %>%
    left_join(nice.treatment.mapper, by=c("Treatment" = "treatment")) %>%
    select(-Treatment) %>%
    filter(!is.na(Present)) %>%
    group_by(usubjid) %>%
    dplyr::summarise(Treatments = list(nice.treatment), Presence = list(Present)) %>%
    dplyr::mutate(treatments.present = map2(Treatments, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Treatments, -Presence)
  
  slider.join <- input.tbl %>% select(usubjid, starts_with("slider"), lower.age.bound, upper.age.bound)
  
  top.n.treatments.tbl <- top.n.treatments.tbl %>% left_join(slider.join)
  
  treatment.upset.input <- top.n.treatments.tbl %>% 
    mutate(condstring = map_chr(treatments.present, function(cp){
      paste(sort(cp), collapse = "-")
    })) %>%
    select(-treatments.present) %>%
    group_by(condstring, 
             slider_sex, 
             slider_country,
             slider_icu_ever,
             slider_outcome,
             slider_monthyear,
             slider_agegp10,
             lower.age.bound,
             upper.age.bound) %>% 
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(which.present = map(condstring, function(x){
      out <- str_split(x, "-")
      if(out == ""){
        character()
      } else {
        unlist(out)
      }
    })) %>%
    select(-condstring)
  
  treatment.upset.input
  
}

icu.treatment.upset.input<-treatment.icu.upset.prep(input.tbl)
save(icu.treatment.upset.input, file = "icu.treatment.upset.input.rda")










#' Aggregate data for symptom prevalence plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr tidyr tidyfast
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @return A \code{tibble} containing the input data for the symptom prevalence plot
#' @export symptom.prevalence.prep
symptom.prevalence.prep <- function(input.tbl){
  
  symptom.prevalence.input <- input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, slider_icu_ever, any_of(starts_with("symptoms")), lower.age.bound, upper.age.bound) %>%
    as.data.table() %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptom", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, symptom, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
    summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present))) %>%
    as_tibble()
  
  nice.symptom.mapper <- tibble(symptom = unique(symptom.prevalence.input$symptom)) %>%
    mutate(nice.symptom = map_chr(symptom, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    })) %>%
    mutate(nice.symptom = case_when(nice.symptom=="Altered consciousness confusion" ~ "Altered consciousness/confusion",
                                    nice.symptom=="Cough" ~ "Cough (no sputum)",
                                    nice.symptom=="Cough bloody sputum haemoptysis" ~ "Cough with bloody sputum/haemoptysis",
                                    nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
                                    TRUE ~ nice.symptom))
  
  symptom.prevalence.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.symptom.mapper) %>%
    as_tibble() 
}
symptom.prevalence.input<-symptom.prevalence.prep(input.tbl)
save(symptom.prevalence.input, file = "symptom.prevalence.input.rda")



#' Aggregate data for symptoms upset plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @param max.symptoms The plot will display only the n most common symptoms, this parameter is n
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the symptoms upset plot
#' @export symptom.upset.prep
symptom.upset.prep <- function(input.tbl, max.symptoms = 5){
  
  
  data2 <- input.tbl %>%
    select(usubjid, starts_with("symp"))
  
  n.symp <- ncol(data2) - 1 #changed here
  
  data2 <- data2 %>%
    pivot_longer(2:(n.symp+1), names_to = "Condition", values_to = "Present") %>%#changed to symp
    dplyr::mutate(Present = map_lgl(Present, function(x){
      if(is.na(x)){
        FALSE
      } else if(x == 1){
        TRUE
      } else if(x == 2){
        FALSE
      } else {
        FALSE
      }
    })) 
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Condition) %>%
    dplyr::summarise(Total = n(), Present = sum(Present, na.rm = T)) %>%
    ungroup() %>%
    filter(Condition != "symptoms_other_signs_and_symptoms") %>%
    arrange(desc(Present)) %>%
    #slice(1:max.symptoms) %>%
    slice(1:5) %>%
    pull(Condition)
  
  
  nice.symptom.mapper <- tibble(symptom = unique(most.common)) %>%
    mutate(nice.symptom = map_chr(symptom, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    })) %>%
    mutate(nice.symptom = case_when(nice.symptom=="Altered consciousness confusion" ~ "Altered consciousness/confusion",
                                    #nice.symptom=="Cough" ~ "Cough (no sputum)",
                                    #nice.symptom=="Cough bloody sputum haemoptysis" ~ "Cough with bloody sputum/haemoptysis",
                                    nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
                                    TRUE ~ nice.symptom))
  
  
  top.n.conditions.tbl <- input.tbl %>%
    dplyr::select(usubjid, matches(most.common)) %>%
    pivot_longer(2:(length(most.common)+1), names_to = "Condition", values_to = "Present") %>%
    left_join(nice.symptom.mapper, by=c("Condition" = "symptom")) %>%
    filter(!is.na(nice.symptom))%>%
    select(-Condition) %>%
    filter(!is.na(Present)) %>%
    group_by(usubjid) %>%
    dplyr::summarise(Conditions = list(nice.symptom), Presence = list(Present)) %>%
    dplyr::mutate(conditions.present = map2(Conditions, Presence, function(c,p){
      c[which(p)]
    })) %>%
    dplyr::select(-Conditions, -Presence)
  
  slider.join <- input.tbl %>% select(usubjid, starts_with("slider"), lower.age.bound, upper.age.bound)
  
  top.n.conditions.tbl <- top.n.conditions.tbl %>% left_join(slider.join)
  
  symptom.upset.input <- top.n.conditions.tbl %>% 
    mutate(condstring = map_chr(conditions.present, function(cp){
      paste(sort(cp), collapse = "-")
    })) %>%
    select(-conditions.present) %>%
    group_by(condstring, 
             slider_sex, 
             slider_country,
             slider_icu_ever,
             slider_outcome,
             slider_monthyear,
             slider_agegp10,
             lower.age.bound,
             upper.age.bound) %>% 
    summarise(count = n()) %>%
    ungroup() %>%
    mutate(which.present = map(condstring, function(x){
      out <- str_split(x, "-")
      if(out == ""){
        character()
      } else {
        unlist(out)
      }
    })) %>%
    select(-condstring)
  
  
  
}

symptom.upset.input<-symptom.upset.prep(input.tbl)
save(symptom.upset.input, file = "symptom.upset.input.rda")

#' Data for the report summary
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the symptoms upset plot
#' @export summary.input.prep

summary.input.prep<- function(input.tbl){
  input.tbl%>%
    select(c(siteid_final,
             starts_with("slider_"),
             age,
             date_admit,
             dur_ho,
             dur_icu,
             t_ad_icu,
             t_son_ad,
             outcome,
             slider_outcome,
             slider_icu_ever,
             treat_high_flow_nasal_canula_oxygen_therapy,
             treat_nasal_mask_oxygen_therapy,
             treat_non_invasive_ventilation,
             treat_invasive_ventilation,
             treat_antibiotic_agents,
             treat_antiviral_agents,
             treat_corticosteroids,
             vs_oxysat,
             icu_treat_antibiotic_agents,
             icu_treat_antiviral_agents,
             icu_treat_non_invasive_ventilation,
             icu_treat_invasive_ventilation,
             icu_treat_nasal_mask_oxygen_therapy,
             icu_treat_high_flow_nasal_canula_oxygen_therapy,
             t_ad_niv,
             t_ad_imv,
             dur_niv,
             dur_imv,
             ))%>%
      mutate(oxygen_therapy=NA)%>%
      mutate(oxygen_therapy=case_when(
                    treat_high_flow_nasal_canula_oxygen_therapy==FALSE|
                    treat_nasal_mask_oxygen_therapy==FALSE|
                    treat_non_invasive_ventilation==FALSE|
                    treat_invasive_ventilation==FALSE~FALSE,
                    treat_high_flow_nasal_canula_oxygen_therapy==TRUE|
                    treat_nasal_mask_oxygen_therapy==TRUE|
                    treat_non_invasive_ventilation==TRUE|
                    treat_invasive_ventilation==TRUE~TRUE,
                    TRUE~oxygen_therapy))%>%
      mutate(icu_oxygen_therapy=NA)%>%
      mutate(icu_oxygen_therapy=case_when(
              icu_treat_high_flow_nasal_canula_oxygen_therapy==FALSE|
              icu_treat_nasal_mask_oxygen_therapy==FALSE|
              icu_treat_non_invasive_ventilation==FALSE|
              icu_treat_invasive_ventilation==FALSE~FALSE,
              icu_treat_high_flow_nasal_canula_oxygen_therapy==TRUE|
              icu_treat_nasal_mask_oxygen_therapy==TRUE|
              icu_treat_non_invasive_ventilation==TRUE|
              icu_treat_invasive_ventilation==TRUE~TRUE,
              TRUE~icu_oxygen_therapy))
}
summary_input<-summary.input.prep(input.tbl)
save(summary_input, file = "summary_input.rda")
        
#' Aggregate data for hospital stay plot by sex
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export length.of.stay.sex.prep
length.of.stay.sex.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    mutate(length.of.stay=dur_ho) %>% 
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever, length.of.stay) %>%
    mutate(sex=slider_sex) %>% 
    mutate(sex=factor(sex,levels = c("Male", "Female")))  %>%  
    filter(!is.na(length.of.stay)) %>% 
    filter(!is.na(sex)) %>% 
    filter(length.of.stay > 0) %>% 
    as_tibble() 
}
length_of_stay_sex_input<- length.of.stay.sex.prep(input.tbl)
save(length_of_stay_sex_input, file = "length_of_stay_sex_input.rda")

#' Aggregate data for hospital stay plot by age 
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export length.of.stay.age.prep
length.of.stay.age.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    mutate(length.of.stay=dur_ho) %>% 
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever, length.of.stay) %>%
    mutate(agegp10=as.character(slider_agegp10)) %>% 
    mutate(agegp10=ifelse(agegp10 %in% c("70-79","80-89","90+"), "70+", agegp10)) %>% 
    filter(!is.na(length.of.stay)) %>% 
    filter(!is.na(agegp10)) %>% 
    filter(length.of.stay > 0) %>% 
    as_tibble() 
}
length_of_stay_age_input<- length.of.stay.age.prep(input.tbl)
save(length_of_stay_age_input, file = "length_of_stay_age_input.rda")

#' Aggregate data for hospital admission to ICU admission
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export admission.to.icu
admission.to.icu.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    mutate(admission.to.icu=t_ad_icu) %>% 
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever, admission.to.icu) %>%
    filter(!is.na(admission.to.icu)) %>% 
    filter(admission.to.icu >= 0) %>% 
    as_tibble() 
}
admission_to_icu_input<-admission.to.icu.prep (input.tbl)
save(admission_to_icu_input, file = "admission_to_icu_input.rda")

#' Aggregate data for timeline plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export status.by.time.after.admission
status.by.time.after.admission.prep <- function(input.tbl){
  
  timings.wrangle <- input.tbl %>% 
    filter(!is.na(date_admit)) %>% 
    select(usubjid, date_admit, icu_in, icu_out, dur_icu, dur_ho, t_ad_icu, date_outcome, date_last, slider_outcome) %>% 
    mutate(subjid=usubjid,
           final.status=slider_outcome,
           hospital.start = 0,
           hospital.end=ho_dur,
           ICU.start = t_ad_icu,
           ICU.end= t_ad_icu+icu_dur,
           censored.date=date_last-date_admit,
           censored=ifelse(slider_outcome == "censored",T,F)) %>% 
    filter(hospital.end >= 0 | is.na(hospital.end))  %>%
    mutate(ever.ICU = !is.na(ICU.start)) %>%
    # If hospital end is known but ICU end is not, impossible to resolve
    filter(!(!is.na(hospital.end) & is.na(ICU.end) & ever.ICU)) %>%
    mutate(last.date = pmax(hospital.end, ICU.end, censored.date, na.rm = T))%>%
    filter(last.date>=0) %>% 
    filter(!(censored & is.na(censored.date)))  # Drop patients who are censored but do not have censored dates 
  
  overall.start <- 0
  overall.end <- quantile(timings.wrangle$hospital.end, 0.99, na.rm = T)
  
  # this generates a table of the status of every patient on every day
  complete.timeline <- map(1:nrow(timings.wrangle), function(pat.no){
    times <- map(overall.start:overall.end, function(day){
      if(!timings.wrangle$ever.ICU[pat.no]){
        if(!timings.wrangle$censored[pat.no] & is.na(timings.wrangle$hospital.end[pat.no])){
          # this happens with an exit code but no exit date. We don't know what happened after admission
          "unknown"
        } else if(timings.wrangle$censored[pat.no]){
          if(day <= timings.wrangle$censored.date[pat.no]){
            "Ward"
          } else {
            "Censored"
          }
        } else {
          if(day <= timings.wrangle$hospital.end[pat.no]){
            "Ward"
          } else {
            as.character(timings.wrangle$final.status[pat.no])
          }
        }
      } else {
        if(!timings.wrangle$censored[pat.no] & is.na(timings.wrangle$hospital.end[pat.no])){
          # this happens with an exit code but no exit date. We don't know what happened after ICU admission or ICU exit if recorded
          if(day <= timings.wrangle$ICU.start[pat.no]){
            "Ward"
          } else if(!is.na(timings.wrangle$ICU.end[pat.no]) & day <= timings.wrangle$ICU.end[pat.no]){
            "ICU"
          } else {
            "unknown"
          }
        } else if(timings.wrangle$censored[pat.no]){
          if(day <= timings.wrangle$censored.date[pat.no]){
            if(day <= timings.wrangle$ICU.start[pat.no]) {
              "Ward"
            } else if(is.na(timings.wrangle$ICU.end[pat.no]) | day <= timings.wrangle$ICU.end[pat.no]) {
              "ICU"
            } else {
              "Ward"
            }
          } else {
            "Censored"
          }
        } else {
          if(day <= timings.wrangle$hospital.end[pat.no]){
            if(day <= timings.wrangle$ICU.start[pat.no]) {
              "Ward"
            } else if(is.na(timings.wrangle$ICU.end[pat.no]) | day <= timings.wrangle$ICU.end[pat.no]) {
              "ICU"
            } else {
              "Ward"
            }
          } else {
            as.character(timings.wrangle$final.status[pat.no])
          }
        }
      }
    })
    names(times) <- glue::glue("day_{overall.start:overall.end}")
    times$subjid <- timings.wrangle$subjid[pat.no]
    times
  }) %>%
    bind_rows()
  
  n.days <- ncol(complete.timeline) - 1
  
  complete.timeline.2 <- complete.timeline %>%
    pivot_longer(all_of(1:n.days), names_to = "day", values_to = "status") %>%
    dplyr::select(subjid, day, status) %>%
    dplyr::mutate(day = map_dbl(day, function(x) as.numeric(str_split_fixed(x, "_", 2)[2]))) %>%     
    mutate(status=replace(status, status=="Unknown outcome", "unknown"),
           status=replace(status, status=="Censored", "Ongoing care")) %>%  #LTFU as unknown? censored as ongoing care?
    dplyr::mutate(status = factor(status, levels = c("discharge", "transferred","unknown", "Ongoing care", "Ward", "ICU", "death"))) %>%
    ungroup()
  
  slider <-  input.tbl %>%
    select(usubjid, slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
    mutate(subjid=usubjid) %>% 
    select(-usubjid)
  
  final_dt <- complete.timeline.2 %>% 
    left_join(slider, by="subjid") %>% 
    as_tibble()
  
  final_dt
}
status_by_time_after_admission_input<- status.by.time.after.admission.prep(input.tbl)
save(status_by_time_after_admission_input, file = "status_by_time_after_admission_input.rda")



