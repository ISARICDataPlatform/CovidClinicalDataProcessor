


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
                                    nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
                                    nice.symptom=="Vomiting nausea"~ "Vomiting/nausea",
                                    TRUE ~ nice.symptom))
  
  symptom.prevalence.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.symptom.mapper) %>%
    as_tibble() 
}

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
                                    nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
                                    nice.symptom=="Vomiting nausea"~ "Vomiting/nausea",
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







#' Aggregate data for case enrolment over time by site
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr janitor
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @return A \code{tibble} containing the input data for the moving map
#' @export patient.enrolment.site.time.map.prep


patient.site.time.map.prep <- function(input.tbl){
  
  patient.site.time.map.input   <- input.tbl %>%
    tabyl(date_start,siteid_final)
  
}


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
    }))%>%
  mutate(nice.treatment = case_when(treatment=='Inotropes vasopressors' ~ 'Inotropes/vasopressors',
                                      TRUE ~ nice.treatment))
  
  
  treatment.use.proportion.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    as_tibble()
}


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
  
  
  nice.treatment.mapper <- tibble(treatment = unique(treatment.use.proportion.input$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))%>%
    mutate(nice.treatment = case_when(treatment=='Inotropes vasopressors' ~ 'Inotropes/vasopressors',
                                      TRUE ~ nice.treatment))
  
  
  icu.treatment.use.proportion.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    as_tibble()
}


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
             cov_det_id,
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
             embargo_length
    ))%>%
    mutate(oxygen_therapy=FALSE)%>%
    mutate(oxygen_therapy=case_when(
      treat_high_flow_nasal_canula_oxygen_therapy==TRUE|
        treat_nasal_mask_oxygen_therapy==TRUE|
        treat_non_invasive_ventilation==TRUE|
        treat_invasive_ventilation==TRUE
      ~TRUE,
      is.na(treat_high_flow_nasal_canula_oxygen_therapy) &
        is.na(treat_nasal_mask_oxygen_therapy) &
        is.na (treat_non_invasive_ventilation) &
        is.na(treat_invasive_ventilation) ~
        NA,
      TRUE~oxygen_therapy))%>%
    mutate(icu_oxygen_therapy=FALSE)%>%
    mutate(icu_oxygen_therapy=case_when(
      icu_treat_high_flow_nasal_canula_oxygen_therapy==TRUE|
        icu_treat_nasal_mask_oxygen_therapy==TRUE|
        icu_treat_non_invasive_ventilation==TRUE|
        icu_treat_invasive_ventilation==TRUE~TRUE,
      is.na(icu_treat_high_flow_nasal_canula_oxygen_therapy)&
        is.na(icu_treat_nasal_mask_oxygen_therapy)&
        is.na(icu_treat_non_invasive_ventilation)&
        is.na(icu_treat_invasive_ventilation)&
        ~NA,
      TRUE~icu_oxygen_therapy))
}
       

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
                                 Description==""~'Unknown',
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
    mutate(Description=case_when(Description==TRUE~"Yes",
                                 Description==FALSE~"No",
                                 TRUE~"Unknown"))%>%
    mutate(count=1)%>%
    group_by(Description)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(value=round(n/tot,digit=2))%>%
    mutate(value=paste0(n," (",value, ")"))%>%
    select(Description,value)%>%
    arrange(Description, levels=c('Yes',
                                  'No',
                                  'Unknown'))
  
  
  out<-rbind(size_cohort,c('By sex',''),by_sex,c('By outcome status',''),by_outcome,
             c('By age group',''), by_age,c('Admitted to ICU/HDU?',''),by_icu  )  
  
  #out<-rbind(size_cohort,c('',''),c('By sex',''),by_sex,c('',''),c('By outcome status',''),by_outcome,c('',''),
  #          c('By age group',''), by_age,c('',''),c('Admitted to ICU/HDU?',''),by_icu  )  
  
}




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
    #'',
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
    select("Ongoing care", Death, Discharge, LTFU)%>%
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
    filter(Variable!="")%>%
    mutate(count=1)%>%
    group_by(slider_outcome)%>%
    mutate(tot = sum(count)) %>%
    ungroup()%>%
    group_by(Variable,slider_outcome, tot)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = Variable, names_from = slider_outcome,  values_from = prop)%>%
    select("Ongoing care", Death, Discharge, LTFU)
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
                                      #'',
                                      'Sex',
                                      'Female',
                                      'Male')))
  
}




#' Prepare Table3. symptoms
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export outcome.age.sex.table
#' 
#' 
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
    summarise(n = sum(count, na.rm=T))%>%
    mutate(prop=round(n/tot,digit=2))%>%
    ungroup()
  
  data2<-out%>%
    filter(value=="Unknown")%>%
    filter(prop<0.95)%>%select(symptom)
  
  out<-left_join(data2, out) %>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = symptom, names_from = value,  values_from = prop)%>%
    select(symptom, Present, Absent, Unknown)
  
  data<-data%>%filter(value==TRUE)%>%tabyl(symptom)%>%select(-c(percent))
  nice.symptom.mapper <- tibble(symptom = unique(data$symptom)) %>%
    mutate(nice.symptom = map_chr(symptom, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    })) %>%
    mutate(nice.symptom = case_when(nice.symptom=="Altered consciousness confusion" ~ "Altered consciousness/confusion",
                                    nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
                                    nice.symptom=="Vomiting nausea"~ "Vomiting/nausea",
                                    TRUE ~ nice.symptom))%>%
    left_join(data)
  
  out<-out%>%
    #lazy_dt(immutable = TRUE) %>%
    left_join(nice.symptom.mapper)%>%
    rename(Symptoms=nice.symptom)%>%
    arrange(desc(n))%>%
    select(Symptoms,Present, Absent, Unknown)%>%
    as_tibble() 
  
}



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
    mutate(prop=round(n/tot,digit=2))#%>%
  
  data2<-out%>%
    filter(value=="Unknown")%>%
    filter(prop<0.95)%>%select(comorbidity)
  
  out<-left_join(data2, out) %>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = comorbidity, names_from = value,  values_from = prop)%>%
    ungroup()
  
  out<-replace(out,is.na(out),as.character("0 (0)"))
  
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
    filter(nice.comorbidity!="Other comorbidities" & nice.comorbidity!="Smoking former")%>%
    arrange(desc(n))%>%
    #rename(Comorbidities=comorbidity)%>%
    select("Comorbidities"=nice.comorbidity,Present, Absent, Unknown)%>%
    as_tibble() 
  
}




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
    mutate(prop=round(n/tot,digit=2))#%>%
  data2<-out%>%
    filter(value=="Unknown")%>%
    filter(prop<0.95)%>%select(treatment)
  
  out<-left_join(data2, out) %>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    pivot_wider(id_cols = treatment, names_from = value,  values_from = prop)%>%
    ungroup()
  
  out<-replace(out,is.na(out),as.character("0 (0)"))
  
  data<-data%>%filter(value==TRUE)%>%tabyl(treatment)%>%select(-c(percent))
  
  nice.treatment.mapper <- tibble(treatment = unique(out$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))%>%
    mutate(nice.treatment = case_when(treatment=='Inotropes vasopressors' ~ 'Inotropes/vasopressors',
                                      TRUE ~ nice.treatment))%>%
    left_join(data)
  
  out %>%
    #lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    arrange(desc(n))%>%
    select("Treatments"=nice.treatment,Present, Absent, Unknown)%>%
    #rename(Treatments=treatment)%>%
    as_tibble() 
  
}




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









