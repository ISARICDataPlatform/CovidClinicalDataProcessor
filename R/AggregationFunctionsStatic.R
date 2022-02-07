#' @keywords internal
#' @export extract.age.boundaries.2
extract.age.boundaries.2 <- function(agestring, lower = TRUE){
  agestring <- as.character(agestring)
  if(is.na(agestring)){
    NA
  } else if(agestring == "90+"){
    if(lower){
      90
    } else {
      119
    }
  } else if(lower){
    as.numeric(str_split_fixed(agestring, "-", Inf)[1])
  } else {
    as.numeric(str_split_fixed(agestring, "-", Inf)[2]) - 1
  }
}

#' Aggregate data for the summary and flowchart
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the summary and flowchart
#' @export summary.prep
#' 
summary.input.prep<- function(input.tbl){
  input.tbl%>%
    select(siteid_final,
           starts_with("slider_"),
           age,
           date_admit,
           cov_det_id,
           dur_ho,
           dur_icu,
           t_ad_icu,
           t_son_ad,
           outcome,
           #slider_outcome,
           #slider_icu_ever,
           d1_oxygen_therapy,
           oxygen_therapy,
           icu_oxygen_therapy,
           treat_oxygen_therapy,
           treat_high_flow_nasal_cannula,
           treat_non_invasive_ventilation,
           treat_invasive_ventilation,
           treat_antibiotic_agents,
           treat_antiviral_agents,
           treat_corticosteroids,
           treat_oxygen_therapy_noimv,
           vs_oxysat,
           vs_oxysat_oxygen_therapy,
           vs_oxysat_room_air,
           vs_oxysat_unknown,
           icu_treat_oxygen_therapy,
           icu_treat_antibiotic_agents,
           icu_treat_antiviral_agents,
           icu_treat_non_invasive_ventilation,
           icu_treat_invasive_ventilation,
           icu_treat_high_flow_nasal_cannula,
           icu_treat_oxygen_therapy_noimv,
           t_ad_niv,
           t_ad_imv,
           dur_niv,
           dur_imv,
           income,
           clin_diag_covid_19)
}
#' Data for the report summary
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the symptoms upset plot
#' @export summary.input.overall.prep

summary.input.overall.prep<- function(input.tbl){
  input.tbl%>%
    select(c(siteid_final,
             starts_with("slider_"),
             cov_det_id,
             income,
             clin_diag_covid_19
    ))
  
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
    filter(!is.na(date_start) & !is.na(siteid_final))%>%
    mutate(count=1)%>%
    group_by(siteid_final,date_start)%>%
    summarise(n_patients=sum(count,na.rm=T))
  
}



#' Data for the report summary
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export age.pyramid.prep
age.pyramid.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    dplyr::select(slider_sex, slider_agegp10, slider_outcome) %>%
    group_by(slider_sex, slider_outcome, slider_agegp10) %>%
    summarise(count = n()) %>%
    as_tibble() 
}

#' Aggregate data for outcome by admission date plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr tidyr forcats
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the outcome by admission date plot
#' @export outcome.admission.date.prep
outcome.admission.date.prep <- function(input.tbl){
  
  input.tbl <- input.tbl %>% 
    mutate(epiweek.admit=epiweek(date_start),
           year.admit=year(date_start),
           year.epiweek.admit=map_chr(date_start, function(ds){
             if(month(ds) == 1 & epiweek(ds) >= 50){
               glue("{year(ds)-1}-{epiweek(ds)}")
             } else {
               glue("{year(ds)}-{epiweek(ds)}")
             }
           }),
           calendar.year.admit=year(date_start),
           calendar.month.admit=month(date_start)) 
  
  epiweek.order <- glue("{c(rep(2019,4), rep(2020, 53), rep(2021,max(input.tbl$epiweek.admit[which(input.tbl$year.admit == 2021 & input.tbl$epiweek.admit!=53)], na.rm = T)))}-{c(49:52, 1:53, 1:max(input.tbl$epiweek.admit[which(input.tbl$year.admit == 2021 & input.tbl$epiweek.admit!=53)], na.rm = T))}")

  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    mutate(year.epiweek.admit = factor(year.epiweek.admit, levels = epiweek.order)) %>%
    filter(!is.na(year.epiweek.admit) & !is.na(slider_outcome)) %>%
    select(calendar.year.admit, calendar.month.admit, year.epiweek.admit,slider_outcome) %>%
    group_by(calendar.year.admit, calendar.month.admit,year.epiweek.admit,slider_outcome) %>%
    summarise(count = n()) %>%
    as_tibble() 
}

#' @return A \code{tibble} containing the input data for the symptoms upset plot
#' @export summary.input.prep

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
    select(slider_agegp10, any_of(starts_with("symptoms")), lower.age.bound, upper.age.bound) %>%
    as.data.table() %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptom", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(slider_agegp10, symptom, lower.age.bound, upper.age.bound) %>%
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
                                    nice.symptom=="Lost altered sense of smell"~ "Lost/altered sense of smell",
                                    nice.symptom=="Lost altered sense of taste"~ "Lost/altered sense of taste",
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
    select(usubjid, starts_with("symptoms"))
  
  n.symp <- ncol(data2) - 1 #changed here
  
  data2 <- data2 %>%
    pivot_longer(2:(n.symp+1), names_to = "Condition", values_to = "Present") %>%#changed to symp
    filter(!is.na(Present))
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Condition) %>%
    dplyr::summarise(Present = sum(Present, na.rm = TRUE), Total = n()) %>%
    mutate(prop=Present/Total)%>%
    ungroup() %>%
    filter(Total>50000) %>% 
    filter(Condition != "symptoms_other_signs_and_symptoms") %>%
    arrange(desc(prop)) %>%
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
                                    nice.symptom=="Lost altered sense of smell"~ "Lost/altered sense of smell",
                                    nice.symptom=="Lost altered sense of taste"~ "Lost/altered sense of taste",
                                    TRUE ~ nice.symptom))
  patients_symp<-input.tbl %>%
    select(usubjid, starts_with("symptoms"))%>%
    pivot_longer(2:(n.symp+1), names_to = "Condition", values_to = "Present") %>%#changed to symp
    filter(!is.na(Present))%>%
    distinct(usubjid, .keep_all =T)%>%select(usubjid)
  
  top.n.conditions.tbl <- patients_symp%>%left_join(input.tbl)%>%
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
    #mutate(studyid=substr(usubjid,1, 7))%>%#added
    mutate(condstring = map_chr(conditions.present, function(cp){
      paste(sort(cp), collapse = "-")
    })) %>%
    select(-conditions.present) %>%
    group_by(condstring
    ) %>% 
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
    group_by(slider_agegp10, comorbidity, lower.age.bound, upper.age.bound) %>%
    summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present)))%>%
    filter(comorbidity!="comorbid_other_comorbidities")%>%
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
    filter(!is.na(Present))
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Condition) %>%
    dplyr::summarise(Present = sum(Present, na.rm = TRUE), Total = n()) %>%
    mutate(prop=Present/Total)%>%
    ungroup() %>%
    filter(Total>50000) %>% 
    filter(Condition != "other_mhyn") %>%
    filter(Condition!="comorbid_other_comorbidities")%>%
    arrange(desc(prop)) %>%
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
  
  patients<-input.tbl %>%
    select(usubjid, starts_with("comorb"))%>%
    pivot_longer(2:( n.comorb+1), names_to = "Condition", values_to = "Present") %>%#changed to symp
    filter(!is.na(Present))%>%
    distinct(usubjid, .keep_all =T)%>%select(usubjid)
  
  top.n.conditions.tbl <- patients%>%left_join(input.tbl)%>%
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
    group_by(condstring) %>% 
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
  
  input.tbl<-input.tbl%>%select(-c(treat_pacing, 
                                   #treat_mechanical_support, 
                                   treat_immunostimulants, 
                                   treat_antiinflammatory, 
                                   treat_oxygen_therapy_noimv,
                                   treat_other_interventions,
                                   treat_antimalarial_agents,
                                   treat_agents_acting_on_the_renin_angiotensin_system))
  
  treatment.use.proportion.input <- input.tbl %>%
    select(any_of(starts_with("treat"))) %>%
    as.data.table() %>%
    pivot_longer(any_of(starts_with("treat")), names_to = "treatment", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(treatment) %>%
    summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present))) %>%
    as_tibble()
  
  nice.treatment.mapper <- tibble(treatment = unique(treatment.use.proportion.input$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))%>%
    mutate(nice.treatment = case_when(treatment=='treat_inotropes_vasopressors' ~ 'Inotropes/vasopressors',
                                      TRUE ~ nice.treatment))
  
  
  treatment.use.proportion.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    filter(times.recorded>90000) %>% 
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
  
  input.tbl<-input.tbl%>%select(-c(treat_pacing, 
                                   #treat_mechanical_support, 
                                   treat_immunostimulants, 
                                   treat_antiinflammatory, 
                                   treat_oxygen_therapy_noimv,
                                   treat_high_flow_nasal_cannula,
                                   treat_non_invasive_ventilation,
                                   treat_invasive_ventilation,
                                   treat_other_interventions,
                                   treat_antimalarial_agents,
                                   treat_agents_acting_on_the_renin_angiotensin_system))
  data2 <- input.tbl %>%
    select(usubjid, starts_with("treat"))
  
  n.treat <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.treat+1), names_to = "Treatment", values_to = "Present") %>%
    filter(!is.na(Present))
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Treatment) %>%
    dplyr::summarise(Present = sum(Present, na.rm = TRUE), Total = n()) %>%
    filter(Total>90000) %>% 
    mutate(prop=Present/Total)%>%
    ungroup() %>%
    arrange(desc(prop)) %>%
    slice(1:max.treatments) %>%
    pull(Treatment)
  
  
  nice.treatment.mapper <- tibble(treatment = unique(most.common)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))
  
  patients<-input.tbl %>%
    select(usubjid, starts_with("treat"))%>%
    pivot_longer(2:(n.treat+1), names_to = "Condition", values_to = "Present") %>%#changed to symp
    filter(!is.na(Present))%>%
    distinct(usubjid, .keep_all =T)%>%select(usubjid)
  
  
  top.n.treatments.tbl <- patients%>%left_join(input.tbl)%>%
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
    group_by(condstring) %>% 
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
    select(-icu_treat_oxygen_therapy_noimv) %>% 
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, slider_icu_ever, any_of(starts_with("icu_treat")), lower.age.bound, upper.age.bound) %>%
    as.data.table() %>%
    pivot_longer(any_of(starts_with("icu_treat")), names_to = "treatment", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(treatment) %>%
    summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present))) %>%
    as_tibble()
  
  nice.treatment.mapper <- tibble(treatment = unique(icu.treatment.use.proportion.input$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 11, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))%>%
    mutate(nice.treatment = case_when(treatment=='icu_treat_inotropes_vasopressors' ~ 'Inotropes/vasopressors',
                                      TRUE ~ nice.treatment))
  
  
  icu.treatment.use.proportion.input%>%
    #lazy_dt(immutable = TRUE) %>%
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
    select(usubjid, starts_with("icu_treat")) %>% 
    select(-icu_treat_oxygen_therapy_noimv) 
  
  n.treat <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.treat+1), names_to = "Treatment", values_to = "Present") %>%
    filter(!is.na(Present))
  
  # get the most common
  
  most.common <- data2 %>%
    group_by(Treatment) %>%
    dplyr::summarise(Present = sum(Present, na.rm = TRUE), Total = n()) %>%
    mutate(prop=Present/Total)%>%
    ungroup() %>%
    filter(Total>10000) %>% 
    arrange(desc(prop)) %>%
    slice(1:max.treatments) %>%
    pull(Treatment)
  
  
  nice.treatment.mapper <- tibble(treatment = unique(icu.treatment.use.proportion.input$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 11, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))%>%
    mutate(nice.treatment = case_when(treatment=='icu_treat_inotropes_vasopressors' ~ 'Inotropes/vasopressors',
                                      TRUE ~ nice.treatment))
  
  patients<-input.tbl %>%
    select(usubjid, starts_with("icu_treat"))%>%
    pivot_longer(2:(n.treat+1), names_to = "Condition", values_to = "Present") %>%#changed to symp
    filter(!is.na(Present))%>%
    distinct(usubjid, .keep_all =T)%>%select(usubjid)
  
  
  top.n.treatments.tbl <- patients%>%left_join(input.tbl)%>%
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
    group_by(condstring) %>% 
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
  
  Description<-c(
    'Female',
    'Male',
    'Unknown')
  Description<-data.frame(Description)
  
  by_sex<-input.tbl %>%
    mutate(Description=slider_sex)%>%
    mutate(Description=replace(Description,is.na(Description)|Description=="","Unknown"))%>%
    mutate(count=1)%>%
    group_by(Description)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(value=round(n/tot,digit=2))%>%
    mutate(value=paste0(n," (",value, ")"))%>%
    select(Description,value)%>%
    full_join(Description)%>%
    mutate(value=replace(value,is.na(value),"0 (0)"))%>%
    arrange(Description, levels=c('Female',
                                  'Male',
                                  'Unknown'))
  
  Description<-c(
    'Death',
    'Discharge',
    #'Ongoing care',
    'LTFU')
  Description<-data.frame(Description)
  
  by_outcome<-input.tbl%>%
    mutate(Description=slider_outcome)%>%
    #mutate(Description=replace(Description,is.na(Description),"Unknown"))%>%
    mutate(count=1)%>%
    group_by(Description)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(value=round(n/tot,digit=2))%>%
    mutate(value=paste0(n," (",value, ")"))%>%
    select(Description,value)#%>%
  #full_join(Description)%>%
  #mutate(value=replace(value,is.na(value),"0 (0)"))%>%
  #arrange(Description, levels=c('Death',
  #                             'Discharge',
  #                            #'Ongoing care',
  #                           'LTFU'))
  
  
  Description<-c(
    #'By age',
    '0-9',
    '10-19',
    '20-29',
    '30-39',
    '40-49',
    '50-59',
    '60-69',
    '70+' ,
    'Unknown')
  Description<-data.frame(Description)
  
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
    select(Description,value)%>%
    full_join(Description)%>%
    mutate(value=replace(value,is.na(value),"0 (0)"))%>%
    arrange(Description, levels=c(
      #'By age',
      '0-9',
      '10-19',
      '20-29',
      '30-39',
      '40-49',
      '50-59',
      '60-69',
      '70+' ,
      'Unknown'))
  
  Description<-c(
    'Yes',
    'No',
    'Unknown')
  Description<-data.frame(Description) 
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
    full_join(Description)%>%
    mutate(value=replace(value,is.na(value),"0 (0)"))%>%
    arrange(Description, levels=c('Yes',
                                  'No',
                                  'Unknown'))
  
  
  out<-rbind(size_cohort,
             c('By sex',''),by_sex,
             c('By outcome status',''),by_outcome,
             c('By age group',''), by_age,
             c('Admitted to ICU/HDU?',''),by_icu  )  
  
  
}


#' Prepare Table2. Outcome by age and sex
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export outcome.age.sex.table
#' 
outcome.age.sex.prep <- function(input.tbl){
  
  #slider_outcome<-c('Death', 'Discharge','Ongoing care', 'LTFU')
  slider_outcome<-c('Death', 'Discharge', 'LTFU')
  slider_outcome<-data.frame(slider_outcome)
  
  Variable<-c(
    'Female',
    'Male')
  Variable<-data.frame(Variable) 
  
  
  
  sex<-input.tbl %>%
    select(slider_sex,slider_outcome)%>%
    mutate(Variable=slider_sex)%>%
    filter(!(is.na(Variable)| Variable=="")) %>%
    mutate(count=1)%>%
    group_by(slider_sex)%>%
    mutate(tot = sum(count)) %>%
    ungroup()%>%
    group_by(Variable,slider_outcome, tot)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    full_join(slider_outcome)%>%
    pivot_wider(id_cols = Variable, names_from = slider_outcome,  values_from = prop)%>%
    full_join(Variable)%>%
    filter(!is.na(Variable))%>%
    arrange(Variable, levels=c('Female',
                               'Male'))%>%
    #select('Death', 'Discharge','Ongoing care', 'LTFU')%>%
    select('Death', 'Discharge', 'LTFU')%>%
    ungroup()
  
  sex_cfr <- input.tbl %>%
    select(slider_sex,slider_outcome)%>%
    mutate(Variable=slider_sex)%>%
    filter(!is.na(Variable))%>%
    filter(Variable!="")%>%
    filter(slider_outcome!='LTFU')%>%
    mutate(count=1)%>%
    group_by(Variable)%>%
    mutate(tot = sum(count)) %>%
    ungroup()%>%
    filter(slider_outcome=='Death')%>%
    group_by(Variable,slider_outcome, tot)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    ungroup()%>%
    mutate("Case fatality ratio"=round(n/tot,digit=2))%>%
    select(Variable,"Case fatality ratio")
  sex<-sex%>%left_join(sex_cfr)
  sex<-replace(sex,is.na(sex),as.character("0 (0)"))
  
  
  Variable<-c(
    '0-9',
    '10-19',
    '20-29',
    '30-39',
    '40-49',
    '50-59',
    '60-69',
    '70+')
  Variable<-data.frame(Variable)
  
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
    group_by(Variable)%>%
    mutate(tot = sum(count)) %>%
    ungroup()%>%
    group_by(Variable,slider_outcome, tot)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    mutate(prop=round(n/tot,digit=2))%>%
    mutate(prop=paste0(n," (",prop, ")"))%>%
    full_join(slider_outcome)%>%
    pivot_wider(id_cols = Variable, names_from = slider_outcome,  values_from = prop)%>%
    full_join(Variable)%>%
    filter(!is.na(Variable))%>%
    arrange(Variable, levels=c('0-9',
                               '10-19',
                               '20-29',
                               '30-39',
                               '40-49',
                               '50-59',
                               '60-69',
                               '70+'))%>%
    #select('Death', 'Discharge','Ongoing care', 'LTFU')%>%
    select('Death', 'Discharge', 'LTFU')%>%
    ungroup()
  
  age_cfr <- input.tbl %>%
    select(slider_agegp10,slider_outcome)%>%
    mutate(slider_agegp10=as.character(slider_agegp10))%>%
    mutate(Variable=case_when(slider_agegp10=="90+" |
                                slider_agegp10=="80-89" |
                                slider_agegp10=="70-79" ~ "70+",
                              TRUE~slider_agegp10))%>%
    filter(!is.na(Variable))%>%
    filter(Variable!="")%>%
    filter(slider_outcome!='LTFU')%>%
    mutate(count=1)%>%
    group_by(Variable)%>%
    mutate(tot = sum(count)) %>%
    ungroup()%>%
    filter(slider_outcome=='Death')%>%
    group_by(Variable,slider_outcome, tot)%>%
    summarise(n = sum(count, na.rm=T)) %>%
    ungroup()%>%
    mutate("Case fatality ratio"=round(n/tot,digit=2))%>%
    select(Variable,"Case fatality ratio")
  age<-age%>%left_join(age_cfr)
  age<-replace(age,is.na(age),as.character("0 (0)"))
  
  out<-rbind(c('Age','','','','',''),age,
             c('Sex','','','','',''),sex)  
  
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
                                    nice.symptom=="Lost altered sense of smell"~ "Lost/altered sense of smell",
                                    nice.symptom=="Lost altered sense of taste"~ "Lost/altered sense of taste",
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
  data<-select(input.tbl, c(starts_with("comorbid_")),slider_sex) %>%
    pivot_longer(starts_with("comorbid_"), names_to = "comorbidity", values_to = "value") %>% 
    filter(!(slider_sex=="Male"&comorbidity=="comorbid_pregnancy"&is.na(value)))
  
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
  
  input.tbl<-input.tbl%>%select(-c(treat_pacing, 
                                   #treat_mechanical_support, 
                                   treat_immunostimulants, 
                                   treat_antiinflammatory, 
                                   treat_oxygen_therapy_noimv,
                                   treat_other_interventions,
                                   treat_antimalarial_agents,
                                   treat_agents_acting_on_the_renin_angiotensin_system))
  
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
  
  nice.treatment.mapper <- tibble(treatment = unique(treatment.use.proportion.input$treatment)) %>%
    mutate(nice.treatment = map_chr(treatment, function(st){
      temp <- substr(st, 7, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    }))%>%
    mutate(nice.treatment = case_when(treatment=='treat_inotropes_vasopressors' ~ 'Inotropes/vasopressors',
                                      treatment=='treat_off_label_compassionate_use_medications' ~ 'Off label/compassionate use medications',
                                      
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
    mutate_at(vars(all_of(starts_with("t_"))), function(x){as.numeric(x)})%>%
    pivot_longer(c(starts_with("t_")), names_to = "key_time", values_to = "value")
  
  out<-select(input.tbl, c(starts_with("dur_"))) %>%
    mutate_at(vars(all_of(starts_with("dur_"))), function(x){as.numeric(x)})%>%
    pivot_longer(c(starts_with("dur_")), names_to = "key_time", values_to = "value")%>%
    rbind(data)%>%
    filter(!is.na(value))%>%
    filter(value>=0)%>%
    group_by(key_time)%>%
    summarise(mean=mean(value,na.rm=T),
              sd=sd(value,na.rm=T),
              median=median(value,na.rm=T),
              #iqr=IQR(value,na.rm=T),
              q1=quantile(value,na.rm=T,0.25),
              q3=quantile(value,na.rm=T,0.75))%>%
    mutate(IQR=paste0(q1,"-",q3)) %>% 
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
    rename("Mean"=mean)%>%
    rename("SD"=sd)%>%
    rename("Median"=median) %>% 
    select(-q1,-q3)
  
}

#' vital signs
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @return A \code{tibble} containing the input data for the vital sign tables plot

#vs_resp
func_plots_vs_resp <- function(input.tbl){
  data_plot_vs_resp <- select(input.tbl, c(vs_resp, slider_agegp10,upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#vs_hr
func_plots_vs_hr <- function(input.tbl){
  data_plot_vs_hr <- select(input.tbl, c(vs_hr, slider_agegp10,)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#vs_temp
func_plots_vs_temp <- function(input.tbl){
  data_plot_vs_temp <- select(input.tbl, c(vs_temp, slider_agegp10, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#vs_sysbp
func_plots_vs_sysbp <- function(input.tbl){
  data_plot_vs_sysbp <- select(input.tbl, c(vs_sysbp, slider_agegp10,upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}
#vs_oxysat
func_plots_vs_oxysat <- function(input.tbl){
  data_plot_vs_oxysat <- select(input.tbl, c(vs_oxysat_room_air, slider_agegp10,upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
  
}
#vs_oxysat_therapy
func_plots_vs_oxysat_therapy <- function(input.tbl){
  data_plot_vs_oxysat_therapy <- select(input.tbl, c(vs_oxysat_oxygen_therapy, slider_agegp10,upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
  
}

#######For Jia

#' Box and whisker plots for laboratory results at hospital presentation stratified by age group.
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @return A \code{tibble} containing the input data for the lab data

#crp
func_plot_lab_crp <- function(input.tbl){
  data_plot_lab_crp <- select(input.tbl, slider_agegp10,lab_crp) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}



#lab_lym
func_plot_lab_lym <- function(input.tbl){
  data_plot_lab_lym <- select(input.tbl,slider_agegp10,lab_lym) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_neut
func_plot_lab_neut <- function(input.tbl){
  data_plot_lab_neut <- select(input.tbl, slider_agegp10,lab_neut) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame() 
}

#lab_wbc
func_plot_lab_crp <- function(input.tbl){
  data_plot_lab_wbc <- select(input.tbl,slider_agegp10,lab_wbc) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_urean
func_plot_lab_urean  <- function(input.tbl){
  data_plot_lab_urean <- select(input.tbl, slider_agegp10,lab_urean) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_pt
func_plot_lab_pt <- function(input.tbl){
  data_plot_lab_pt <- select(input.tbl, slider_agegp10,lab_pt) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame() 
}

#lab_alt
func_plot_lab_alt <- function(input.tbl){
  data_plot_lab_alt <- select(input.tbl,slider_agegp10,lab_alt) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_aptt
func_plot_lab_aptt <- function(input.tbl){
  data_plot_lab_aptt <- select(input.tbl, slider_agegp10,lab_aptt) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}


#lab_bili
func_plot_lab_bili <- function(input.tbl){
  data_plot_lab_bili <- select(input.tbl, slider_agegp10,lab_bili) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_ast
func_plot_lab_ast <- function(input.tbl){
  data_plot_lab_ast <- select(input.tbl, slider_agegp10,lab_ast) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame() 
}



#' Comorbidities by age
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @return A \code{tibble} containing the input data for the Comorbidities data

#comorbid_asthma
func_plot_comorbid_asthma <- function(input.tbl){
  data_plot_comorbid_asthma <- select(input.tbl, slider_agegp10,comorbid_asthma) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value") %>%
    as.data.frame()  
}


#comorbid_malignant_neoplasm
func_plot_comorbid_malignant_neoplasm <- function(input.tbl){
  data_plot_comorbid_malignant_neoplasm <- select(input.tbl,slider_agegp10,comorbid_malignant_neoplasm) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
}


#comorbid_obesity
func_plot_comorbid_obesity <- function(input.tbl){
  data_plot_comorbid_obesity <- select(input.tbl, slider_agegp10,comorbid_obesity) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
  
}

#comorbid_diabetes
func_plot_comorbid_diabetes <- function(input.tbl){
  data_plot_comorbid_diabetes <- select(input.tbl, slider_agegp10,comorbid_diabetes) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
}


#comorbid_dementia
func_plot_comorbid_dementia <- function(input.tbl){
  data_plot_comorbid_dementia <- select(input.tbl, slider_agegp10,comorbid_dementia) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
}

#comorbid_smoking
func_plot_comorbid_smoking <- function(input.tbl){
  data_plot_comorbid_smoking <- select(input.tbl, slider_agegp10, comorbid_smoking) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
  
}

#comorbid_hypertension
func_plot_comorbid_hypertension <- function(input.tbl){
  data_plot_comorbid_hypertension <- select(input.tbl, slider_agegp10,comorbid_hypertension) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value") %>%
    as.data.frame()  
}


#' symptoms by age
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @return A \code{tibble} containing the input data for the Comorbidities data


#symptoms_history_of_fever
func_plot_symptoms_history_of_fever <- function(input.tbl){
  data_plot_symptoms_history_of_fever <- select(input.tbl, slider_agegp10,symptoms_history_of_fever) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value") %>%
    as.data.frame()   
}


#symptoms_cough
func_plot_symptoms_cough <- function(input.tbl){
  data_plot_symptoms_cough <- select(input.tbl, slider_agegp10,symptoms_cough) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_cough_fever
func_plot_symptoms_cough_fever <- function(input.tbl){
  data_plot_symptoms_cough_fever <- select(input.tbl, slider_agegp10,symptoms_history_of_fever,symptoms_cough) %>%
    filter(!is.na(slider_agegp10)) %>%
    unite(col = "symptoms_cough_fever",c(symptoms_history_of_fever,symptoms_cough),sep = "_",remove = FALSE,na.rm = FALSE) %>%
    filter(symptoms_cough_fever != "NA_NA") %>%
    mutate(symptoms_cough_fever = ifelse(symptoms_cough_fever %in% c("TRUE_FALSE", "TRUE_TRUE", "TRUE_NA" ,
                                                                     "NA_TRUE" ,"FALSE_TRUE"), TRUE, FALSE)) %>%
    pivot_longer(symptoms_cough_fever, names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_shortness_of_breath
func_plot_symptoms_shortness_of_breath <- function(input.tbl){
  data_plot_symptoms_shortness_of_breath <- select(input.tbl, slider_agegp10,symptoms_shortness_of_breath) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_cought_fever_shortness_of_breath
func_plot_symptoms_cought_fever_shortness_of_breath <- function(input.tbl){
  data_plot_symptoms_cought_fever_shortness_of_breath <- select(input.tbl, 
                                                                slider_agegp10,symptoms_cough,symptoms_history_of_fever,
                                                                  symptoms_shortness_of_breath) %>%
    filter(!is.na(slider_agegp10)) %>%
    unite(col = "symptoms_cough_fever_sob",c(symptoms_history_of_fever,symptoms_cough,symptoms_shortness_of_breath),
          sep = "_",remove = FALSE,na.rm = FALSE) %>%
    filter(symptoms_cough_fever_sob != "NA_NA_NA") %>%
    mutate(symptoms_cough_fever_sob = ifelse(
      symptoms_cough_fever_sob %in% c("FALSE_FALSE_TRUE","FALSE_TRUE_FALSE","FALSE_TRUE_NA","FALSE_TRUE_TRUE","NA_FALSE_TRUE",
                                      "NA_NA_TRUE","NA_TRUE_FALSE", "NA_TRUE_NA","NA_TRUE_TRUE","TRUE_FALSE_FALSE","TRUE_FALSE_NA",  
                                      "TRUE_FALSE_TRUE","TRUE_NA_FALSE" ,"TRUE_NA_NA","TRUE_NA_TRUE","TRUE_TRUE_FALSE","TRUE_TRUE_NA",
                                      "TRUE_TRUE_TRUE" ), TRUE, FALSE)) %>%
    pivot_longer(symptoms_cough_fever_sob, names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
  
}


#symptoms_upper_respiratory_tract_symptoms 
func_plot_symptoms_upper_respiratory_tract_symptoms <- function(input.tbl){
  data_plot_symptoms_upper_respiratory_tract_symptoms <- select(input.tbl,slider_agegp10,
                                                                             symptoms_sore_throat,symptoms_runny_nose,symptoms_ear_pain) %>%
    filter(!is.na(slider_agegp10)) %>%
    unite(col = "symptoms_upper_respiratory_tract_symptoms",c(symptoms_sore_throat,symptoms_runny_nose,symptoms_ear_pain),
          sep = "_",remove = FALSE,na.rm = FALSE) %>%
    filter(symptoms_upper_respiratory_tract_symptoms != "NA_NA_NA") %>%
    mutate(symptoms_upper_respiratory_tract_symptoms = ifelse(
      symptoms_upper_respiratory_tract_symptoms %in% c(  "FALSE_NA_TRUE","FALSE_TRUE_FALSE", "FALSE_TRUE_NA","FALSE_TRUE_TRUE",    
                                                         "NA_FALSE_TRUE", "NA_NA_TRUE",    "NA_TRUE_FALSE", "NA_TRUE_NA" , "NA_TRUE_TRUE",     
                                                         "TRUE_FALSE_FALSE" , "TRUE_FALSE_NA",  "TRUE_FALSE_TRUE" ,  "TRUE_NA_FALSE"  ,  
                                                         "TRUE_NA_NA" , "TRUE_NA_TRUE" , "TRUE_TRUE_FALSE" , "TRUE_TRUE_NA",   
                                                         "TRUE_TRUE_TRUE"), TRUE, FALSE)) %>%
    pivot_longer(symptoms_upper_respiratory_tract_symptoms, names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
  
}

#symptoms_altered_consciousness_confusion
func_plot_symptoms_altered_consciousness_confusion <- function(input.tbl){
  data_plot_symptoms_altered_consciousness_confusion <- select(input.tbl,slider_agegp10,symptoms_altered_consciousness_confusion) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_constitutional
func_plot_symptoms_constitutional <- function(input.tbl){
  data_plot_symptoms_constitutional <- select(input.tbl,slider_agegp10,symptoms_muscle_aches_joint_pain,symptoms_fatigue_malaise,
                                                          symptoms_headache) %>%
    filter(!is.na(slider_agegp10)) %>%
    unite(col = "symptoms_constitutional",c(symptoms_muscle_aches_joint_pain,symptoms_fatigue_malaise,symptoms_headache),
          sep = "_",remove = FALSE,na.rm = FALSE) %>%
    filter(symptoms_constitutional != "NA_NA_NA") %>%
    mutate(symptoms_constitutional = ifelse(
      symptoms_constitutional %in% c("FALSE_FALSE_TRUE","FALSE_TRUE_FALSE","FALSE_TRUE_NA","FALSE_TRUE_TRUE","NA_FALSE_TRUE",
                                     "NA_NA_TRUE","NA_TRUE_FALSE", "NA_TRUE_NA","NA_TRUE_TRUE","TRUE_FALSE_FALSE","TRUE_FALSE_NA",  
                                     "TRUE_FALSE_TRUE","TRUE_NA_FALSE" ,"TRUE_NA_NA","TRUE_NA_TRUE","TRUE_TRUE_FALSE","TRUE_TRUE_NA",
                                     "TRUE_TRUE_TRUE" ), TRUE, FALSE)) %>%
    pivot_longer(symptoms_constitutional, names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
  
}


#symptoms_vomiting_nausea
func_plot_symptoms_vomiting_nausea <- function(input.tbl){
  data_plot_symptoms_vomiting_nausea <- select(input.tbl, slider_agegp10,symptoms_vomiting_nausea) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value") %>%
    as.data.frame()   
}


#symptoms_diarrhoea
func_plot_symptoms_diarrhoea <- function(input.tbl){
  data_plot_symptoms_diarrhoea <- select(input.tbl,slider_agegp10,symptoms_diarrhoea) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_abdominal_pain
func_plot_symptoms_abdominal_pain <- function(input.tbl){
  data_plot_symptoms_abdominal_pain <- select(input.tbl, slider_agegp10,symptoms_abdominal_pain) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#' Create the Heat admission.symptoms 
#' @title Heat admission.symptoms 
######################

admission.symptoms <- cbind(field = c("symptoms_runny_nose",
                                      "symptoms_sore_throat",
                                      "symptoms_ear_pain",
                                      "symptoms_diarrhoea",
                                      "symptoms_vomiting_nausea",
                                      "symptoms_abdominal_pain",  
                                      "symptoms_muscle_aches_joint_pain", 
                                      "symptoms_fatigue_malaise",
                                      "symptoms_headache",  
                                      "symptoms_shortness_of_breath",
                                      "symptoms_history_of_fever", 
                                      #"symptoms_wheeze", 
                                      "symptoms_cough", 
                                      "symptoms_chest_pain",
                                      "symptoms_lymphadenopathy",
                                      "symptoms_lost_altered_sense_of_taste",
                                      "symptoms_lost_altered_sense_of_smell", 
                                      "symptoms_conjunctivitis",
                                      "symptoms_bleeding",  
                                      #"symptoms_skin_ulcers", 
                                      "symptoms_skin_rash",  
                                      "symptoms_seizures",
                                      "symptoms_altered_consciousness_confusion"),
                            label = c("Runny nose",
                                      "Sore throat",
                                      "Ear pain",
                                      "Diarrhoea",
                                      "Vomiting / Nausea",
                                      "Abdominal pain",
                                      "Muscle aches / Joint pain",
                                      "Fatigue / Malaise",
                                      "Headache",
                                      "Shortness of breath",
                                      "History of fever",
                                      #"Wheezing",
                                      "Cough",
                                      "Chest pain",
                                      "Lymphadenopathy",
                                      "Loss of taste",
                                      "Loss of smell",
                                      "Conjunctivitis",
                                      "Bleeding",
                                      #"Skin ulcers",
                                      "Skin rash",
                                      "Seizures",
                                      "Altered consciousness / confusion"))
admission.symptoms <- as_tibble(admission.symptoms)

##### Prevalence of symptoms heatmap #####
#'  Plot pairwise symptom prevalance.
#'
#'  Plots a heatmap for prevalance of pairwise combinations of symptoms.
#'  The pairwise prevalence proportions are caculated amongst patients with recorded presence or absence of both symptoms.
#' @export symptom.heatmap
#' @param data \code{detailed.data}, a component of the output of \code{\link{import.and.process.data}}..
#'
#' @return  Heatmap showing the proportion of patients for each pairwise combination of symptoms.
symptom.heatmap <- function(data, admission.symptoms, asterisks = vector(), ...){
  
  data2 <- data %>%
    #data2 <- input.tbl %>%
    dplyr::select(usubjid, one_of(admission.symptoms$field))
  
  
  phi.correlation <- function(c1, c2){
    if(c1 == c2){
      return(1)
    } else {
      restricted.df <- data2 %>% dplyr::select_at(c(c1, c2))
      
      restricted.df <- restricted.df %>%
        filter((!!sym(c1) != 3) & (!!sym(c2) != 3) & !is.na(!!sym(c1)) & !is.na(!!sym(c2))) %>%
        mutate(!!sym(c1) := (!!sym(c1) == 1)) %>%
        mutate(!!sym(c2) := !!sym(c2) == 1)
      
      twobytwo <- table(restricted.df[[c1]], restricted.df[[c2]])
      # print(twobytwo)
      
      if(nrow(twobytwo) == 2 & ncol(twobytwo) == 2){
        return(phi(twobytwo))
      } else {
        return(NA)
      }
      
      
    }
  }
  
  fct.order <- c("Runny nose",
                 "Sore throat",
                 "Ear pain",
                 "Diarrhoea",
                 "Vomiting / Nausea",
                 "Abdominal pain",
                 "Muscle aches / Joint pain",
                 "Fatigue / Malaise",
                 "Headache",
                 "Shortness of breath",
                 "History of fever",
                 #"Wheezing",
                 "Cough",
                 "Chest pain",
                 "Lymphadenopathy",
                 "Loss of taste",
                 "Loss of smell",
                 "Conjunctivitis",
                 "Bleeding",
                 #"Skin ulcers",
                 "Skin rash",
                 "Seizures",
                 "Altered consciousness / confusion"  )
  
  fct.order[which(fct.order %in% admission.symptoms$label[which(admission.symptoms$field %in% asterisks)])] <- 
    glue("{fct.order[which(fct.order %in% admission.symptoms$label[which(admission.symptoms$field %in% asterisks)])]}*")
  
  
  admission.symptoms$label[which(admission.symptoms$field %in% asterisks)] <- 
    glue("{admission.symptoms$label[which(admission.symptoms$field %in% asterisks)]}*")
  
  
  combinations.tibble <- tibble(cond1 = rep(admission.symptoms$field, length(admission.symptoms$field)),
                                cond2 = rep(admission.symptoms$field, each = length(admission.symptoms$field))) %>%
    mutate(phi.correlation = map2_dbl(cond1, cond2, phi.correlation)) %>%
    left_join(admission.symptoms, by=c("cond1" = "field"), suffix = c(".x", ".y")) %>%
    left_join(admission.symptoms, by=c("cond2" = "field"), suffix = c(".x", ".y"))
  
  
  if(length(asterisks) > 0){
    fct.order[asterisks] <- glue("{fct.order[asterisks]}*")
  }
  
  combinations.tibble.2 <- combinations.tibble %>%
    mutate(label.x = factor(label.x, levels = fct.order)) %>%
    mutate(label.y = factor(label.y, levels = fct.order))
  
  return(combinations.tibble.2)
}


#' Aggregate data for length of stay within ICU
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the length of stay within plot
#' @export length.of.stay.icu.prep
length.of.stay.icu.prep <- function(input.tbl){
  
  tb1 <- input.tbl %>%
    mutate(dur_ho=as.numeric(dur_ho))%>%
    mutate(dur_ho=case_when(dur_ho>89~NA_real_,
                            TRUE~dur_ho))%>%
    lazy_dt(immutable = TRUE) %>%
    #filter(embargo_length!=TRUE & cov_det_id=="POSITIVE") %>% 
    select(slider_icu_ever,dur_ho) %>% 
    filter(dur_ho>0) %>% 
    rename(dur=dur_ho) %>% 
    mutate(type=1) %>% 
    as_tibble() 
  tb2 <- input.tbl %>%
    mutate(dur_icu=as.numeric(dur_icu))%>%
    mutate(dur_icu=case_when(dur_icu>89~NA_real_,
                             TRUE~dur_icu))%>%
    lazy_dt(immutable = TRUE) %>%
    #filter(embargo_length!=TRUE & cov_det_id=="POSITIVE") %>% 
    select(slider_icu_ever,dur_icu) %>% 
    filter(dur_icu>0) %>% 
    rename(dur=dur_icu) %>% 
    mutate(type=2) %>% 
    as_tibble()
  
  d <- rbind(tb1, tb2, deparse.level = 1) %>%
    filter(!is.na(dur))
  d$type <- factor(d$type, levels = c(1, 2), labels = c("Total hospital stay", "ICU"))
  
  d
}


#' Aggregate data for patient by country
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for patient by country plot
#' @export patient.by.country.prep
patient.by.country.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_country) %>%
    filter(!is.na(slider_country)) %>% 
    mutate(slider_country=ifelse(slider_country=="Congo","Colombia",slider_country)) %>% 
    as_tibble() 
}

#'Map data
patient.by.country.map.prep <- function(input.tbl){
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_country) %>%
    filter(!is.na(slider_country)) %>% 
    mutate(Freq = 1) %>%
    group_by(slider_country)%>%
    mutate(Freq = sum(Freq))%>%
    distinct()%>%
    as_tibble() 
}


#'Case definitions data
#'From christiana's paper
patient.by.case.def.prep <- function(input.tbl){
  input.tbl$symptoms_WHO <- NA
  input.tbl[which(input.tbl$symptoms_history_of_fever == TRUE & input.tbl$symptoms_cough == TRUE),]$symptoms_WHO <- TRUE
  input.tbl[which(apply(input.tbl[,c("symptoms_history_of_fever", "symptoms_cough", "symptoms_fatigue_malaise", 
                                     "symptoms_headache", "symptoms_muscle_aches_joint_pain", "symptoms_sore_throat",
                                     "symptoms_runny_nose", "symptoms_shortness_of_breath",
                                     #"symptoms_anorexia", 
                                     "symptoms_vomiting_nausea", "symptoms_diarrhoea", "symptoms_altered_consciousness_confusion")], 1, sum, na.rm = TRUE) >= 3),]$symptoms_WHO <- TRUE
  # anorexia not available
  input.tbl$symptoms_CDC <- NA
  input.tbl[which(apply(input.tbl[,c("symptoms_history_of_fever", # "symptoms_rigor_or_sweating",
                                     "symptoms_muscle_aches_joint_pain", "symptoms_headache", 
                                     "symptoms_sore_throat", "symptoms_lost_altered_sense_of_smell", 
                                     "symptoms_lost_altered_sense_of_taste")], 1, sum, na.rm = TRUE) >= 2),]$symptoms_CDC <- TRUE
  # chills/rigor not available
  input.tbl[which(input.tbl$symptoms_cough == TRUE | input.tbl$symptoms_shortness_of_breath == TRUE),]$symptoms_CDC <- TRUE
  # difficulty breathing not available
  input.tbl$symptoms_PHE <- NA
  input.tbl[which(input.tbl$symptoms_cough == TRUE | input.tbl$symptoms_history_of_fever == TRUE | 
                    input.tbl$symptoms_lost_altered_sense_of_smell == TRUE |
                    input.tbl$symptoms_lost_altered_sense_of_taste == TRUE),]$symptoms_PHE <- TRUE
  input.tbl$symptoms_ECDC <- NA
  input.tbl[which(input.tbl$symptoms_cough == TRUE | input.tbl$symptoms_history_of_fever == TRUE |
                    input.tbl$symptoms_shortness_of_breath == TRUE | input.tbl$symptoms_lost_altered_sense_of_smell == TRUE | 
                    input.tbl$symptoms_lost_altered_sense_of_taste == TRUE),]$symptoms_ECDC <- TRUE
  #input.tbl$slider_agegp10 <- cut(input.tbl$age, c(0, seq(20, 100, by = 10), 120), right = FALSE, include.lowest = TRUE)
  input.tbl$sars_cov2 <- as.character(input.tbl$cov_id_sarscov2 == "POSITIVE" | input.tbl$cov_det_sarscov2 == "POSITIVE")
  input.tbl[is.na(input.tbl$sars_cov2),]$sars_cov2 <- "Unknown"
  input.tbl$sars_cov2 <- factor(input.tbl$sars_cov2, labels = c("Positive", "Unknown"))
 
  symptoms_long <- input.tbl %>% 
    filter_at(vars("symptoms_history_of_fever", "symptoms_cough", "symptoms_fatigue_malaise", 
                   "symptoms_headache", "symptoms_muscle_aches_joint_pain", "symptoms_sore_throat",
                   "symptoms_runny_nose", "symptoms_shortness_of_breath",
                   "symptoms_lost_altered_sense_of_smell", 
                   "symptoms_lost_altered_sense_of_taste",
                   "symptoms_vomiting_nausea", "symptoms_diarrhoea", "symptoms_altered_consciousness_confusion"), all_vars(!is.na(.))) %>% 
    select("symptoms_WHO", "symptoms_CDC", "symptoms_PHE", "symptoms_ECDC","date_admit", "slider_country", "slider_agegp10", "sars_cov2")  %>% 
    pivot_longer(cols = -c(date_admit,slider_country,slider_agegp10, sars_cov2), names_to = "symptom", values_to = "value")
  symptoms_long$value <- factor(symptoms_long$value, levels = c("TRUE", "FALSE"), labels = c("Yes", "No"))
  # change symptom labels
  symptoms_long$symptom <- paste(toupper(substring(gsub("_", " ", gsub("symptoms_", "", symptoms_long$symptom)), 1, 1)), 
                                 substring(gsub("_", " ", gsub("symptoms_", "", symptoms_long$symptom)), 2), sep = "")
  
  
  symptoms_long <- symptoms_long %>%
    filter(!is.na(slider_agegp10)) %>%
    mutate(delete_sa = ifelse((slider_country == "South Africa")&(date_admit > "2020-07-01"),1,0))%>%
    filter(delete_sa ==0)%>%
    select(slider_agegp10,symptom,value)%>%
    mutate(count_yes = ifelse(value=="Yes",1,NA))%>%
    mutate(count_yes = ifelse(is.na(value), 0, value))%>%
    mutate(count_all = 1)%>%
    group_by(slider_agegp10,symptom)%>%
    mutate(total = sum(count_all))%>%
    mutate(present = sum(count_yes))%>%
    mutate(proportion = present/total)%>%
    select(slider_agegp10, symptom, proportion)%>%
    distinct()
  
  
}

patient.by.case.def.prep_new <- function(input.tbl){
  #############################
  #   Case definitions
  #############################
  prep=input.tbl
  
  ######################################################################################################
  # • European Centre for Disease Prevention and Control
  # At least one of: cough, fever, shortness of breath, sudden onset anosmia, ageusia or dysgeusia
  ######################################################################################################
  
  #ECDC
  ECDC_case_def <- prep   %>%
    select(symptoms_cough, symptoms_history_of_fever,symptoms_shortness_of_breath, symptoms_lost_altered_sense_of_smell,
           symptoms_lost_altered_sense_of_taste,slider_agegp10)%>%
    #The first step is to mark all of those that at least meet one of the mentioned symptoms as yes
    mutate(ECDC_yes = ifelse(symptoms_cough == TRUE | symptoms_history_of_fever == TRUE |
                               symptoms_shortness_of_breath == TRUE | symptoms_lost_altered_sense_of_smell == TRUE |
                               symptoms_lost_altered_sense_of_taste == TRUE , 1, 0)) %>%
    filter(!is.na(ECDC_yes))%>%
    group_by(ECDC_yes, slider_agegp10)%>%
    mutate(indicator = 1)%>%
    mutate(total = sum(indicator))%>%
    select(ECDC_yes, total, slider_agegp10)%>%
    unique()%>%
    pivot_wider(names_from = ECDC_yes,  values_from = total)%>%
    mutate(Yes_ECDC = `1`)%>%
    mutate(No_ECDC = `0`)%>%
    mutate(percentage_met_ECDC = round(Yes_ECDC/(No_ECDC+Yes_ECDC)*100,2))%>%
    select(slider_agegp10, Yes_ECDC,No_ECDC, percentage_met_ECDC )
  
  ######################################################################################################
  # • Public Health England
  # New cough (we use cough), or temperature f37.8°C (we use hitory of fever), or a loss or change in sense of smell or taste
  ######################################################################################################
  
  #PHE                                                        
  PHE_case_def <- prep   %>%
    select(symptoms_cough, symptoms_history_of_fever,symptoms_shortness_of_breath, symptoms_lost_altered_sense_of_smell,
           symptoms_lost_altered_sense_of_taste,slider_agegp10)%>%
    #Combine lack of taste or smell
    mutate(smell_or_taste = ifelse(symptoms_lost_altered_sense_of_taste == TRUE |  symptoms_lost_altered_sense_of_smell == TRUE, TRUE, FALSE))%>%
    
    #The first step is to mark all of those that at least meet one of the mentioned symptoms as yes
    mutate(PHE_yes = ifelse(symptoms_cough == TRUE | symptoms_history_of_fever == TRUE |smell_or_taste == TRUE , 1, 0)) %>%
    filter(!is.na(PHE_yes))%>%
    group_by(PHE_yes, slider_agegp10)%>%
    mutate(indicator = 1)%>%
    mutate(total = sum(indicator))%>%
    select(PHE_yes, total, slider_agegp10)%>%
    unique()%>%
    pivot_wider(names_from = PHE_yes,  values_from = total)%>%
    mutate(Yes_PHE = `1`)%>%
    mutate(No_PHE = `0`)%>%
    mutate(percentage_met_PHE = round(Yes_PHE/(No_PHE+Yes_PHE)*100,2))%>%
    select(slider_agegp10, Yes_PHE,No_PHE, percentage_met_PHE )
  
  ######################################################################################################
  #   • WHO:
  #   1. A combination of acute fever and cough,
  # Or
  # 2. A combination of three or more of: fever, cough, general weakness and fatigue, headache, myalgia (we use muscle aches, joint pain),
  # sore throat, coryza  (we use runny nose), dyspnoea (we use SOB), anorexia (not available), nausea and vomiting,
  # diarrhoea, altered mental status (we use altered consciousness or confusion)
  ######################################################################################################
  
  #WHO
  WHO_case_def <- prep %>%
    select(symptoms_cough, symptoms_history_of_fever, symptoms_fatigue_malaise, symptoms_headache,
           
           symptoms_diarrhoea,symptoms_muscle_aches_joint_pain, symptoms_runny_nose,
           
           symptoms_sore_throat,symptoms_shortness_of_breath, symptoms_vomiting_nausea,symptoms_altered_consciousness_confusion,
           slider_agegp10)%>%
    mutate(n_symptoms_cough = ifelse(symptoms_cough == TRUE,1,0))%>%
    mutate(n_symptoms_history_of_fever = ifelse(symptoms_history_of_fever == TRUE,1,0))%>%
    mutate(n_symptoms_fatigue_malaise = ifelse(symptoms_fatigue_malaise == TRUE,1,0))%>%
    mutate(n_symptoms_headache = ifelse(symptoms_headache == TRUE,1,0))%>%
    
    mutate(n_symptoms_diarrhoea = ifelse(symptoms_diarrhoea == TRUE,1,0))%>%
    mutate(n_symptoms_muscle_aches_joint_pain = ifelse(symptoms_muscle_aches_joint_pain == TRUE,1,0))%>%
    mutate(n_symptoms_runny_nose = ifelse(symptoms_runny_nose == TRUE,1,0))%>%
    
    
    mutate(n_symptoms_sore_throat = ifelse(symptoms_sore_throat == TRUE,1,0))%>%
    mutate(n_symptoms_shortness_of_breath = ifelse(symptoms_shortness_of_breath == TRUE,1,0))%>%
    mutate(n_symptoms_vomiting_nausea = ifelse(symptoms_vomiting_nausea == TRUE,1,0))%>%
    mutate(n_symptoms_altered_consciousness_confusion = ifelse(symptoms_altered_consciousness_confusion == TRUE,1,0))%>%
    mutate(case_def =n_symptoms_cough +n_symptoms_history_of_fever +n_symptoms_fatigue_malaise+
             n_symptoms_headache+
             
             n_symptoms_diarrhoea + n_symptoms_muscle_aches_joint_pain+ n_symptoms_runny_nose+
             
             n_symptoms_sore_throat+n_symptoms_shortness_of_breath+
             n_symptoms_vomiting_nausea+ n_symptoms_altered_consciousness_confusion)%>%
    
    #These are all that meet but ignores that some might have missing on a bunch of these sympt but still meet if 3 are reported yes
    mutate(WHO_yes_1 = ifelse(case_def>2, 1, 0))%>%
    rowwise() %>% 
    mutate(case_def2 = sum(c(n_symptoms_cough, n_symptoms_history_of_fever, n_symptoms_fatigue_malaise,
                             n_symptoms_headache,
                             
                             n_symptoms_diarrhoea, n_symptoms_muscle_aches_joint_pain, n_symptoms_runny_nose,
                             
                             n_symptoms_sore_throat,n_symptoms_shortness_of_breath,
                             n_symptoms_vomiting_nausea, n_symptoms_altered_consciousness_confusion), na.rm = T))%>%
    mutate(WHO_yes_2 = ifelse(case_def2>2, 1, 0))%>%
    filter(!(is.na(WHO_yes_1) & WHO_yes_2 == 0))%>%
    
    mutate(WHO_yes = ifelse(WHO_yes_1== 1 | WHO_yes_2 == 1, 1, 0))%>%
    group_by(WHO_yes, slider_agegp10)%>%
    mutate(indicator = 1)%>%
    mutate(total = sum(indicator))%>%
    select(WHO_yes, total, slider_agegp10)%>%
    unique()%>%
    pivot_wider(names_from = WHO_yes,  values_from = total)%>%
    mutate(Yes_WHO = `1`)%>%
    mutate(No_WHO = `0`)%>%
    mutate(percentage_met_WHO = round(Yes_WHO/(No_WHO+Yes_WHO)*100,2))%>%
    select(slider_agegp10, Yes_WHO,No_WHO, percentage_met_WHO )
  
  #################################################################################################################
  # • Centers for Disease Control (CDC), United States:
  #   1. At least two of: fever, chills (not available), rigors (not available), myalgia, headache, sore throat,
  # new olfactory and taste disorder,
  # Or
  # 2. At least one of: cough, shortness of breath, difficulty breathing (not available)
  #################################################################################################################
  
  #CDC
  CDC_case_def <- prep %>%
    select(symptoms_cough, symptoms_history_of_fever, symptoms_muscle_aches_joint_pain, symptoms_headache,
           symptoms_sore_throat,symptoms_shortness_of_breath, symptoms_lost_altered_sense_of_taste, symptoms_lost_altered_sense_of_smell,
           slider_agegp10)%>%
    #Combined taste and smell
    mutate(smell_or_taste = ifelse(symptoms_lost_altered_sense_of_taste == TRUE |  symptoms_lost_altered_sense_of_smell == TRUE, TRUE, FALSE))%>%
    
    mutate(n_symptoms_cough = ifelse(symptoms_cough == TRUE,1,0))%>%
    mutate(n_symptoms_history_of_fever = ifelse(symptoms_history_of_fever == TRUE,1,0))%>%
    mutate(n_symptoms_muscle_aches_joint_pain = ifelse(symptoms_muscle_aches_joint_pain == TRUE,1,0))%>%
    mutate(n_symptoms_headache = ifelse(symptoms_headache == TRUE,1,0))%>%
    mutate(n_symptoms_sore_throat = ifelse(symptoms_sore_throat == TRUE,1,0))%>%
    mutate(n_smell_or_taste = ifelse(smell_or_taste == TRUE,1,0))%>%
    mutate(n_symptoms_shortness_of_breath = ifelse(symptoms_shortness_of_breath == TRUE,1,0))%>%
    
    mutate(case_def_1 =n_symptoms_history_of_fever +n_symptoms_headache+n_symptoms_sore_throat+
             n_symptoms_shortness_of_breath +n_smell_or_taste +n_symptoms_muscle_aches_joint_pain)%>%
    mutate(case_def_2 = n_symptoms_cough + n_symptoms_shortness_of_breath)%>%
    
    #These are all that meet but ignores that some might have missing on a bunch of these sympt but still meet if 3 are reported yes
    mutate(CDC_1_def = ifelse(case_def_1>1, 1, 0))%>%
    mutate(CDC_2_def = ifelse(case_def_2>0, 1, 0))%>%
    
    rowwise() %>% 
    mutate(case_def_1_sum = sum(c(n_symptoms_history_of_fever,n_symptoms_headache , n_symptoms_sore_throat,n_symptoms_muscle_aches_joint_pain,
                                  n_symptoms_shortness_of_breath ,n_smell_or_taste), na.rm = T))%>%
    
    mutate(case_def_2_sum = sum(c(n_symptoms_cough , n_symptoms_shortness_of_breath), na.rm = T))%>%
    
    mutate(CDC_yes_1 = ifelse(case_def_1_sum>1, 1, 0))%>%
    mutate(CDC_yes_2 = ifelse(case_def_2_sum>0, 1, 0))%>%
    
    filter(!((is.na(CDC_1_def) & CDC_yes_1 == 0) | (is.na(CDC_2_def) & CDC_yes_2 == 0)))%>%
    
    mutate(CDC_yes = ifelse(CDC_1_def== 1 | CDC_2_def == 1 | CDC_yes_1== 1 | CDC_yes_2 == 1, 1, 0))%>%
    group_by(CDC_yes, slider_agegp10)%>%
    mutate(indicator = 1)%>%
    mutate(total = sum(indicator))%>%
    select(CDC_yes, total, slider_agegp10)%>%
    unique()%>%
    pivot_wider(names_from = CDC_yes,  values_from = total)%>%
    mutate(Yes_CDC = `1`)%>%
    mutate(No_CDC = `0`)%>%
    mutate(percentage_met_CDC = round(Yes_CDC/(No_CDC+Yes_CDC)*100,2))%>%
    select(slider_agegp10, Yes_CDC,No_CDC, percentage_met_CDC )
  
  
  
  #Bind them all together
  case_def_final_1 <- left_join(ECDC_case_def, CDC_case_def, by="slider_agegp10")
  case_def_final_2 <- left_join(case_def_final_1, WHO_case_def, by="slider_agegp10")
  case_def_final <- left_join(case_def_final_2, PHE_case_def, by="slider_agegp10")
  # 
  # case_def_final_print <- case_def_final %>%
  #   filter(!is.na(slider_agegp10))%>%
  #   arrange(slider_agegp10)%>%
  #   mutate(slider_agegp10 =as.character(slider_agegp10))
  # 
  # 
  # overal_who <- round(sum(case_def_final_print$Yes_WHO)/(sum(case_def_final_print$Yes_WHO)+sum(case_def_final_print$No_WHO))*100,2)
  # overal_ECDC <- round(sum(case_def_final_print$Yes_ECDC)/(sum(case_def_final_print$Yes_ECDC)+sum(case_def_final_print$No_ECDC))*100,2)
  # overal_PHE <- round(sum(case_def_final_print$Yes_PHE)/(sum(case_def_final_print$Yes_PHE)+sum(case_def_final_print$No_PHE))*100,2)
  # overal_CDC <- round(sum(case_def_final_print$Yes_CDC)/(sum(case_def_final_print$Yes_CDC)+sum(case_def_final_print$No_CDC))*100,2)
  # 
  
  case_def_final <- case_def_final %>%
    filter(!is.na(slider_agegp10))%>%
    arrange(slider_agegp10)%>%
    mutate(slider_agegp10 =as.character(slider_agegp10)) %>% 
    select(slider_agegp10,starts_with("percentage")) %>% 
    pivot_longer(starts_with("percentage"),names_to = "symptom",values_to = "proportion") %>% 
    mutate(symptom=gsub("percentage_met_","",symptom),
           proportion=proportion/100)
  
  case_def_final
  
  
  
   
}

#' Aggregate data for hospital stay plot by sex
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the hospital stay plot by sex
#' @export length.of.stay.sex.prep
length.of.stay.sex.prep <- function(input.tbl){
  
  input.tbl %>%
    mutate(dur_ho=as.numeric(dur_ho))%>%
    mutate(dur_ho=case_when(dur_ho>89~NA_real_,
                            TRUE~dur_ho))%>%
    lazy_dt(immutable = TRUE) %>%
    #filter(embargo_length!=TRUE & cov_det_id=="POSITIVE") %>% 
    mutate(length.of.stay=dur_ho) %>% 
    select(slider_sex,length.of.stay) %>%
    mutate(sex=slider_sex) %>% 
    mutate(sex=factor(sex,levels = c("Male", "Female")))  %>%  
    filter(!is.na(length.of.stay)) %>% 
    filter(!is.na(sex)) %>% 
    as_tibble() 
}


#' Aggregate data for hospital stay plot by age 
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the hospital stay plot by age
#' @export length.of.stay.age.prep
length.of.stay.age.prep <- function(input.tbl){
  
  input.tbl %>%
    mutate(dur_ho=as.numeric(dur_ho))%>%
    mutate(dur_ho=case_when(dur_ho>89~NA_real_,
                            TRUE~dur_ho))%>%
    lazy_dt(immutable = TRUE) %>%
    #filter(embargo_length!=TRUE & cov_det_id=="POSITIVE") %>% 
    mutate(length.of.stay=dur_ho) %>% 
    select(slider_agegp10,length.of.stay) %>%
    mutate(agegp10=as.character(slider_agegp10)) %>% 
    mutate(agegp10=ifelse(agegp10 %in% c("70-79","80-89","90+"), "70+", agegp10)) %>% 
    filter(!is.na(length.of.stay)) %>% 
    filter(!is.na(agegp10)) %>% 
    as_tibble() 
}

#' Aggregate data for hospital admission to ICU admission
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the hospital admission to ICU plot
#' @export admission.to.icu
admission.to.icu.prep <- function(input.tbl){
  
  input.tbl %>%
    mutate(t_ad_icu=as.numeric(t_ad_icu))%>%
    mutate(t_ad_icu=case_when(t_ad_icu>89~NA_real_,
                              TRUE~t_ad_icu))%>%
    lazy_dt(immutable = TRUE) %>%
    mutate(admission.to.icu=t_ad_icu) %>% 
    select( admission.to.icu) %>%
    filter(!is.na(admission.to.icu)) %>% 
    filter(admission.to.icu >= 0) %>% 
    as_tibble() 
}

#' Aggregate data for timeline plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the timeline plot
#' @export status.by.time.after.admission
status.by.time.after.admission.prep <- function(input.tbl){
  
  timings.wrangle <- input.tbl %>% 
    filter(!is.na(date_start)) %>% 
    select(usubjid, date_start, icu_in, icu_out, dur_icu, dur_ho, t_ad_icu, date_outcome, date_last, slider_outcome) %>% 
    mutate(subjid=usubjid,
           final.status= ifelse(slider_outcome %in% c("LTFU","Ongoing care"), "unknown", slider_outcome),
           hospital.start = 0,
           hospital.end=date_outcome-date_start,
           ICU.start = icu_in-date_start,
           ICU.end= icu_out-date_start, 
           last_date=date_last-date_start) %>% 
    filter(icu_in>=date_start|is.na(icu_in)) %>% 
    filter(hospital.end >= 0 | is.na(hospital.end))  %>%
    mutate(ever.ICU = !is.na(ICU.start)) %>%
    # If hospital end is known but ICU end is not, impossible to resolve
    filter(!(!is.na(hospital.end) & is.na(ICU.end) & ever.ICU)) %>%
    mutate(last.date = pmax(hospital.end, ICU.end, last_date, na.rm = T))%>%
    filter(last.date>=0) 
  
  overall.start <- 0
  overall.end <- quantile(timings.wrangle$hospital.end, 0.975, na.rm = T)
  
  # this generates a table of the status of every patient on every day
  complete.timeline <- map(1:nrow(timings.wrangle), function(pat.no){  
    times <- map(overall.start:overall.end, function(day){
      if(!timings.wrangle$ever.ICU[pat.no]){  #no icu
        if(is.na(timings.wrangle$hospital.end[pat.no])){
          # this happens with an exit code but no exit date. We don't know what happened after admission
          "unknown"
        } else {
          if(day <= timings.wrangle$hospital.end[pat.no]){
            "Ward"
          } else {
            as.character(timings.wrangle$final.status[pat.no])
          }
        }
      } else {  #had icu
        if(is.na(timings.wrangle$hospital.end[pat.no])){
          # this happens with an exit code but no exit date. We don't know what happened after admission
          if(day <= timings.wrangle$ICU.start[pat.no]){
            "Ward"
          } else if(!is.na(timings.wrangle$ICU.end[pat.no]) & day <= timings.wrangle$ICU.end[pat.no]){
            "ICU"
          } else {
            "unknown"
          }
        }else {
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
    dplyr::mutate(status = factor(status, levels = c("Discharge", "unknown", "Ward", "ICU", "Death"))) %>%
    ungroup() 
  
  
  final_dt <- complete.timeline.2 %>% 
    group_by(day,
             status) %>%
    summarise(count = n()) %>%
    as_tibble()
  
  final_dt
}



##################################################################
region_data <- read_csv('region.definitions.csv')

# Modifying the country names in the data I used for regions to match ISARIC data
region_data[which(region_data$whoname == "United States of America"),]$whoname <- "United States"
region_data[which(region_data$whoname == "Bolivia (Plurinational State of)"),]$whoname <- "Bolivia"
region_data[which(region_data$whoname == "Lao People's Democratic Republic"),]$whoname <- "Lao PDR"
region_data[which(region_data$whoname == "Viet Nam"),]$whoname <- "Vietnam"
region_data[which(region_data$whoname == "Republic of Korea"),]$whoname <- "Korea, Republic of"
gib <- data.frame(iso3="GIB",whoname="Gibraltar",wb_region="Europe_and_Central_Asia",wb_income="")

region_data <- rbind(region_data,gib)

number_by_region <- function(input.tbl){
  
  ### Figure 1
  data_country <- patient.by.country.map.prep(input.tbl)
  
  data_country <- left_join(data_country, region_data, by = c("slider_country" = "whoname"))
  
  data_country[which(data_country$slider_country == "Taiwan"),]$wb_region <- "East_Asia_and_Pacific"
  data_country[which(data_country$slider_country == "Hong Kong"),]$wb_region <- "East_Asia_and_Pacific"
  data_country[which(data_country$slider_country == "Gibraltar"),]$wb_region <- "Europe_and_Central_Asia"
  
  data_country$new_region <- recode(data_country$wb_region, East_Asia_and_Pacific = "East Asia and Pacific", 
                                    Europe_and_Central_Asia = "Europe and Central Asia",
                                    Middle_East_and_North_Africa = "Middle East and North Africa", 
                                    North_America = "North America", 
                                    South_Asia = "South Asia", 
                                    Latin_America_and_Caribbean = "Latin America and Caribbean",
                                    "Sub-Saharan_Africa" = "Sub-Saharan Africa")
  
  
  # This is to create the x-axis order -- sorting regions, within-regions freq, 
  # and creating space between regions (there is probably a much faster way of doing this)
  data_country <- data_country %>% arrange(wb_region, Freq) %>% mutate(order_var = 1) %>% mutate(order_var = cumsum(order_var))
  data_country <- data_country %>% mutate(previous = lag(wb_region, default = NA)) %>% 
    mutate(previous = wb_region != previous) %>%
    mutate(previous = replace(previous, is.na(previous), 0)) %>%
    mutate(previous = cumsum(previous*2)) %>%
    mutate(x_axis = order_var + previous)
  
  return(data_country)
  
}


patient.by.country_date.map.prep <- function(input.tbl){
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_country, slider_monthyear) %>%
    filter(!is.na(slider_country)) %>%
    filter(!is.na(slider_monthyear)) %>%
    mutate(Freq = 1) %>%
    group_by(slider_country, slider_monthyear)%>%
    mutate(Freq = sum(Freq))%>%
    distinct()%>%
    as_tibble() 
}


month_by_region <- function(input.tbl){
  data_country_date <- patient.by.country_date.map.prep(input.tbl)
  
  data_country_date <- left_join(data_country_date, region_data, by = c("slider_country" = "whoname"))
  
  data_country_date[which(data_country_date$slider_country == "Taiwan"),]$wb_region <- "East_Asia_and_Pacific"
  data_country_date[which(data_country_date$slider_country == "Hong Kong"),]$wb_region <- "East_Asia_and_Pacific"
  data_country_date[which(data_country_date$slider_country == "Gibraltar"),]$wb_region <- "Europe_and_Central_Asia"
  
  data_country_date$new_region <- recode(data_country_date$wb_region, East_Asia_and_Pacific = "East Asia and Pacific", 
                                         Europe_and_Central_Asia = "Europe and Central Asia",
                                         Middle_East_and_North_Africa = "Middle East and North Africa", 
                                         North_America = "North America", 
                                         South_Asia = "South Asia", 
                                         Latin_America_and_Caribbean = "Latin America and Caribbean",
                                         "Sub-Saharan_Africa" = "Sub-Saharan Africa")
  
  data_country_date <- data_country_date %>% 
    separate(slider_monthyear, c("month_adm", "year_adm"), sep="-") %>% 
    arrange(year_adm, month_adm, new_region)
  
  summary_country_date <-  data_country_date %>%
    group_by(year_adm, month_adm, new_region) %>%
    summarise(sum_records = sum(Freq)) %>%
    filter(year_adm>2019)
  
  
  summary_country_date$time_id <- as.numeric(summary_country_date$month_adm)
  summary_country_date$time_id <- if_else(summary_country_date$year_adm == "2020",summary_country_date$time_id, summary_country_date$time_id + 12 )
  summary_country_date$new_month <- recode(summary_country_date$month_adm,  "01" = "Jan", 
                                           "02" = "Feb",
                                           "03" = "Mar", 
                                           "04" = "Apr",
                                           "05" = "May", 
                                           "06" = "Jun", 
                                           "07" = "Jul", 
                                           "08" = "Aug", 
                                           "09" = "Sep", 
                                           "10" = "Oct", 
                                           "11" = "Nov", 
                                           "12" = "Dec")
  
  return(summary_country_date)
}

data_com_sym <- function(data_1, data_2, comorb_symp) {
  ## data_1 is the actual data
  ## data_2 is the dataset with regions
  ## comorb_symp = "comorb"for comorbidities and 
  ## comorb_symp = "symptoms"for symptooms and 
  
  if (comorb_symp=="comorb"){
    data_1=data_1 %>%
      lazy_dt(immutable = TRUE) %>%
      select(slider_country, slider_monthyear, any_of(starts_with(comorb_symp))) %>%
      as_tibble() %>%
      mutate(comorbid_other=ifelse(comorbid_other==T| comorbid_other_comorbidities==T&is.na(comorbid_other_comorbidities)==F, T, comorbid_other)) %>% 
      select(-comorbid_other_comorbidities) %>% 
      left_join(., data_2, by = c("slider_country" = "whoname")) %>%
      
      pivot_longer(any_of(starts_with(comorb_symp)), names_to = "outcome", values_to = "present") %>%
      lazy_dt(immutable = TRUE) %>%
      group_by(wb_region, outcome) %>% 
      summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present)), times.total = n())%>% 
      
      as.data.frame() 
  }else{
    data_1=data_1 %>%
      lazy_dt(immutable = TRUE) %>%
      select(slider_country, slider_monthyear, any_of(starts_with(comorb_symp))) %>%
      as_tibble() %>% 
      
      left_join(., data_2, by = c("slider_country" = "whoname")) %>%
      
      pivot_longer(any_of(starts_with(comorb_symp)), names_to = "outcome", values_to = "present") %>%
      lazy_dt(immutable = TRUE) %>%
      group_by(wb_region, outcome) %>% 
      summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present)), times.total = n())%>% 
      
      as.data.frame() 
  }

  
  nice.comorbidity.mapper <- tibble(comorbidity = unique(comorbidity.prevalence.input$comorbidity)) %>%
    mutate(nice.comorbidity = map_chr(comorbidity, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      #temp2
    })) %>%
    mutate(nice.comorbidity = case_when(nice.comorbidity=="Aids hiv" ~ "HIV/AIDS",
                                        TRUE ~ nice.comorbidity))%>%
    as.data.frame()
  
  nice.symptom.mapper <- tibble(symptom = unique(symptom.prevalence.input$symptom)) %>%
    mutate(nice.symptom = map_chr(symptom, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    })) %>%
    mutate(nice.symptom = case_when(nice.symptom=="Altered consciousness confusion" ~ "Altered consciousness/confusion",
                                    nice.symptom=="Fatigue malaise" ~ "Fatigue/malaise",
                                    nice.symptom=="Vomiting nausea"~ "Vomiting/nausea",
                                    nice.symptom=="Lost altered sense of smell"~ "Lost/altered sense of smell",
                                    nice.symptom=="Lost altered sense of taste"~ "Lost/altered sense of taste",
                                    TRUE ~ nice.symptom))
  
  
  data_1=data_1 %>% 
    left_join(nice.symptom.mapper, by=c("outcome" = "symptom")) %>%
    left_join(nice.comorbidity.mapper, by=c("outcome" = "comorbidity"))
  
}


plot_by_region <- function(input.tbl,com_sym = "comorb",current_region ='Latin_America_and_Caribbean'){
  
  data_outcome <- data_com_sym(input.tbl, region_data, com_sym)
  
  current_title <- str_replace_all(current_region, '_', ' ')
  
  data_plot <- data_outcome %>%
    filter(wb_region == current_region) %>%
    mutate(freq_outcome = 100*times.present/times.recorded) %>%
    arrange(freq_outcome) %>%
    filter(times.recorded>0) %>%
    mutate(order_var = 1:length(freq_outcome)) %>%
    mutate(alpha_outcome = if_else(freq_outcome<50,freq_outcome/50,1))
  
  if(com_sym=="comorb"){
    data_plot <- data_plot %>% 
      rename(outcome_names=nice.comorbidity) 

  }else{
    data_plot <- data_plot %>% 
      rename(outcome_names=nice.symptom)
  }
  
}
