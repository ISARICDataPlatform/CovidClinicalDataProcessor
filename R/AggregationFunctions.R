#load packages (some of these are not being used but we can delete them later)
library(RColorBrewer)
library(incidence)
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
library(MASS)
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
library(binom)
library(flextable)
library(knitr)
#' Preprocessing step for all aggregations. Currently: remaps outcome to death, discharge or NA, cuts age into 5-year age groups, and adds a year-epiweek column
#' @param input.tbl Input tibble (output of \code{process.all.data})
#' @import dtplyr dplyr purrr lubridate tibble
#' @importFrom glue glue
#' @return A \code{tibble} intended for input into other aggregation functions (e.g. \code{age.pyramid.prep})
#' @export data.preprocessing
data.preprocessing <- function(input.tbl){
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    mutate(outcome.3 = map2_chr(outcome, date_outcome, outcome.remap)) %>%
    select(-outcome) %>%
    rename(slider_outcome = outcome.3) %>%
    mutate(agegp10 = cut(age, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120))) %>%
    mutate(calendar.year.admit = year(date_admit)) %>%
    mutate(calendar.month.admit = month(date_admit)) %>%
    mutate(slider_monthyear = map2_chr(calendar.year.admit, calendar.month.admit, month.year.mapper)) %>%
    mutate(year.admit = map_dbl(date_admit, epiweek.year)) %>%
    mutate(epiweek.admit = epiweek(date_admit)) %>%
    mutate(year.epiweek.admit = glue("{year.admit}-{epiweek.admit}", .envir = .SD)) %>%
    mutate(year.epiweek.admit = replace(year.epiweek.admit, year.epiweek.admit == "NA-NA", NA)) %>%
    mutate(lower.age.bound  = map_dbl(agegp10, extract.age.boundaries, TRUE)) %>%
    mutate(upper.age.bound  = map_dbl(agegp10, extract.age.boundaries, FALSE)) %>%
    mutate(slider_agegp10 = fct_relabel(agegp10, prettify.age.labels)) %>%
    select(-agegp10) %>%
    rename(slider_icu_ever = icu_ever) %>%
    rename(slider_country = country) %>%
    rename(slider_sex = sex) %>%
    as_tibble()
}




#' Aggregate data for age pyramid plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export age.pyramid.prep
age.pyramid.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
    group_by(slider_sex, slider_outcome, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_agegp10, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
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
  
  epiweek.order <- glue("{c(rep(2019,4), rep(2020,max(input.tbl$epiweek.admit[which(input.tbl$year.admit == 2020)], na.rm = T)))}-{c(49:52, 1:max(input.tbl$epiweek.admit[which(input.tbl$year.admit == 2020)], na.rm = T))}")
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    mutate(year.epiweek.admit = factor(year.epiweek.admit, levels = epiweek.order)) %>%
    filter(!is.na(year.epiweek.admit) & !is.na(slider_outcome)) %>%
    select(slider_sex, slider_agegp10, lower.age.bound, upper.age.bound, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, year.epiweek.admit, slider_outcome, slider_icu_ever) %>%
    group_by(slider_sex, slider_outcome, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, year.epiweek.admit, slider_agegp10, lower.age.bound, upper.age.bound, slider_icu_ever) %>%
    summarise(count = n()) %>%
    as_tibble() 
}


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
    #dplyr::select(-`symptoms_covid-19_symptoms`) %>% #commented out bc this column doesn't exist
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
    summarise(times.present = sum(present, na.rm = TRUE), times.recorded = sum(!is.na(present))) %>%
    as_tibble()
  
  nice.comorbidity.mapper <- tibble(comorbidity = unique(comorbidity.prevalence.input$comorbidity)) %>%
    mutate(nice.comorbidity = map_chr(comorbidity, function(st){
      temp <- substr(st, 10, nchar(st)) %>% str_replace_all("_", " ")
      temp2 <- glue("{toupper(substr(temp, 1, 1))}{substr(temp, 2, nchar(temp))}")
      temp2
    })) %>%
    mutate(nice.comorbidity = case_when(comorbidity=="Aids hiv" ~ "HIV/AIDS",
                                        TRUE ~ nice.comorbidity))
  
  comorbidity.prevalence.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.comorbidity.mapper) %>%
    as_tibble() 
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
    }))
  
  treatment.use.proportion.input %>%
    lazy_dt(immutable = TRUE) %>%
    left_join(nice.treatment.mapper) %>%
    as_tibble()
}


#' Aggregate data for ICU treatment use proportion plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the ICU treatment use proportion plot
#' @export icu.treatment.use.proportion.prep
icu.treatment.use.proportion.prep <- function(input.tbl){
  
  treatment.use.proportion.prep(input.tbl %>% filter(slider_icu_ever))
  
}

#' @keywords internal
#' @export prettify.age.labels
prettify.age.labels <- function(a){
  temp <- substr(a, 2, nchar(a) - 1)
  newlabels <- map_chr(temp, function(x) {
    components <- as.numeric(str_split_fixed(x, ",", Inf))
    components[2] <- components[2] - 1
    paste(components, collapse = "-")
  })
  str_replace(newlabels, "90-119", "90+")
}

#' @keywords internal
#' @export extract.age.boundaries
extract.age.boundaries <- function(agestring, lower = TRUE){
  agestring <- as.character(agestring)
  temp <- substr(agestring, 2, nchar(agestring)-1)
  if(lower){
    as.numeric(str_split_fixed(temp, ",", Inf)[1])
  } else {
    as.numeric(str_split_fixed(temp, ",", Inf)[2]) - 1
  }
}

#' @keywords internal
#' @export epiweek.year
epiweek.year <- function(date){
  if(is.na(date)){
    return(NA)
  }
  if(year(date)==2019 & date > ymd("2019-12-28")){
    2020
  } else {
    year(date)
  }
}

#' @keywords internal
#' @export outcome.remap
outcome.remap <- function(oc, od){
  if(is.na(od) & is.na(oc)){
    "censored"
  } else {
    out <- case_when(is.na(oc) ~ NA_character_,
                     oc == "Death" ~ "death",
                     oc == "Discharged Alive" ~ "discharge")
  }
}

#' @keywords internal
#' @export month.year.mapper
month.year.mapper <- function(y,m){
  if(any(is.na(c(y,m)))){
    NA
  } else if(m<10){
    glue("0{m}-{y}")
  } else {
    glue("{m}-{y}")
  }
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
    slice(1:max.symptoms) %>%
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
  
  n.comorb <- ncol(data2) - 1
  
  data2 <- data2 %>%
    pivot_longer(2:(n.comorb+1), names_to = "Treatment", values_to = "Present") %>%
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


#' Aggregate data for ICU treatments upset plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @param max.treatments The plot will display only the n most common treatments, this parameter is n
#' @import dplyr purrr tidyr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the ICU treatments upset plot
#' @export icu.treatment.upset.prep
icu.treatment.upset.prep <- function(input.tbl, max.treatments = 5){
  treatment.upset.prep(input.tbl %>% filter(slider_icu_ever), max.treatments)
  
  
}
############################################
#' @export plot.prop.by.age
#' @keywords internal
############################################
plot.prop.by.age <- function(data, var, name, ymax = 1, sz = 750, ...) {
  data2 <- data
  summ <- data2 %>%
    add_column(All = 1)%>%
    add_column(a = var) %>%
    filter(!is.na(a)) %>%
    group_by(slider_agegp10) %>%
    dplyr::summarise(
      All = sum(All, na.rm = TRUE),
      v = sum(a, na.rm = TRUE)
    )
  d <- binom.confint(summ$v, summ$All, conf.level = .95, method = "exact")
  d$X <- summ$slider_agegp10
  d$lbl <- paste(d$x, d$n, sep = "/\n", collapse = NULL)
  censored.lbl <- paste("-", d$n, sep = "/\n", collapse = NULL)
  d$lbl[d$x <= 5] <- censored.lbl[d$x <= 5]
  d$size <- d$n / sz
  #xlabs <- c(
  #  "<10",
   # "10-",
  #  "20-",
   # "30-",
  #  "40-",
  #  "50-",
  #  "60-",
  #  "70-",
  #  "80-",
  #  expression(phantom(x) >= 90)
  #)
  N <- paste("N = ", sum(summ$All), sep = "", collapse = NULL)
  pts <- geom_point(
    data = d,
    aes(x = d$X, y = mean),
    shape = "square",
    size = d$size,
    colour = "navy"
  )
  lines <- geom_linerange(
    data = d,
    aes(x = X, ymin = lower, ymax = upper),
    colour = "#000000",
    show.legend = FALSE
  )
  xa <- scale_x_discrete(
    name = "Age group (years)"#,
    #labels = xlabs
  )
  ya <- scale_y_continuous(
    name = name,
    limits = c(0, ymax)
  )
  #  lbls <- geom_text(
  #    data = d,
  #    aes(x = X, y = ymax, label = lbl),
  #    size = 2
  #  )
  p <- ggplot() +
    pts +
    lines +
    #    lbls +
    xa + ya +
    theme_bw() + theme(axis.text = element_text(size = 6)) +
    labs(title = N)
  
  return(p)
  
}

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
                 "Wheezing",
                 "Cough",
                 "Chest pain",
                 "Lymphadenopathy",
                 "Loss of taste",
                 "Loss of smell",
                 "Conjunctivitis",
                 "Bleeding",
                 "Skin ulcers",
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
  
#'Create the function that makes the heatmap plot
#'
heatmap_plot <- function(data_plot_heatmap){
  heatmap_plot <- ggplot(data_plot_heatmap) +
    geom_tile(aes(x=label.x, y=label.y, fill=phi.correlation)) +
    scale_fill_gradient2(low = "deepskyblue3", mid = "white", high = "indianred3",
                         name = "phi coefficient", limits = c(-1,1)) +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          text = element_text(size=9),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    coord_fixed()
}







#' vital signs
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @return A \code{tibble} containing the input data for the vital sign tables plot

#vs_resp
func_plots_vs_resp <- function(input.tbl){
  data_plot_vs_resp <- select(input.tbl, c(starts_with("slider"),vs_resp, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#vs_hr
func_plots_vs_hr <- function(input.tbl){
  data_plot_vs_hr <- select(input.tbl, c(starts_with("slider"),vs_hr, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#vs_temp
func_plots_vs_temp <- function(input.tbl){
  data_plot_vs_temp <- select(input.tbl, c(starts_with("slider"),vs_temp, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#vs_sysbp
func_plots_vs_sysbp <- function(input.tbl){
  data_plot_vs_sysbp <- select(input.tbl, c(starts_with("slider"),vs_sysbp, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}
#vs_oxysat
func_plots_vs_oxysat <- function(input.tbl){
  data_plot_vs_oxysat <- select(input.tbl, c(starts_with("slider"),vs_oxysat, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("vs"), names_to = "symptom", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
  
}




#' Box and whisker plots for laboratory results at hospital presentation stratified by age group.
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @return A \code{tibble} containing the input data for the lab data

#crp
func_plot_lab_crp <- function(input.tbl){
  data_plot_lab_crp <- select(input.tbl, c(starts_with("slider"),lab_crp, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_lym
func_plot_lab_lym <- function(input.tbl){
  data_plot_lab_lym <- select(input.tbl, c(starts_with("slider"),lab_lym, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_neut
func_plot_lab_neut <- function(input.tbl){
  data_plot_lab_neut <- select(input.tbl, c(starts_with("slider"),lab_neut, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame() 
}

#lab_wbc
func_plot_lab_crp <- function(input.tbl){
  data_plot_lab_wbc <- select(input.tbl, c(starts_with("slider"),lab_wbc, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value)) %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_urean
func_plot_lab_urean  <- function(input.tbl){
  data_plot_lab_urean <- select(input.tbl, c(starts_with("slider"),lab_urean, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_pt
func_plot_lab_pt <- function(input.tbl){
  data_plot_lab_pt <- select(input.tbl, c(starts_with("slider"),lab_pt, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame() 
}

#lab_alt
func_plot_lab_alt <- function(input.tbl){
  data_plot_lab_alt <- select(input.tbl, c(starts_with("slider"),lab_alt, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_aptt
func_plot_lab_aptt <- function(input.tbl){
  data_plot_lab_aptt <- select(input.tbl, c(starts_with("slider"),lab_aptt, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}


#lab_bili
func_plot_lab_bili <- function(input.tbl){
  data_plot_lab_bili <- select(input.tbl, c(starts_with("slider"),lab_bili, upper.age.bound, lower.age.bound)) %>%
    pivot_longer(starts_with("lab"), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))  %>%
    filter(!is.na(slider_agegp10)) %>%
    as.data.frame()
}

#lab_ast
func_plot_lab_ast <- function(input.tbl){
  data_plot_lab_ast <- select(input.tbl, c(starts_with("slider"),lab_ast, upper.age.bound, lower.age.bound)) %>%
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
  data_plot_comorbid_asthma <- select(input.tbl, c(starts_with("slider"),comorbid_asthma, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value") %>%
    as.data.frame()  
}


#comorbid_malignant_neoplasm
func_plot_comorbid_malignant_neoplasm <- function(input.tbl){
  data_plot_comorbid_malignant_neoplasm <- select(input.tbl, c(starts_with("slider"),comorbid_malignant_neoplasm, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
}


#comorbid_obesity
func_plot_comorbid_obesity <- function(input.tbl){
  data_plot_comorbid_obesity <- select(input.tbl, c(starts_with("slider"),comorbid_obesity, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
  
}

#comorbid_diabetes
func_plot_comorbid_diabetes <- function(input.tbl){
  data_plot_comorbid_diabetes <- select(input.tbl, c(starts_with("slider"),comorbid_diabetes, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
}


#comorbid_dementia
func_plot_comorbid_dementia <- function(input.tbl){
  data_plot_comorbid_dementia <- select(input.tbl, c(starts_with("slider"),comorbid_dementia, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
}

#comorbid_smoking
func_plot_comorbid_smoking <- function(input.tbl){
  data_plot_comorbid_smoking <- select(input.tbl, c(starts_with("slider"),comorbid_smoking, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value")  %>%
    as.data.frame() 
  
}

#comorbid_hypertension
func_plot_comorbid_hypertension <- function(input.tbl){
  data_plot_comorbid_hypertension <- select(input.tbl, c(starts_with("slider"),comorbid_hypertension, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("comorbid"), names_to = "comorbid", values_to = "value") %>%
    as.data.frame()  
}


#' symptoms by age
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @return A \code{tibble} containing the input data for the Comorbidities data


#symptoms_history_of_fever
func_plot_symptoms_history_of_fever <- function(input.tbl){
  data_plot_symptoms_history_of_fever <- select(input.tbl, c(starts_with("slider"),symptoms_history_of_fever, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value") %>%
    as.data.frame()   
}


#symptoms_cough
func_plot_symptoms_cough <- function(input.tbl){
  data_plot_symptoms_cough <- select(input.tbl, c(starts_with("slider"),symptoms_cough, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_cough_fever
func_plot_symptoms_cough_fever <- function(input.tbl){
  data_plot_symptoms_cough_fever <- select(input.tbl, c(starts_with("slider"),symptoms_history_of_fever,symptoms_cough,
                                                        upper.age.bound, lower.age.bound)) %>%
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
  data_plot_symptoms_shortness_of_breath <- select(input.tbl, c(starts_with("slider"),symptoms_shortness_of_breath, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_cought_fever_shortness_of_breath
func_plot_symptoms_cought_fever_shortness_of_breath <- function(input.tbl){
  data_plot_symptoms_cought_fever_shortness_of_breath <- select(input.tbl, 
                                                                c(starts_with("slider"),symptoms_cough,symptoms_history_of_fever,
                                                                  symptoms_shortness_of_breath,upper.age.bound, lower.age.bound)) %>%
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
  data_plot_symptoms_upper_respiratory_tract_symptoms <- select(input.tbl, c(starts_with("slider"),
                                                                             symptoms_sore_throat,symptoms_runny_nose,symptoms_ear_pain,
                                                                             upper.age.bound, lower.age.bound)) %>%
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
  data_plot_symptoms_altered_consciousness_confusion <- select(input.tbl, c(starts_with("slider"),symptoms_altered_consciousness_confusion, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_constitutional
func_plot_symptoms_constitutional <- function(input.tbl){
  data_plot_symptoms_constitutional <- select(input.tbl,c(starts_with("slider"),symptoms_muscle_aches_joint_pain,symptoms_fatigue_malaise,
                                                          symptoms_headache,upper.age.bound, lower.age.bound)) %>%
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
  data_plot_symptoms_vomiting_nausea <- select(input.tbl, c(starts_with("slider"),symptoms_vomiting_nausea, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value") %>%
    as.data.frame()   
}


#symptoms_diarrhoea
func_plot_symptoms_diarrhoea <- function(input.tbl){
  data_plot_symptoms_diarrhoea <- select(input.tbl, c(starts_with("slider"),symptoms_diarrhoea, upper.age.bound, lower.age.bound)) %>%
    filter(!is.na(slider_agegp10)) %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptoms", values_to = "value")  %>%
    as.data.frame()  
}


#symptoms_abdominal_pain
func_plot_symptoms_abdominal_pain <- function(input.tbl){
  data_plot_symptoms_abdominal_pain <- select(input.tbl, c(starts_with("slider"),symptoms_abdominal_pain, upper.age.bound, lower.age.bound)) %>%
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
                                      "symptoms_wheezing", 
                                      "symptoms_cough", 
                                      "symptoms_chest_pain",
                                      "symptoms_lymphadenopathy",
                                      "symptoms_loss_of_taste",
                                      "symptoms_loss_of_smell", 
                                      "symptoms_conjunctivitis",
                                      "symptoms_bleeding",  
                                      "symptoms_skin_ulcers", 
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
                                      "Wheezing",
                                      "Cough",
                                      "Chest pain",
                                      "Lymphadenopathy",
                                      "Loss of taste",
                                      "Loss of smell",
                                      "Conjunctivitis",
                                      "Bleeding",
                                      "Skin ulcers",
                                      "Skin rash",
                                      "Seizures",
                                      "Altered consciousness / confusion"))
admission.symptoms <- as_tibble(admission.symptoms)



###################################################################################
###################################################################################
#####Create tables for dashboard###################################################
###################################################################################
###################################################################################

#Import the rds file and cleaning it
base::load("Data/ISVARIC_dash_db_preprocess.rda")
import_martina  <- prepr.tbl


import_martina <- import_martina%>%
  filter((embargo_length==FALSE | is.na(embargo_length)) & cov_det_id=="POSITIVE")

######################################################
#   Saving all the tables
######################################################
#Age pyramid part
age.pyramid.input <- age.pyramid.prep(import_martina)
save(age.pyramid.input, file ="saved_rda_files/age_pyramid_input.rda")

#outcome by admision date
outcome_admission_date_input <- outcome.admission.date.prep(import_martina)
save(outcome_admission_date_input, file ="saved_rda_files/outcome_admission_date_input.rda")

#vital signs
data_plot_vs_resp <- func_plots_vs_resp(import_martina)
data_plot_vs_hr <- func_plots_vs_hr(import_martina)
data_plot_vs_temp <- func_plots_vs_temp(import_martina)
data_plot_vs_sysbp <- func_plots_vs_sysbp(import_martina)
data_plot_vs_oxysat <- func_plots_vs_oxysat(import_martina)

save(data_plot_vs_resp, file ="saved_rda_files/data_plot_vs_resp.rda")
save(data_plot_vs_hr, file ="saved_rda_files/data_plot_vs_hr.rda")
save(data_plot_vs_temp, file ="saved_rda_files/data_plot_vs_temp.rda")
save(data_plot_vs_sysbp, file ="saved_rda_files/data_plot_vs_sysbp.rda")
save(data_plot_vs_oxysat, file ="saved_rda_files/data_plot_vs_oxysat.rda")

#Lab data
data_plot_lab_crp <- func_plot_lab_crp(import_martina)
data_plot_lab_lym <- func_plot_lab_lym(import_martina)
data_plot_lab_neut <- func_plot_lab_neut(import_martina)
data_plot_lab_wbc <- func_plot_lab_crp(import_martina)
data_plot_lab_urean <- func_plot_lab_urean(import_martina)
data_plot_lab_pt <- func_plot_lab_pt(import_martina)
data_plot_lab_alt <- func_plot_lab_alt(import_martina)
data_plot_lab_aptt <- func_plot_lab_aptt(import_martina)
data_plot_lab_bili <- func_plot_lab_bili(import_martina)
data_plot_lab_ast <- func_plot_lab_ast(import_martina)

save(data_plot_lab_crp, file ="saved_rda_files/data_plot_lab_crp.rda")
save(data_plot_lab_lym, file ="saved_rda_files/data_plot_lab_lym.rda")
save(data_plot_lab_neut, file ="saved_rda_files/data_plot_lab_neut.rda")
save(data_plot_lab_wbc, file ="saved_rda_files/data_plot_lab_wbc.rda")
save(data_plot_lab_urean, file ="saved_rda_files/data_plot_lab_urean.rda")
save(data_plot_lab_pt, file ="saved_rda_files/data_plot_lab_pt.rda")
save(data_plot_lab_alt, file ="saved_rda_files/data_plot_lab_alt.rda")
save(data_plot_lab_aptt, file ="saved_rda_files/data_plot_lab_aptt.rda")
save(data_plot_lab_bili, file ="saved_rda_files/data_plot_lab_bili.rda")
save(data_plot_lab_ast, file ="saved_rda_files/data_plot_lab_ast.rda")


#Symptom plots
symptom.prevalence.input <- symptom.prevalence.prep(import_martina)
symptom.upset.input <- symptom.upset.prep(import_martina, max.symptoms = 5)

save(symptom.prevalence.input, file ="saved_rda_files/symptom_prevalence_input.rda")
save(symptom.upset.input, file ="saved_rda_files/symptom_upset_input.rda")

#Comorbidity plots by age
data_plot_comorbid_asthma <- func_plot_comorbid_asthma(import_martina)
data_plot_comorbid_malignant_neoplasm <-func_plot_comorbid_malignant_neoplasm(import_martina)
data_plot_comorbid_obesity <- func_plot_comorbid_obesity(import_martina)
data_plot_comorbid_diabetes <-func_plot_comorbid_diabetes(import_martina)
data_plot_comorbid_dementia <-func_plot_comorbid_dementia(import_martina)
data_plot_comorbid_smoking <-func_plot_comorbid_smoking(import_martina)
data_plot_comorbid_hypertension <- func_plot_comorbid_hypertension(import_martina)

save(data_plot_comorbid_asthma, file ="saved_rda_files/data_plot_comorbid_asthma.rda")
save(data_plot_comorbid_malignant_neoplasm, file ="saved_rda_files/data_plot_comorbid_malignant_neoplasm.rda")
save(data_plot_comorbid_obesity, file ="saved_rda_files/data_plot_comorbid_obesity.rda")
save(data_plot_comorbid_diabetes, file ="saved_rda_files/data_plot_comorbid_diabetes.rda")
save(data_plot_comorbid_dementia, file ="saved_rda_files/data_plot_comorbid_dementia.rda")
save(data_plot_comorbid_smoking, file ="saved_rda_files/data_plot_comorbid_smoking.rda")
save(data_plot_comorbid_hypertension, file ="saved_rda_files/data_plot_comorbid_hypertension.rda")


#Symptoms by age
data_plot_symptoms_history_of_fever <- func_plot_symptoms_history_of_fever(import_martina)
data_plot_symptoms_cough <-func_plot_symptoms_cough(import_martina)
data_plot_symptoms_cough_fever<-func_plot_symptoms_cough_fever(import_martina)
data_plot_symptoms_shortness_of_breath<-func_plot_symptoms_shortness_of_breath(import_martina)
data_plot_symptoms_cought_fever_shortness_of_breath<-func_plot_symptoms_cought_fever_shortness_of_breath(import_martina)
data_plot_symptoms_upper_respiratory_tract_symptoms<-func_plot_symptoms_upper_respiratory_tract_symptoms(import_martina)
data_plot_symptoms_altered_consciousness_confusion<-func_plot_symptoms_altered_consciousness_confusion(import_martina)
data_plot_symptoms_constitutional<-func_plot_symptoms_constitutional(import_martina)
data_plot_symptoms_vomiting_nausea<-func_plot_symptoms_vomiting_nausea
data_plot_symptoms_diarrhoea <-func_plot_symptoms_diarrhoea(import_martina)
data_plot_symptoms_abdominal_pain <- func_plot_symptoms_abdominal_pain(import_martina)

save(data_plot_symptoms_history_of_fever, file ="saved_rda_files/data_plot_symptoms_history_of_fever.rda")
save(data_plot_symptoms_cough, file ="saved_rda_files/data_plot_symptoms_cough.rda")
save(data_plot_symptoms_cough_fever, file ="saved_rda_files/data_plot_symptoms_cough_fever.rda")
save(data_plot_symptoms_shortness_of_breath, file ="saved_rda_files/data_plot_symptoms_shortness_of_breath.rda")
save(data_plot_symptoms_cought_fever_shortness_of_breath, file ="saved_rda_files/data_plot_symptoms_cought_fever_shortness_of_breath.rda")
save(data_plot_symptoms_upper_respiratory_tract_symptoms, file ="saved_rda_files/data_plot_symptoms_upper_respiratory_tract_symptoms.rda")
save(data_plot_symptoms_altered_consciousness_confusion, file ="saved_rda_files/data_plot_symptoms_altered_consciousness_confusion.rda")
save(data_plot_symptoms_constitutional, file ="saved_rda_files/data_plot_symptoms_constitutional.rda")
save(data_plot_symptoms_vomiting_nausea, file ="saved_rda_files/data_plot_symptoms_vomiting_nausea.rda")
save(data_plot_symptoms_diarrhoea, file ="saved_rda_files/data_plot_symptoms_diarrhoea.rda")
save(data_plot_symptoms_abdominal_pain, file ="saved_rda_files/data_plot_symptoms_abdominal_pain.rda")

#Heatmap
data_plot_heatmap <- symptom.heatmap(data = import_martina, admission.symptoms = admission.symptoms, asterisks = vector())

save(data_plot_heatmap, file ="saved_rda_files/data_plot_heatmap.rda")









