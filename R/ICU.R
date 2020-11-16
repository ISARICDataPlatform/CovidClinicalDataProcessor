library(tidyverse)
library(dtplyr)
library(lubridate)
library(glue)
data <- read_csv("ISVARIC_dash_db_20201110.csv")
data_pre <- read_csv("ISVARIC_dash_db_20201110_preprocess.csv")


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
    mutate(length.of.stay=date_outcome2-date_admit2) %>% 
    mutate(admission.to.icu=icu_in2-date_admit2) %>% 
    select(-agegp10) %>%
    rename(slider_icu_ever = ever_icu) %>%
    rename(slider_country = country) %>%
    rename(slider_sex = sex) %>%
    as_tibble()
}

df=data.preprocessing(data)

test <- data %>% lazy_dt(immutable = TRUE) %>%
  select(outcome, date_outcome, date_admit,date_admit2, date_outcome, date_outcome2, icu_in, icu_in2, sex) %>% 
  mutate(length.of.stay=date_outcome-date_admit) %>% 
  mutate(outcome.3 = map2_chr(outcome, date_outcome, outcome.remap)) %>% 
  as_tibble()


#' Aggregate data for hospital stay plot by sex
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export length.of.stay.sex.prep
length.of.stay.sex.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever, length.of.stay) %>%
    mutate(sex=slider_sex) %>% 
    mutate(sex=factor(sex,levels = c("Male", "Female")))  %>%  
    filter(!is.na(length.of.stay)) %>% 
    filter(!is.na(sex)) %>% 
    filter(length.of.stay > 0) %>% 
    as_tibble() 
}


#' Aggregate data for hospital stay plot by age 
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export length.of.stay.age.prep
length.of.stay.age.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever, length.of.stay) %>%
    mutate(agegp10=as.character(slider_agegp10)) %>% 
    mutate(agegp10=ifelse(agegp10 %in% c("70-79","80-89","90+"), "70+", agegp10)) %>% 
    filter(!is.na(length.of.stay)) %>% 
    filter(!is.na(agegp10)) %>% 
    filter(length.of.stay > 0) %>% 
    as_tibble() 
}

#' Aggregate data for hospital admission to ICU admission
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export admission.to.icu
admission.to.icu.prep <- function(input.tbl){
  
  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, lower.age.bound, upper.age.bound, slider_icu_ever, admission.to.icu) %>%
    filter(!is.na(admission.to.icu)) %>% 
    filter(admission.to.icu >= 0) %>% 
    as_tibble() 
}

length.of.stay.sex.input=length.of.stay.sex.prep(df)
save(length.of.stay.sex.input,file="length_of_stay_sex_input.rda")

length.of.stay.age.input=length.of.stay.age.prep(df)
save(length.of.stay.age.input,file="length_of_stay_age_input.rda")

admission.to.icu.input=admission.to.icu.prep(df)
save(admission.to.icu.input,file="admission_to_icu_input.rda")
