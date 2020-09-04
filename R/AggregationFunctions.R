
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
    rename(outcome = outcome.3) %>%
    mutate(agegp10 = cut(age, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120))) %>%
    mutate(calendar.year.admit = year(date_admit)) %>%
    mutate(calendar.month.admit = month(date_admit)) %>%
    mutate(year.admit = map_dbl(date_admit, epiweek.year)) %>%
    mutate(epiweek.admit = epiweek(date_admit)) %>%
    mutate(year.epiweek.admit = glue("{year.admit}-{epiweek.admit}", .envir = .SD)) %>%
    mutate(year.epiweek.admit = replace(year.epiweek.admit, year.epiweek.admit == "NA-NA", NA)) %>%
    mutate(lower.age.bound  = map_dbl(agegp10, extract.age.boundaries, TRUE)) %>%
    mutate(upper.age.bound  = map_dbl(agegp10, extract.age.boundaries, FALSE)) %>%
    mutate(agegp10t = fct_relabel(agegp10, prettify.age.labels)) %>%
    select(-agegp10) %>%
    rename(agegp10 = agegp10t) %>%
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
    select(sex, agegp10, country, calendar.year.admit, calendar.month.admit, outcome, lower.age.bound, upper.age.bound, icu_ever) %>%
    group_by(sex, outcome, country, calendar.year.admit, calendar.month.admit, agegp10, lower.age.bound, upper.age.bound, icu_ever) %>%
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
  

  input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    filter(!is.na(year.epiweek.admit) & !is.na(outcome)) %>%
    select(sex, agegp10, lower.age.bound, upper.age.bound, country, calendar.year.admit, calendar.month.admit, year.epiweek.admit, outcome, icu_ever) %>%
    group_by(sex, outcome, country, calendar.year.admit, calendar.month.admit, year.epiweek.admit, agegp10, lower.age.bound, upper.age.bound, icu_ever) %>%
    summarise(count = n()) %>%
    as_tibble() %>%
    complete(sex, nesting(agegp10, lower.age.bound, upper.age.bound), country, nesting(calendar.year.admit, calendar.month.admit, year.epiweek.admit), outcome, icu_ever, fill = list(count = 0)) %>%
    arrange(year.epiweek.admit) %>%
    group_by(sex, outcome, country, agegp10) %>%
    mutate(cum.count = cumsum(count)) %>%
    left_join(age.bound.lookup, by="agegp10")
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
    select(sex, agegp10, country, calendar.year.admit, calendar.month.admit, outcome, icu_ever, any_of(starts_with("symptoms")), lower.age.bound, upper.age.bound) %>%
    select(-`symptoms_covid-19_symptoms`) %>%
    as.data.table() %>%
    pivot_longer(starts_with("symptoms"), names_to = "symptom", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(sex, agegp10, country, calendar.year.admit, calendar.month.admit, outcome, symptom, lower.age.bound, upper.age.bound, icu_ever) %>%
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
    select(sex, agegp10, country, calendar.year.admit, calendar.month.admit, outcome, icu_ever, any_of(starts_with("comorb")), lower.age.bound, upper.age.bound) %>%
    as.data.table() %>%
    pivot_longer(any_of(starts_with("comorb")), names_to = "comorbidity", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(sex, agegp10, country, calendar.year.admit, calendar.month.admit, outcome, comorbidity, lower.age.bound, upper.age.bound, icu_ever) %>%
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
    select(sex, agegp10, country, calendar.year.admit, calendar.month.admit, outcome, icu_ever, any_of(starts_with("treat")), lower.age.bound, upper.age.bound) %>%
    as.data.table() %>%
    pivot_longer(any_of(starts_with("treat")), names_to = "treatment", values_to = "present") %>%
    lazy_dt(immutable = TRUE) %>%
    group_by(sex, agegp10, country, calendar.year.admit, calendar.month.admit, outcome, treatment, lower.age.bound, upper.age.bound, icu_ever) %>%
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
  
  treatment.use.proportion.prep(input.tbl %>% filter(icu_ever))
  
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


