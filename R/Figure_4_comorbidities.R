#' Aggregate data for comorbidity prevalence plot
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dtplyr dplyr tibble purrr tidyr
#' @importFrom glue glue
#' @importFrom data.table as.data.table
#' @return A \code{tibble} containing the input data for the comorbidity prevalence plot
#' @export comorbidity.prevalence.prep

###New dataset_all in one
folder <- "C:/Users/ESCHERM/OneDrive/Documents/ISARIC/data/2020-10-25"
setwd(folder)
input.tbl<- readRDS("ISVARIC_dash_db_20201117_preprocess.rds")

save(comorbidity.prevalence.tbl, file = "comorbidity.prevalence.input.rda")
comorbidity.prevalence.tbl<-symptom.prevalence.prep(input.tbl)

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






aggregated.tbl<-symptom.prevalence.prep(input.tbl)


symptom.prevalence.prep <- function(input.tbl){
  symptom.prevalence.input <- input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(slider_sex, slider_agegp10, slider_country, calendar.year.admit, calendar.month.admit, slider_monthyear, slider_outcome, slider_icu_ever, any_of(starts_with("symptoms")), lower.age.bound, upper.age.bound) %>%
    #select(-symptoms_covid-19_symptoms) %>%
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

aggregated.tbl <- comorbidity.prevalence.input

comorbidity.prevalence.plot <- function(aggregated.tbl, ...){
  aggregated.tbl <- aggregated.tbl %>%
    group_by(comorbidity)%>% 
    summarise(present = sum(times.present, na.rm = TRUE), recorded = sum(times.recorded, na.rm = TRUE))%>% 
    mutate(label = glue("{present}/{recorded}"))%>%
    mutate(proportion=present/recorded)%>%
    mutate(affected="Yes")

  
  plt <- ggplot(aggregated.tbl, aes(x = comorbidity, y = proportion, fill = affected)) +
    geom_col(position = "stack") +
    geom_text(data = aggregated.tbl, aes(x=comorbidity, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    xlab("Comorbidity") +
    ylab("Proportion") +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = c("indianred1", "indianred4"), name = "Condition\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  
  plt
  
}

aggregated.tbl<-symptom.prevalence.prep(input.tbl)

symptom.prevalence.plot <- function(aggregated.tbl, ...){
  aggregated.tbl <- aggregated.tbl %>%
    mutate(label = glue("{times.present}/{times.recorded}"))
  plt <- ggplot(aggregated.tbl) +
    geom_col(aes(x = nice.symptom, y = proportion, fill = affected)) +
    geom_text(data = aggregated.tbl %>% filter(affected), aes(x=nice.symptom, y = 1, label = label), hjust = 1, nudge_y = -0.01, size = 2)+
    theme_bw() +
    xlab("Symptom") +
    ylab("Proportion") +
    coord_flip() +
    ylim(0, 1) +
    scale_fill_manual(values = c("deepskyblue1", "deepskyblue4"), name = "Symptom\npresent", labels = c("No", "Yes")) +
    theme(axis.text.y = element_text(size = 7))
  plt
}

This is the one I use for another project and I do get them decreasing, but I'm not sure why is not working when I add the mapping and reorder to the code above:
ggplot(filtered_1,mapping = aes(x=reorder(symptom,(times.recorded/times.present)), y=(times.recorded/times.present))) +
      geom_bar(stat = "identity",  fill  = "#C4961A") + my_theme + ylab("Number of patients") + xlab("Symptom") + coord_flip()
