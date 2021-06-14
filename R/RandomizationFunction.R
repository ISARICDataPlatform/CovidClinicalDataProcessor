# Randomization--------------------------------

# Documentation
#' Generate a randomized dataset based on a real wide dataset supplied which maintains key relationships.
#' @description Used to generate a randomized dataset that can be used to produce aggregated table with possible real data
#' @param data Dataframe to be used as a basis for a randomised dataset.
#' @param var_identifier Vector of names of variables that are identifying variables (default = "usubjid")
#' @return data.frame with values synthesised based on the original dataset
#' @import tidyverse lubridate tidyselect
#' @importFrom  stats runif sample
#' @export


randomization <- function(import.tbl, var_identifier = "usubjid"){
  
  #Full list of variable
  var_all<-import.tbl %>% names()
  
  
  # Classify variables--------------
  if(is.null(var_identifier)==F){var_identifer <- import.tbl %>%
    select(tidyselect::any_of(c(var_identifier))) %>% 
    names()}
  
  # Variables grouped because with a logical link--------------
  var_group<-import.tbl %>% select(ever_icu,
                             date_admit, 
                             date_onset, 
                             date_in_last, 
                             icu_in,
                             icu_out,
                             date_ho_last,
                             date_outcome,
                             dur_niv,
                             dur_imv,
                                treat_invasive_ventilation,
                                treat_non_invasive_ventilation,
                                starts_with("icu_"),
                                icu_treat_invasive_ventilation,
                                icu_treat_non_invasive_ventilation,
                                date_outcome,
                                outcome)%>% names()
  
  # Variables link to icu admission and icu dates--------------
  var_icu_treat <- import.tbl %>% select(starts_with("icu_")) %>% names()
  
  # Variables dates--------------
  var_date<- import.tbl %>% select(date_admit, 
                             date_onset, 
                             date_in_last, 
                             icu_in,
                             icu_out,
                             date_ho_last,
                             date_outcome)%>% names()
  var_other <- import.tbl %>% select(-all_of(c(var_group, var_identifer))) %>% names()
  
  
  
  # Randomizing the wide preprocessed database--------------
  
  randomized <- import.tbl %>%
    mutate(icu_treat_inc=case_when
           (ever_icu==TRUE &
                !is.na(icu_in)&
                !is.na(icu_in)~TRUE, 
                TRUE~FALSE))%>%

    # Adjust all dates by -60 to 100 days
    #mutate(date_shift = ceiling(runif(1, -60, 100))) %>%
    add_column(date_shift = ceiling(runif(nrow(.),  min = -60, max = 60))) %>%
    mutate_at(vars(all_of(var_date)), ~ as_date(ifelse(is.na(.)==T, NA, lubridate::as_date(.)+date_shift))) %>%
    
    # Randomly reordering var_other variables
    mutate_at(vars(all_of(var_other)), function(x){sample(x, replace=FALSE)})
  
    # Select, Randomly reordering and re-bind the grouped variable
    
    rows <- sample(nrow(randomized))
    data2<-select(randomized,c(all_of(var_group)))[rows, ]
    randomized<-select(randomized,-c(all_of(var_group)))%>%bind_cols(data2)
    
    
    # Filter, Randomly reordering and append the ICU_treatment variable
    
    
    data2<-randomized %>%filter(icu_treat_inc==TRUE)%>%
      mutate_at(vars(all_of(var_icu_treat)), function(x){sample(x, replace=FALSE)})
      
    randomized<-randomized %>% filter(icu_treat_inc==FALSE)%>% 
      bind_rows(data2)
    # Select, Randomly reordering and re-bind the outcome for those with an outcome date
    
    data2<-randomized %>% filter(is.na(date_outcome))
    randomized<-randomized %>%filter(!is.na(date_outcome))%>%
      mutate_at(vars(outcome), function(x){sample(x, replace=FALSE)})%>%
      bind_rows(data2)%>%
  
    
    # randomly order the unique identifer, then convert to a number
      #rows <- data.frame(sample(nrow(data)))
      mutate_at(vars(all_of(var_identifer)),
                function(x){factor(sample(x, replace=FALSE))%>% as.numeric}) %>%
      arrange(usubjid) %>%
    select(all_of(var_all))
  

  return(randomized)}

