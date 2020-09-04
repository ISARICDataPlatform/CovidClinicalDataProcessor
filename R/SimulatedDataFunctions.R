# synthesise--------------------------------

# Documentation
#' Generate a basic synthetic dataset based on a dataset supplied which maintains key relationships.
#' @description Used to generate an easily sharable data dictionary for an R dataframe. This supports the following classes: numeric, integer, logical, Date, character, String, factor, ordered.
#' @param data Dataframe to be used as a basis for a synthetic dataset.
#' @param var_identifier Vector of names of variables that are identifying variables (default = "usubjid")
#' @return data.frame with values synthesised based on the original dataset
#' @import tidyverse lubridate tidyselect
#' @importFrom  stats runif
#' @export


synthesise <- function(data, var_identifier = "usubjid"){
  
  # Classify variables--------------
  if(is.null(var_identifier)==F){var_identifer <- data %>%
    select(tidyselect::any_of(c(var_identifier))) %>% 
    names()}
  
  var_date <- data %>% select_if(lubridate::is.Date) %>% names()
  var_numeric <- data %>% select_if(is.numeric) %>% names()
  var_logical <- data %>% select_if(is.logical) %>% names()
  var_other <- data %>% select(-all_of(c(var_date, var_numeric, var_logical, var_identifer))) %>% names()
  
  
  # Synthesise variables--------------
  synth <- data %>%
    
    # Adjust all dates by -60 to 100 days
    group_by(usubjid) %>%
    mutate(date_shift = ceiling(runif(1, -60, 100))) %>%
    ungroup() %>%
    mutate_at(vars(all_of(var_date)), ~ as_date(ifelse(is.na(.)==T, NA, lubridate::as_date(.)+date_shift))) %>%
    
    # Randomly generate numbers for each variable within current range.
    mutate_at(vars(all_of(var_numeric)), function(x){ceiling(runif(x, min(x, na.rm = T), max(x, na.rm = T)))}) %>%

    # Randomly assign booleans
    mutate_at(vars(all_of(var_logical)), function(x){as.logical(sample(c(0,1), nrow(data), replace = T))}) %>%
    
    
    # randomly change values (within a max of 10 options for each var) if not NA
    mutate_at(vars(all_of(var_other)),
                     function(x){purrr::map2_chr(.x = as.character(x),
                                                 .y = sample(rep(names(as.character(x) %>% table() %>% sort(decreasing = T) %>% head(10)), length(x)))[1:length(x)],
                                                 function(.x, .y){ifelse(is.na(as.character(.x))==F, sample(.y, replace=T)[1], NA)})}) %>%
    
    # randomly order the unique identifer, then convert to a number
    mutate_at(vars(all_of(var_identifer)),
                     function(x){factor(x, levels = x %>% sample()) %>% as.numeric}) %>%
    arrange(usubjid) %>%
    select(-date_shift)
  
  
  return(synth)}