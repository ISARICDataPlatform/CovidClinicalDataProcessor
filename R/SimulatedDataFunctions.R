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
  library(dplyr);library(lubridate);library(stats);library(purrr);library(tidyselect)
  
  # Classify variables--------------
  if(is.null(var_identifier)==F){var_identifer <- data %>%
    dplyr::select(tidyselect::any_of(c(var_identifier))) %>% 
    names()}
  
  var_date <- data %>% select_if(lubridate::is.Date) %>% names()
  var_numeric <- data %>% select_if(is.numeric) %>% names()
  var_other <- data %>% select(-all_of(c(var_date, var_numeric, var_identifer))) %>% names()
  
  
  # Synthesise variables--------------
  synth <- data %>%
    
    # Adjust all dates by -60 to 100 days
    dplyr::group_by(usubjid) %>%
    dplyr::mutate(date_shift = ceiling(stats::runif(1, -60, 100))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at(vars(all_of(var_date)), ~ lubridate::as_date(ifelse(is.na(.)==T, NA, lubridate::as_date(.)+date_shift))) %>%
    
    # Randomly generate numbers for each variable within current range.
    dplyr::mutate_at(vars(all_of(var_numeric)), function(x){ceiling(stats::runif(x, min(x, na.rm = T), max(x, na.rm = T)))}) %>%
    
    # randomly change values (within a max of 10 options for each var) if not NA
    dplyr::mutate_at(vars(all_of(var_other)),
                     function(x){purrr::map2_chr(.x = as.character(x),
                                                 .y = sample(rep(names(as.character(x) %>% table() %>% sort(decreasing = T) %>% head(10)), length(x)))[1:length(x)],
                                                 function(.x, .y){ifelse(is.na(as.character(.x))==F, sample(.y, replace=T)[1], NA)})}) %>%
    
    # randomly order the unique identifer, then convert to a number
    dplyr::mutate_at(vars(all_of(var_identifer)),
                     function(x){factor(x, levels = x %>% sample()) %>% as.numeric}) %>%
    dplyr::arrange(usubjid) %>%
    dplyr::select(-date_shift)
  
  
  return(synth)}