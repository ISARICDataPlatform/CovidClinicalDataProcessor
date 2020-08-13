
#' Aggregate data for age pyramid plot
#' @param input.tbl Input tibble (output of \code{process.all.data})
#' @import dtplyr dplyr tibble purrr
#' @importFrom glue glue
#' @return A \code{tibble} containing the input data for the age pyramid plot
#' @export age.pyramid.prep
age.pyramid.prep <- function(input.tbl){

  aggregated.tbl <- input.tbl %>%
    lazy_dt(immutable = TRUE) %>%
    select(age, sex, outcome, country, date_admit, date_outcome) %>%
    filter(!is.na(age) & !is.na(sex)) %>%
    mutate(outcome2 = map2_chr(outcome, date_outcome, ~(case_when(.x == "Death" ~ "death",
                                                                  .x == "Discharged Alive" ~ "discharge",
                                                                  is.na(.y) ~ "censored",
                                                                  TRUE ~ NA_character_)))) %>%
    filter(!is.na(outcome2)) %>%
    select(-outcome) %>%
    rename(outcome = outcome2) %>%
    mutate(agegp5 = cut(age, right = FALSE, breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 120))) %>%
    mutate(year.admit = year(date_admit)) %>%
    mutate(epiweek.admit = epiweek(date_admit)) %>%
    mutate(year.epiweek.admit = glue("{year.admit}-{epiweek.admit}", .envir = .SD)) %>%
    mutate(year.epiweek.admit = replace(year.epiweek.admit, year.epiweek.admit == "NA-NA", NA)) %>%
    select(sex, agegp5, country, year.epiweek.admit, outcome) %>%
    group_by(sex, outcome, country, year.epiweek.admit, agegp5) %>%
    summarise(count =n()) %>%
    mutate(lower.ag.bound  = map_dbl(agegp5, extract.age.boundaries, TRUE)) %>%
    mutate(upper.ag.bound  = map_dbl(agegp5, extract.age.boundaries, FALSE)) %>%
    mutate(agegp5t = fct_relabel(agegp5, prettify.age.labels)) %>%
    select(-agegp5) %>%
    rename(agegp5 = agegp5t) %>%
    as_tibble()
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

