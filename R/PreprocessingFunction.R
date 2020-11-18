
#' Preprocessing step for all aggregations. Currently: remaps outcome to death, discharge or NA, cuts age into 5-year age groups, and adds a year-epiweek column
#' @param input.tbl Input tibble (output of \code{process.all.data})
#' @import dtplyr dplyr purrr lubridate tibble
#' @importFrom glue glue
#' @return A \code{tibble} intended for input into other aggregation functions (e.g. \code{age.pyramid.prep})
#' @export data.preprocessing

data.preprocessing <- function(input.tbl){
  input.tbl %>%
    #lazy_dt(immutable = TRUE) %>%
    select(-c("symptoms_covid.19_symptoms", "treat_NA", "icu_treat_NA"))%>%
    ###cleaning dates
    mutate(date_admit=replace(date_admit,date_admit>as.Date("2020-10-15") | date_admit < as.Date("2019-01-01"), NA_Date_))%>%
    mutate(date_onset=replace(date_onset,date_onset<as.Date("2020-01-01") | date_onset> as.Date("15/10/2020"), NA_Date_))%>%
    mutate(icu_in=replace(icu_in,icu_in<as.Date("2020-01-01") | icu_in> as.Date("2020-10-15"),NA_Date_))%>%
    mutate(icu_out=replace(icu_out,icu_out<as.Date("2020-01-01") | icu_out> as.Date("2020-10-15"),NA_Date_))%>%
    mutate(date_outcome=replace(date_outcome,date_outcome<as.Date("2020-01-01") | date_outcome> as.Date("2020-10-15"),NA_Date_))%>%
    mutate(date_ho_last=replace(date_ho_last,date_ho_last<as.Date("2020-01-01") | date_ho_last> as.Date("2020-10-15"),NA_Date_))%>%
    mutate(date_in_last=replace(date_in_last,date_in_last<as.Date("2020-01-01") | date_in_last> as.Date("2020-10-15"),NA_Date_))%>%
    mutate(date_start=case_when(date_onset>date_admit~ date_onset,
                                is.na(date_admit) ~ date_onset,
                                TRUE ~  date_admit  ))%>%
    mutate(date_hoin_last=case_when(is.na(date_ho_last) ~ date_in_last,
                                    date_ho_last<date_in_last ~ date_in_last,
                                    TRUE ~ date_ho_last ))%>%
    mutate(date_last=case_when(is.na(date_hoin_last)~date_start,
                               TRUE~ date_hoin_last))%>%
    mutate(date_last=case_when(!is.na(date_outcome)~date_outcome,
                                TRUE  ~ date_last))%>%
    mutate(slider_outcome=case_when(outcome == "Death" ~ "death",
                                    outcome == "Discharge" ~ "discharge",
                                    outcome == "Ongoing care" ~ "Ongoing care",
                                    outcome == "Transferred" ~ "transferred",
                                    outcome== "Unknown outcome" ~ "Unknown outcome",
                                    is.na(outcome) & !is.na(date_outcome) ~"Unknown outcome",
                                    is.na(outcome) & is.na(date_outcome) & date_last>as.Date("2020-08-15") ~ "censored",
                                    TRUE~"LTFU" )) %>%
    select(-outcome) %>%
    #applying a cut-off value for censored
     
    mutate(agegp10 = cut(age, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120))) %>%

    ###delete implausible respiratory rates
    mutate(vs_resp=case_when(vs_resp<= 3 ~ NA_real_,
                             vs_resp<=5 & age < 10 ~ NA_real_ ,
                             TRUE ~ vs_resp)) %>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(calendar.year.admit = year(date_admit)) %>%
    mutate(calendar.month.admit = month(date_admit)) %>%
    mutate(slider_monthyear = map2_chr(calendar.year.admit, calendar.month.admit, month.year.mapper)) %>%
    mutate(year.admit = map_dbl(date_admit, epiweek.year)) %>%
    mutate(epiweek.admit = epiweek(date_admit)) %>%
    mutate(year.epiweek.admit=paste0(year.admit,"-", epiweek.admit))%>%
    mutate(year.epiweek.admit = replace(year.epiweek.admit, year.epiweek.admit == "NA-NA", NA)) %>%
    mutate(lower.age.bound  = map_dbl(agegp10, extract.age.boundaries, TRUE)) %>%
    mutate(upper.age.bound  = map_dbl(agegp10, extract.age.boundaries, FALSE)) %>%
    mutate(slider_agegp10 = fct_relabel(agegp10, prettify.age.labels)) %>%
    select(-agegp10) %>%
    rename(slider_icu_ever = ever_icu) %>%
    rename(slider_country = country) %>%
    rename(slider_sex = sex) %>%
    mutate(date_onset=as_date(date_onset))%>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(icu_in=as_date(icu_in))%>%
    mutate(icu_out=as_date(icu_out))%>%
    mutate(date_outcome=as_date(date_outcome))%>%
    mutate(t_son_ad=date_admit-date_onset)%>%
    mutate(t_ad_icu=icu_in-date_admit)%>%
    mutate(t_ad_icu=replace(t_ad_icu,t_ad_icu<0,NA))%>%
    mutate(icu_dur=icu_out-icu_in)%>%
    mutate(icu_dur=replace(icu_dur,icu_dur<0,NA))%>%
    mutate(ho_dur=date_outcome-date_admit)%>%
    mutate(ho_dur=replace(ho_dur,ho_dur<0,NA))%>%
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

