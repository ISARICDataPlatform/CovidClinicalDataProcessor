
#' Preprocessing step for all aggregations. Currently: remaps outcome to death, discharge or NA, cuts age into 5-year age groups, and adds a year-epiweek column
#' @param input.tbl Input tibble (output of \code{process.all.data})
#' @import dtplyr dplyr purrr lubridate tibble
#' @importFrom glue glue
#' @return A \code{tibble} intended for input into other aggregation functions (e.g. \code{age.pyramid.prep})
#' @export data.preprocessing

data.preprocessing <- function(input.tbl){
 input.tbl %>%
    #lazy_dt(immutable = TRUE) %>%
    select(-c("symptoms_covid-19_symptoms",symptoms_asymptomatic,	cov_det_id))%>%
    ###creating first and last date
    mutate(date_hoin_last=case_when(is.na(date_ho_last) ~ date_in_last,
                                    date_ho_last<date_in_last ~ date_in_last,
                                    TRUE ~ date_ho_last ))%>%
    mutate(date_start=as_date(date_onset))%>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(date_onset=as_date(date_onset))%>%
    mutate(date_start=case_when(is.na(date_onset) ~ date_admit,
                                date_onset<=date_admit ~ date_admit,
                                TRUE ~  date_onset  ))%>%
    mutate(date_hoin_last=as_date(date_hoin_last))%>%
    mutate(date_last=as_date(date_hoin_last))%>%
    mutate(date_last=case_when(is.na(date_hoin_last)~as_date(date_admit),
                               TRUE~ date_hoin_last))%>%
    mutate(date_outcome=as_date(date_outcome))%>%
    mutate(date_last=as_date(date_last))%>%
    mutate(date_last=case_when(!is.na(date_outcome)~date_outcome,
                                TRUE  ~ date_last))%>%
    mutate(slider_outcome="LFTU")%>%
    mutate(slider_outcome=case_when(outcome == "death" ~ "Death",
                                    outcome == "discharge" ~ "Discharge",
                                    is.na(outcome) & as_date(date_last)> ymd("2020-10-15")~"Ongoing care",
                                    TRUE~slider_outcome
                                    )) %>%
    mutate(date_start=replace(date_start,date_start < "2020-01-01",NA))%>%
    mutate(date_last=replace(date_last,date_last < "2020-01-01",NA))%>%
    mutate(imv_en=as_date(imv_en))%>%
    mutate(imv_st=as_date(imv_st))%>%
    mutate(niv_en=as_date(niv_en))%>%
    mutate(niv_st=as_date(niv_st))%>%
    #mutate(slider_outcome=replace(slider_outcome,slider_outcome=="LFTU" & date_last>as.Date("2020-08-15"),"censored"))%>%
    #select(-outcome) %>%
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
    rename(slider_symptomatic = symptomatic) %>%
    mutate(date_onset=as_date(date_onset))%>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(icu_in=as_date(icu_in))%>%
    mutate(icu_out=as_date(icu_out))%>%
    mutate(date_outcome=as_date(date_outcome))%>%
    mutate(t_son_ad=date_admit-date_onset)%>%
    mutate(t_ad_icu=icu_in-date_admit)%>%
    mutate(t_ad_icu=replace(t_ad_icu,t_ad_icu<0,NA))%>%
    mutate(t_ad_imv=imv_st-date_onset)%>%
    mutate(t_ad_imv=replace(t_ad_imv,t_ad_imv<0,NA))%>%
    mutate(t_ad_niv=niv_st-date_onset)%>%
    mutate(t_ad_niv=replace(t_ad_niv,t_ad_niv<0,NA))%>%
    mutate(t_ad_icu=icu_in-date_admit)%>%
    mutate(t_ad_icu=replace(t_ad_icu,t_ad_icu<0,NA))%>%
    mutate(icu_dur=icu_out-icu_in)%>%
    mutate(icu_dur=replace(icu_dur,icu_dur<0,NA))%>%
    mutate(dur_icu=icu_out-icu_in)%>%
    mutate(dur_icu=replace(dur_icu,dur_icu<0,NA))%>%
    mutate(dur_ho=date_outcome-date_admit)%>%
    mutate(dur_ho=replace(dur_ho,dur_ho<0,NA))%>%
    mutate(dur_imv=imv_en-imv_st)%>%
    mutate(dur_imv=replace(dur_imv,dur_imv<0,NA))%>%
    mutate(dur_niv=niv_en-niv_st)%>%
    mutate(dur_niv=replace(dur_niv,dur_niv<0,NA))%>%
    mutate(under_nutrition=case_when(vs_bmi< 18.5&age<65~TRUE,
                                     vs_bmi< 20.5 &age<65~TRUE,
                                     is.na(vs_bmi)|is.na(age)~NA,
                                     TRUE~FALSE))%>%
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
#' @export cleaning.unplosible.dates
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

