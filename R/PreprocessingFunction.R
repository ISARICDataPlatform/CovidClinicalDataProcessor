
#' Preprocessing step for all aggregations. Currently: remaps outcome to death, discharge or NA, cuts age into 5-year age groups, and adds a year-epiweek column
#' @param input.tbl Input tibble (output of \code{process.all.data})
#' @import dtplyr dplyr purrr lubridate tibble
#' @importFrom glue glue
#' @return A \code{tibble} intended for input into other aggregation functions (e.g. \code{age.pyramid.prep})
#' @export data.preprocessing

data.preprocessing <- function(input.tbl){
  

 rmv<-exclud.sympt.comorb.tret(input.tbl)
  
 input.tbl %>%
   
  #test<- input.tbl%>%
    lazy_dt(immutable = TRUE) %>%
    select(-c("symptoms_covid.19_symptoms",symptoms_asymptomatic, comorbid_smoking_former))%>%
    mutate(symptrcd_upper_respiratory_tract_symptoms=NA)%>%
    mutate(symptrcd_upper_respiratory_tract_symptoms=case_when(
              symptoms_upper_respiratory_tract_symptoms==FALSE|
              symptoms_sore_throat==FALSE|
              symptoms_runny_nose==FALSE|
              symptoms_ear_pain==FALSE~FALSE,
              TRUE~symptrcd_upper_respiratory_tract_symptoms))%>%
    mutate(symptrcd_upper_respiratory_tract_symptoms=case_when(
              symptoms_upper_respiratory_tract_symptoms==TRUE|
              symptoms_sore_throat==TRUE|
              symptoms_runny_nose==TRUE|
              symptoms_ear_pain==TRUE~TRUE,
              TRUE~symptrcd_upper_respiratory_tract_symptoms))%>%
   mutate(symptrcd_loss_of_taste_smell=NA)%>%
   mutate(symptrcd_loss_of_taste_smell=case_when(
               symptoms_loss_of_smell==FALSE|
               symptoms_loss_of_smell_taste==FALSE|
               symptoms_loss_of_taste==FALSE~FALSE,
               TRUE~symptrcd_upper_respiratory_tract_symptoms))%>%
  mutate(symptrcd_loss_of_taste_smell=case_when(
              symptoms_loss_of_smell==TRUE|
              symptoms_loss_of_smell_taste==TRUE|
              symptoms_loss_of_taste==TRUE~TRUE,
              TRUE~symptrcd_upper_respiratory_tract_symptoms))%>%
  select(-c(all_of(rmv)))%>%
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
    mutate(slider_outcome="LTFU")%>%
    mutate(slider_outcome=case_when(outcome == "death" ~ "Death",
                                    outcome == "discharge" ~ "Discharge",
                                    is.na(outcome) & as_date(date_last)> ymd("2020-10-15")~"Ongoing care",
                                    outcome=="" & as_date(date_last)> ymd("2020-10-15")~"Ongoing care",
                                    TRUE~slider_outcome
                                    )) %>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(date_admit=replace(date_admit,date_admit < "2019-01-01",NA))%>%
    mutate(date_admit=replace(date_admit,date_admit >today(),NA))%>%
    mutate(date_start=replace(date_start,date_start < "2020-01-01",NA))%>%
    mutate(date_last=replace(date_last,date_last < "2020-01-01",NA))%>%
    mutate(imv_en=as_date(imv_en))%>%
    mutate(imv_st=as_date(imv_st))%>%
    mutate(niv_en=as_date(niv_en))%>%
    mutate(niv_st=as_date(niv_st))%>%
    mutate(age=replace(age,age>120,NA))%>%
    mutate(agegp10 = cut(age, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120))) %>%

    ###delete implausible respiratory rates
    mutate(vs_resp=case_when(vs_resp<= 3 ~ NA_real_,
                             vs_resp<=5 & age < 10 ~ NA_real_ ,
                             TRUE ~ vs_resp)) %>%
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
  #rename slider variables
    rename(slider_icu_ever = ever_icu) %>%
    rename(slider_country = country) %>%
    rename(slider_sex = sex) %>%
    rename(slider_symptomatic = symptomatic) %>%
  #work on the dates
    mutate(date_onset=as_date(date_onset))%>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(icu_in=as_date(icu_in))%>%
    mutate(icu_out=as_date(icu_out))%>%
    mutate(date_outcome=as_date(date_outcome))%>%
  #create times variable
    mutate(t_son_ad=date_admit-date_onset)%>%
    mutate(t_ad_icu=icu_in-date_admit)%>%
    mutate(t_ad_imv=imv_st-date_onset)%>%
    mutate(t_ad_niv=niv_st-date_onset)%>%
    mutate(t_ad_icu=icu_in-date_admit)%>%
    mutate(dur_icu=icu_out-icu_in)%>%
    mutate(dur_ho=date_outcome-date_admit)%>%
    mutate(dur_imv=imv_en-imv_st)%>%
    mutate(dur_niv=niv_en-niv_st)%>%
  #set as NA implosible negative value
      mutate(t_ad_icu=replace(t_ad_icu,t_ad_icu<0,NA))%>%
      mutate(t_ad_imv=replace(t_ad_imv,t_ad_imv<0,NA))%>%
      mutate(t_ad_niv=replace(t_ad_niv,t_ad_niv<0,NA))%>%
      mutate(t_ad_icu=replace(t_ad_icu,t_ad_icu<0,NA))%>%
      mutate(dur_icu=replace(dur_icu,dur_icu<0,NA))%>%
      mutate(dur_ho=replace(dur_ho,dur_ho<0,NA))%>%
      mutate(dur_imv=replace(dur_imv,dur_imv<0,NA))%>%
      mutate(dur_niv=replace(dur_niv,dur_niv<0,NA))%>%
  #set as NA outliers for time, vital sign and laboratory variables
    mutate(t_ad_niv = map_dbl(t_ad_niv, outlier.numerical)) %>%
    mutate(t_son_ad = map_dbl(t_son_ad, outlier.numerical)) %>%
    mutate(t_ad_icu = map_dbl(t_ad_icu, outlier.numerical)) %>%
    mutate(t_ad_imv = map_dbl(t_ad_imv, outlier.numerical)) %>%
    mutate(t_ad_icu = map_dbl(t_ad_icu, outlier.numerical)) %>%
    mutate(dur_icu = map_dbl(dur_icu, outlier.numerical)) %>%
    mutate(dur_ho = map_dbl(dur_ho, outlier.numerical)) %>%
    mutate(dur_imv = map_dbl(dur_imv, outlier.numerical)) %>%
    mutate(dur_niv = map_dbl(dur_niv, outlier.numerical)) %>%
    mutate(vs_bmi= map_dbl(vs_bmi, outlier.numerical)) %>%
    mutate(vs_diabp = map_dbl(vs_diabp, outlier.numerical)) %>%
    mutate(vs_height = map_dbl(vs_height, outlier.numerical)) %>%
    mutate(vs_hr = map_dbl(vs_hr, outlier.numerical)) %>%
    mutate(vs_map = map_dbl(vs_map, outlier.numerical)) %>%
    mutate(vs_muarmcir = map_dbl(vs_muarmcir, outlier.numerical)) %>%
    mutate(vs_oxysat_oxygen_therapy = map_dbl(vs_oxysat_oxygen_therapy, outlier.numerical)) %>%
    mutate(vs_oxysat_room_air= map_dbl(vs_oxysat_room_air, outlier.numerical)) %>%
    mutate(vs_oxysat_unknown = map_dbl(vs_oxysat_unknown, outlier.numerical)) %>%
    mutate(vs_pulse= map_dbl(vs_pulse, outlier.numerical)) %>%
    mutate(vs_resp = map_dbl(vs_resp, outlier.numerical)) %>%
    mutate(vs_sysbp = map_dbl(vs_sysbp, outlier.numerical)) %>%
    mutate(vs_temp = map_dbl(vs_temp, outlier.numerical)) %>%
    mutate(vs_weight = map_dbl(vs_weight, outlier.numerical)) %>%
    mutate(vs_oxysat = map_dbl(vs_oxysat, outlier.numerical)) %>%
    mutate(lab_alt = map_dbl(lab_alt, outlier.numerical)) %>%
    mutate(lab_aptt = map_dbl(lab_aptt, outlier.numerical)) %>%
    mutate(lab_ast = map_dbl(lab_ast, outlier.numerical)) %>%
    mutate(lab_bili = map_dbl(lab_bili, outlier.numerical)) %>%
    mutate(lab_crp = map_dbl(lab_crp, outlier.numerical)) %>%
    mutate(lab_lym = map_dbl(lab_lym, outlier.numerical)) %>%
    mutate(lab_neut = map_dbl(lab_neut, outlier.numerical)) %>%
    mutate(lab_pt = map_dbl(lab_pt, outlier.numerical)) %>%
    mutate(lab_urean = map_dbl(lab_urean, outlier.numerical)) %>%
    mutate(lab_wbc = map_dbl(lab_wbc, outlier.numerical)) %>%
   
   mutate(vs_bmi_calc=vs_weight/(vs_height/100)^2)%>%
   mutate(vs_bmi_calc=as.numeric(vs_bmi_calc))%>%
   mutate(vs_bmi=as.numeric(vs_bmi))%>%
   mutate(bmi_comb=ifelse(!is.na(vs_bmi),vs_bmi,vs_bmi_calc))%>%
   mutate(und_nutr=case_when(bmi_comb<18.5 & age<65~"under nutrition",
                             bmi_comb<20.5 & age>64~"under nutrition",
                             bmi_comb>18.4 & age<65 ~"normal nutrition",
                             bmi_comb>20.4 & age>65 ~"normal nutrition",
                             TRUE~NA_character_))%>%
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


#' @keywords internal
#' @export outlier.numerical
outlier.numerical <- function(y){
  if(is.na(y)){
    NA_real_
  } else if(y<(quantile(y, 0.25)-(1.5*IQR(y)))){
    NA_real_
  } else if(y>(quantile(y, 0.75)+(1.5*IQR(y)))){
    NA_real_
  } else {
    y
  }
}



#' @keywords internal
#' @export exclud.sympt.comorb.tret
exclud.sympt.comorb.tret <- function(input.tbl){

tot=nrow(input.tbl)
tot_icu=nrow(filter(input.tbl,ever_icu==TRUE))


data<-select(input.tbl, c(starts_with("symptoms_"),starts_with("comorbid_"),starts_with("treat_"))) %>%
  pivot_longer(c(starts_with("symptoms_"),starts_with("comorbid_"),starts_with("treat_")), 
               names_to = "variable", 
               values_to = "value")%>%
  mutate(count=1)%>%
  group_by(variable,value)%>%
  summarise(n = sum(count, na.rm=T))%>%
  mutate(prop=round(n/tot,digit=2))%>%
  filter(is.na(value))%>%
  filter(prop>=0.95)%>%
  select(variable)

data2<-select(input.tbl, c(starts_with("icu_treat"),ever_icu)) %>%
  filter(ever_icu==TRUE)%>%
  pivot_longer(c(starts_with("icu_treat")), 
               names_to = "variable", 
               values_to = "value")%>%
  mutate(count=1)%>%
  group_by(variable,value)%>%
  summarise(n = sum(count, na.rm=T))%>%
  mutate(prop=round(n/tot_icu,digit=2))%>%
  filter(is.na(value))%>%
  filter(prop>=0.95)%>%
  select(variable)

rmv<-unique(c(data$variable, data2$variable))

}
