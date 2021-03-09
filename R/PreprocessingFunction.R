
#' Preprocessing step for all aggregations. Currently: remaps outcome to death, discharge or NA, cuts age into 5-year age groups, and adds a year-epiweek column
#' @param input.tbl Input tibble (output of \code{process.all.data})
#' @import dtplyr dplyr purrr lubridate tibble
#' @importFrom glue glue
#' @return A \code{tibble} intended for input into other aggregation functions (e.g. \code{age.pyramid.prep})
#' @export data.preprocessing


date_pull<-as_date("2021-01-17")

data.preprocessing <- function(input.tbl){


  #create a list with the variable dates  
  var_date<- input.tbl %>% select(date_admit, 
                                  date_onset, 
                             date_in_last, 
                             icu_in,
                             icu_out,
                             date_ho_last,
                             #extracorporeal_st,
                             #imv_st,
                             #niv_st,
                             #extracorporeal_en,
                             #imv_en,
                             #niv_en,
                             date_outcome)%>% names()
 
  #create a list of symptoms, comorbidity and treatment variables to be removed since completness<5%
  rmv<-exclud.sympt.comorb.tret(input.tbl)
  
 #preprocessing function
  input.tbl %>%
   
  #test<- input.tbl%>%
    #lazy_dt(immutable = TRUE) %>%
    select(-c("symptoms_covid-19_symptoms",symptoms_asymptomatic))%>%
    #select(-c("symptoms_covid-19_symptoms"))%>%
   #create upper respiratory tract symptoms combining several symptoms
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
   #create loss_of_taste_smell combining several symptoms
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
    mutate(oxygen_therapy=FALSE)%>%
    mutate(oxygen_therapy=case_when(
      treat_high_flow_nasal_cannula==TRUE|
      treat_nasal_mask_oxygen_therapy==TRUE|
        treat_non_invasive_ventilation==TRUE|
        treat_invasive_ventilation==TRUE
      ~TRUE,
      is.na(treat_high_flow_nasal_cannula) &
      is.na(treat_nasal_mask_oxygen_therapy) &
        is.na (treat_non_invasive_ventilation) &
        is.na(treat_invasive_ventilation) ~
        NA,
      TRUE~oxygen_therapy))%>%
    mutate(icu_oxygen_therapy=FALSE)%>%
    mutate(icu_oxygen_therapy=case_when(
      icu_treat_high_flow_nasal_cannula==TRUE|
      icu_treat_nasal_mask_oxygen_therapy==TRUE|
        icu_treat_non_invasive_ventilation==TRUE|
        icu_treat_invasive_ventilation==TRUE~TRUE,
      is.na(icu_treat_high_flow_nasal_cannula)&
      is.na(icu_treat_nasal_mask_oxygen_therapy)&
        is.na(icu_treat_non_invasive_ventilation)&
        is.na(icu_treat_invasive_ventilation)~NA,
      TRUE~icu_oxygen_therapy))%>%
   #Removing variables with records UNK >95% (function: exclud.sympt.comorb.tret)#perhaps to be removed from here and to be added when preparing the aggregated table
    select(-c(all_of(rmv)))%>%
   #Setting_up dates as date
   mutate_at(vars(all_of(var_date)), function(x){as_date(x)})%>%
   #creating first and last date
    mutate(date_hoin_last=case_when(is.na(date_ho_last) ~ date_in_last,
                                    date_ho_last<date_in_last ~ date_in_last,
                                    TRUE ~ date_ho_last ))%>%
    mutate(date_start=case_when(is.na(date_onset) ~ date_admit,
                                date_onset<=date_admit ~ date_admit,
                                TRUE ~  date_onset  ))%>%
    mutate(date_last=case_when(is.na(date_hoin_last)~as_date(date_admit),
                               TRUE~ date_hoin_last))%>%
    mutate(date_last=case_when(!is.na(date_outcome)~date_outcome,
                                TRUE  ~ date_last))%>%
    mutate(date_admit=replace(date_admit,date_admit < "2019-01-01"|date_admit >date_pull,NA))%>%
    mutate(date_start=replace(date_start,date_start < "2020-01-01",NA))%>%
    mutate(date_last=replace(date_last,date_last < "2020-01-01",NA))%>%
    #categorizing outcome
    mutate(slider_outcome="LTFU")%>%
    mutate(slider_outcome=case_when(outcome == "death" ~ "Death",
                                    outcome == "discharge" ~ "Discharge",
                                    is.na(outcome) & as_date(date_last)> date_pull-45 ~"Ongoing care",
                                    outcome=="" & as_date(date_last)> date_pull-45~"Ongoing care",
                                    TRUE~slider_outcome
                                    )) %>%
    #categorizing age
    mutate(age=replace(age,age>120,NA))%>%
    mutate(agegp10 = cut(age, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120))) %>%
    mutate(agegp5 = cut(age, right = FALSE, breaks = c(0,5, 10,15, 20,25, 30,35, 40,45, 50,55,
                                                     60,65, 70,75, 80,85, 90, 95, 100, 120))) %>%
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
  #create time variables but t_son_ad
    mutate(t_ad_icu=icu_in-date_start)%>%
    mutate(t_ad_imv=imv_st-date_start)%>%
    mutate(t_ad_niv=niv_st-date_start)%>%
    mutate(dur_icu=icu_out-icu_in)%>%
    mutate(dur_ho=date_outcome-date_start)%>%
    #mutate(dur_imv=imv_en-imv_st)%>%
    #mutate(dur_niv=niv_en-niv_st)%>%
    #set as NA implausible negative value 
    mutate_at(vars(all_of(c(starts_with("t_"),starts_with("dur_")))), function(x){replace(x,x<0,NA)})%>%
    #create time variable: t_son_ad
    mutate(t_son_ad=case_when(date_admit>=date_onset~date_admit-date_onset,
                              TRUE~ NA_real_))%>%
    #deleting implausible respiratory rates based on age
    mutate(vs_resp=case_when(vs_resp<= 3 ~ NA_real_,
                             vs_resp<=5 & age < 10 ~ NA_real_ ,
                             TRUE ~ vs_resp)) %>%  
  #set as NA outliers for vital sign and laboratory variables
    mutate_at(vars(c(all_of(c(starts_with("vs_"),starts_with("lab_"))))), 
              function(x,na.rm = FALSE){replace(x, 
                                                x<(quantile(x, 0.25, na.rm = TRUE))-(1.5*IQR(x, na.rm = TRUE))|
                                                x>(quantile(x, 0.75, na.rm = TRUE))+(1.5*IQR(x, na.rm = TRUE)),
                                                NA_real_)
                                                  })%>% 
   mutate_at(vars(c(all_of(c(starts_with("t_"),starts_with("dur_"))))), 
             function(x,na.rm = FALSE){replace(x, 
                                               x>(quantile(x, 0.975, na.rm = TRUE)),
                                               NA_real_)
             }
   )%>% 
   #calculating bmi

   mutate(vs_bmi_calc=vs_weight/(vs_height/100)^2)%>%
   mutate(vs_bmi_calc=as.numeric(vs_bmi_calc))%>%
   mutate(vs_bmi=as.numeric(vs_bmi))%>%
   mutate(bmi_comb=ifelse(!is.na(vs_bmi),vs_bmi,vs_bmi_calc))%>%
   mutate(und_nutr=case_when(bmi_comb<18.5 & age<65~"under nutrition",
                             bmi_comb<20.5 & age>64~"under nutrition",
                             bmi_comb>18.4 & age<65 ~"normal nutrition",
                             bmi_comb>20.4 & age>65 ~"normal nutrition",
                             TRUE~NA_character_))%>%
   mutate(embargo_length=case_when(date_admit>date_pull-14~TRUE,
                                   date_admit<=date_pull-14~FALSE
                                   ))%>%
   
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
  filter(prop>=0.90)%>%#changing from 0.95 to 0.90
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
  filter(prop>=0.90)%>%#changing from 0.95 to 0.90
  select(variable)

rmv<-unique(c(data$variable, data2$variable))

}
