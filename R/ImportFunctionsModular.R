
#' Import demographic data
#' @param file.name Path of the demographics data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble 
#' @return Formatted demographic data as a tibble or \code{dtplyr_step}
#' @export import.demographic.data



import.demographic.data <- function(file.name, dtplyr.step = FALSE){
  
  wdi_dat <- WDI(indicator = c("NY.GDP.PCAP.KD", "SP.DYN.LE00.IN", "SP.DYN.IMRT.IN"), 
                 start = 2020, end = 2020, extra = TRUE)%>%
                  filter(region != "Aggregates")%>%
                  select("Alpha_3"=iso3c,income)

  
  country.lookup <- ISOcodes::ISO_3166_1 %>% as_tibble%>%
    mutate(Name=case_when(!is.na(Common_name)~Common_name,
                          Name=="Lao People's Democratic Republic"~"Lao PDR",
                          TRUE~Name))%>%select(Alpha_3, Name)%>%left_join(wdi_dat)
  #regexp <- "[[:digit:]]+"  # process string
  
  out <- dm %>%
    ###delete patients duplicates
    group_by(usubjid) %>% 
    mutate(count=1)%>% 
    mutate(n = sum(count)) %>%
    filter(n == 1) %>%
    ungroup()%>%
    rename(date_admit=rfstdtc)%>%
    as.data.frame()%>%
    mutate(age_d=case_when(ageu=="MONTHS"~12,
                           ageu=="YEARS" ~ 1,
                           ageu=="DAYS" ~ 365.25,
                           TRUE~ NA_real_))%>%
    mutate(age2=age/age_d)%>%
    select(-(age))%>%
    rename(age=age2)%>%
    mutate(age=replace(age,age<0,NA))%>%
    mutate(ethnic = iconv(ethnic, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(ethnic = str_remove_all(ethnic, "\\s*\\([^)]*\\)")) %>%
    mutate(ethnic = str_replace_all(ethnic, " - ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, "-", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, "/| / ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, " ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, ",", "_")) %>%
    mutate(ethnic = replace(ethnic, ethnic == "n_a" | ethnic == "na" | ethnic == "", NA))%>%
    #mutate(studyid=substr(usubjid,1, 7))%>%
    mutate(siteid_final=invid)%>%
    mutate(siteid_final=case_when(invid=="00741cca_network"~ substr(subjid,1, 12),
                                  invid=="227inserm"~ sub("\\-.*", "",subjid),
                                  invid=="00689us_nhlbi_peta"~ sub("\\-.*", "",subjid),
                                  invid==""~studyid,
                                  studyid=="CVPRQTA"~"CVPRQTA",
                                  TRUE~invid))%>%
    mutate(sex = case_when(sex == "M" ~ "Male",
                           sex == "F" ~ "Female",
                           TRUE ~ NA_character_))%>%
    mutate(date_admit=substr(date_admit,1, 10))%>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(date_admit=replace(date_admit,date_admit >date_pull,NA))%>%
    select(usubjid, studyid, siteid_final, date_admit, age, sex, ethnic, country)
  
  site_id_country<-out%>%
    mutate(country=ifelse(siteid_final=="321cub_erasme__bru","BEL",country))%>% 
    mutate(country=ifelse(siteid_final=="00580netcare_unita","ITA",country))%>%
    mutate(country=ifelse(siteid_final=="00835consortium_im","POL",country))%>%
    mutate(country=ifelse(siteid_final=="00831nicvd_dhaka","BGD",country))%>%
    mutate(country = replace(country, country == "", NA)) %>%
    left_join(country.lookup, by = c("country" = "Alpha_3")) %>%
    select(-country) %>%
    rename(country = Name) %>%
    filter(!is.na(country))%>%
    arrange(desc(country, income, siteid_final))%>%
    distinct(siteid_final, .keep_all =T)%>% 
    select(siteid_final, 'country_2'=country,income)
  
  out<-out%>% 
    left_join(site_id_country)%>%
    mutate(country=country_2)%>%select(-c(country_2))
  
  if(dtplyr.step){
    return(out)
  } else {
    return(out %>% as_tibble())
  }
}



#' Import microb data
#' @param file.name Path of the microbio data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble 
#' @return Formatted demographic data as a tibble or \code{dtplyr_step}
#' @export import.microbio.data


import.microb.data <- function(file.name, dtplyr.step = FALSE){
  
  #mb<-shared.data.import(file.name, dtplyr.step = TRUE)
  mb<-mb
  
  detection<- mb%>%
    #select(usubjid,mbtestcd,mbtest,mbtstdtl,mbcat,mbstresc,mbspec,mbloc,mbmethod)%>%
    filter(mbtstdtl=="DETECTION")%>%
    filter(mbtestcd=="CRONAVIR"|mbtestcd=="SARSCOV2")%>%
    mutate(mbstresc = case_when(mbstresc == "NO" ~ "NEGATIVE",
                                mbstresc == "NEGATIVE" ~ "NEGATIVE",
                                mbstresc == "POSITIVE" ~ "POSITIVE",
                                TRUE ~ NA_character_)) %>%
    mutate(mbtestcd = paste0("cov_det_",mbtestcd)%>% tolower%>%str_replace_all(" ", "_")) %>%
    arrange(desc(mbstresc))%>%
    distinct(usubjid, mbtestcd, .keep_all =T)%>% 
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = mbtestcd, values_from = mbstresc) %>%
    as.data.frame()
  
  
  identification<-mb%>%
    filter(mbtstdtl=="IDENTIFICATION")%>%
    distinct(usubjid, mbstresc, .keep_all =T)%>% 
    filter(mbstresc=="SEVERE ACUTE RESPIRATORY SYNDROME CORONAVIRUS 2"|
             mbstresc=="CORONAVIRIDAE")%>%
    mutate(mbstresc=replace(mbstresc,mbstresc=="SEVERE ACUTE RESPIRATORY SYNDROME CORONAVIRUS 2","SARSCOV2"))%>%
    mutate(mbstresc=replace(mbstresc,mbstresc=="SEVERE ACUTE RESPIRATORY SYNDROME-RELATED CORONAVIRUS","SARSCOV2"))%>%
    
    mutate(mbstresc=replace(mbstresc,mbstresc=="CORONAVIRIDAE","CRONAVIR"))%>%
    mutate(result="POSITIVE")%>%
    mutate(mbstresc = paste0("cov_id_",mbstresc)%>%
             tolower%>%
             str_replace_all(" ", "_")) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = mbstresc, values_from = result) %>%
    as.data.frame()
  
  out<-full_join(detection,identification)%>%
    mutate(cov_det_id="NEGATIVE")%>%
    mutate(cov_det_id=case_when(cov_det_cronavir=="POSITIVE"|
                                  cov_det_sarscov2=="POSITIVE"|
                                  cov_id_cronavir=="POSITIVE"|
                                  cov_id_sarscov2=="POSITIVE"~
                                  "POSITIVE",
                                is.na(cov_det_cronavir)&
                                  is.na(cov_det_sarscov2)&
                                  is.na(cov_id_cronavir)&
                                  is.na(cov_id_sarscov2)~
                                  NA_character_,
                                TRUE~cov_det_id))
  
  
  if(dtplyr.step){
    return(out)
  } else {
    return(out %>% as_tibble())
  }
}

#' Import data on symptoms and comorbidities
#' @param file.name Path of the symptoms data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble stringr
#' @return Formatted comorbidity and symptom data as a tibble or \code{dtplyr_step}
#' @export import.symptom.and.comorbidity.data
import.symptom.and.comorbidity.data <- function(file.name, dtplyr.step = TRUE){
  
  #out <- shared.data.import(file.name, 
  #                         dtplyr.step = TRUE, 
  #                        immutable = TRUE) %>% # this will often by used twice, so should be immutable
  
  out<-sa%>%
    
    #imp_sa<-sa%>%
    select(usubjid, saterm, sacat,  samodify, sapresp, saoccur, sastdtc) %>%
    mutate(sacat=case_when(
      saterm=="CLINICALLY-DIAGNOSED COVID-19"~"CLINICALLY-DIAGNOSED COVID-19",
      TRUE~sacat))%>%
    filter(
          sacat=="MEDICAL HISTORY"|
      sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION"
      |sacat=="CLINICALLY-DIAGNOSED COVID-19"
           )%>%
    #mutate(sacat=replace(sacat,saterm=="MALNUTRITION","MEDICAL HISTORY"))%>%#temporary correction
    #filter( sapresp=="Y") %>%
    mutate(saoccur = case_when(saoccur == "Y" ~ TRUE,
                               saoccur == "N" ~ FALSE,
                               TRUE ~ NA)) %>%
    filter(!is.na(saoccur)) %>%
    mutate(saterm=toupper(saterm))%>%
    mutate(saterm=case_when(samodify!=""|is.na(samodify)~samodify,
                            TRUE ~ saterm))%>%#to add
    mutate(saterm=case_when(saterm%like%'CARDIAC ARRHYTHMIA'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CARDIAC DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CHORNIC CARDIAC DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CHRONIC HEART DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CONGENITAL CA'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CONGENTIAL CARDIOPATHY'~'CHRONIC CARDIAC DISEASE',
                            saterm=='CORONARY DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='HEART FAILURE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='OROVALVA DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='RHEUMATIC HEART DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='VALVULAR HEART DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='CONGESTIVE HEART FAILURE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='CORONARY ARTERY DISEASE'~'CHRONIC CARDIAC DISEASE',
                            TRUE~saterm))%>%
    mutate(saterm=case_when(saterm=='CHRONIC DIALYSIS'~'CHRONIC KIDNEY DISEASE',
                            saterm%like%'DEPRESSION'~'PSYCHIATRIC CONDITION',
                            saterm%like%'PSYCHOSIS'~'PSYCHIATRIC CONDITION',
                            saterm%like%'DYSLIPIDEMIA'~'CHRONIC METABOLIC DISORDER',
                            saterm%like%'HYPOTHYROIDISM'~'CHRONIC ENDOCRINE DISORDER NON DIABITES',
                            saterm%like%'HEPATITIS'~'LIVER DISEASE',	
                            saterm%like%'MARASUMAS'~'MALNUTRITION',	
                            saterm=='SAM UNDEFINED'~'MALNUTRITION',	
                            saterm=='MIXED MARASMIC-KWASH'~'MALNUTRITION',	
                            saterm=='OSA/ HOME CPAP/BI-PAP USE'~'OBESITY',
                            saterm=='PAOD'~'OTHER COMORBIDITIES',
                            saterm=='PEPTIC ULCER DISEASE EXCLUDING BLEEDING'~'OTHER COMORBIDITIES',
                            saterm=='PARALYSIS'~'CHRONIC NEUROLOGICAL DISORDER',
                            saterm=='STROKE OR OTHER NEUROLOGICAL DISORDERS'~'CHRONIC NEUROLOGICAL DISORDER',
                            saterm=='PULMONARY CIRCULATION DISORDER'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'ARRHYTHMIA'~'CHRONIC CARDIAC DISEASE',
                            
                            saterm=='SUBSTANCE USE DISORDER'~'SUBSTANCE MISUSE',	
                            saterm=='VENOUS THROMBOEMBOLISM- DVT/PE'~'THROMBOLIC DISORDERS',
                            
                            saterm=='CHILLS/RIGORS'~'RIGOR OR SWEATING',
                            saterm=='NIGHT SWEAT'~'RIGOR OR SWEATING',
                            
                            saterm=='CONGESTION/RHINORRHEA'~'RUNNY NOSE',
                            saterm=='CONJUNCTIVAL CONGESTION'~'UPPER RESPIRATORY TRACT SYMPTOMS',
                            saterm=='SNEEZING'~'UPPER RESPIRATORY TRACT SYMPTOMS',
                            
                            saterm=='DELIRIUM / ENCEPHALOPATHY'~'ALTERED CONSCIOUSNESS CONFUSION',
                            saterm=='DIZZINESS/LIGHTHEADEDNESS'~'OTHER SIGNS AND SYMPTOMS',	
                            saterm=='GASTROGASTROINTESTINAL HEMORRHAGE'~'OTHER SIGNS AND SYMPTOMS',	
                            TRUE~saterm))%>%
    mutate(saterm=case_when(saterm%like%'TUBERCULOSIS'~'TUBERCULOSIS',
                            saterm%like%'MALIGNANCY'~'MALIGNANT NEOPLASM',
                            saterm%like%'SPECIFIC CANCERS'~'MALIGNANT NEOPLASM',
                            saterm%like%'SOLID TUMOR'~'MALIGNANT NEOPLASM',
                            saterm%like%'METASTATIC CANCER'~'MALIGNANT NEOPLASM',
                            
                            
                            saterm=='SORE THROAT/THROAT PAIN'~'SORE THROAT',
                            
                            saterm=='COAGULOPATHY'~'CHRONIC HEMATOLOGIC DISEASE',
                            saterm=='DYSLIPIDEMIA/HYPERLIPIDEMIA'~'CHRONIC HEMATOLOGIC DISEASE',
                            saterm=='IRON DEFICIENCY ANEMIA'~'CHRONIC HEMATOLOGIC DISEASE',
                            saterm=='BLOOD LOSS ANEMIA'~'CHRONIC HEMATOLOGIC DISEASE',
                            
                            saterm=='CHRONIC HEMATOLOGICAL DISEASE'~'CHRONIC HEMATOLOGIC DISEASE',
                            saterm=='CHRONIC LIVER DISEASE'~'LIVER DISEASE',
                            saterm%like%'ACUTE LIVER'~'LIVER DISEASE',
                            
                            saterm%like%'CHRONIC RENAL FAILURE'~'CHRONIC KIDNEY DISEASE',
                            
                            saterm%like%'CHRONIC LUNG DISEASE'~'CHRONIC PULMONARY DISEASE',
                            saterm%like%'CHROMIC PULMONARY DISEASE'~'CHRONIC PULMONARY DISEASE',
                            TRUE~saterm))%>%
    mutate(saterm=case_when(saterm%like%'RHEMATOLOGICAL DISORDER'~'rheumatologic disorder',
                            saterm%like%'CHRONIC NEUROLOGICAL'~'CHRONIC NEUROLOGICAL DISORDER',
                            saterm%like%'CURRENT SMOK'~'SMOKING',
                            saterm%like%'DIABETES'~'DIABETES',
                            saterm=='HISTORY OF PERIPHERAL OR CARDIAC REVASCULARIZATION'~'HISTORY OF PERIPHERAL OR CARDIAC REVASCULARIZATION',
                            saterm=='HISTORY OF SMOKING'~'SMOKING',
                            saterm%like%'SMOKING'~'SMOKING',
                            saterm%like%'HIV'~'AIDS/HIV',
                            saterm%like%'LIVER DISEASE'~'LIVER DISEASE',
                            saterm%like%'OTHER RELEVANT RISK'~'OTHER COMORBIDITIES',
                            saterm=='OTHER RISK FACTOR'~'OTHER COMORBIDITIES',
                            saterm%like%'RHEUMATOLOGICAL DISORD'~'RHEUMATOLOGIC DISORDER',
                            saterm=='SMOKER'~'SMOKING',
                            saterm=='SMOKER - CURRENT'~'SMOKING',
                            saterm=='SMOKER - FORMER'~'SMOKING - FORMER',
                            saterm=='FEEDING INTOLERANCE (PAEDIATRICS)'~'ANOREXIA',
                            saterm=='REFUSING TO EAT OR DRINK/HISTORY OF POOR ORAL INTAKE'~'ANOREXIA',
                            saterm%like%'ANOREXIA'~'ANOREXIA',
                            saterm=='ANOREXIA - LOSS OF APPETITE'~'ANOREXIA',
                            saterm=='CHEST PAIN/TIGHTNESS'~'CHEST PAIN',
                            
                            
                            saterm=='SWOLLEN NECK GLANDS/LYMPHADENOPATHY'~'LYMPHADENOPATHY',
                            TRUE~saterm))%>%
    mutate(saterm=case_when(saterm%like%'COUGH'~'COUGH',
                            saterm%like%'COUTH'~'COUGH',
                            saterm=='HEMOPTYSIS'~'COUGH',
                            saterm=='DIARRHEA'~'DIARRHOEA',
                            saterm=='CONJUNCTIVAL CONGESTION '~'CONJUNCTIVITIS',
                            
                            saterm%like%'FEVER'~'HISTORY OF FEVER',
                            saterm=='SEIZURE'~'SEIZURES',
                            saterm%like%'TRANSPLANT'~'TRANSPLANTATION',
                            saterm%like%'ANOSMIA'~'LOSS OF SMELL',
                            saterm%like%'AGEUSIA'~'LOSS OF TASTE',
                            saterm=="LOSS OF TASTE OR LOSS OF SMELL"~'LOSS OF SMELL/TASTE',
                            saterm=="NAUSEA/VOMITING"~'VOMITING/NAUSEA',
                            
                            saterm%like%'MYALGIA OR FATIGUE'~'MUSCLE ACHES/JOINT PAIN',
                            saterm%like%'JOINT PAIN'~'MUSCLE ACHES/JOINT PAIN',
                            saterm%like%'MUSCLE ACHES'~'MUSCLE ACHES/JOINT PAIN',
                            saterm=='OTHER SIGN OR SYMPTOM'~'OTHER SIGNS AND SYMPTOMS',
                            saterm=='LOWER CHEST WALL INDRAWING'~'SHORTNESS OF BREATH',
                            saterm%like%'DEHYDRATION'~'SEVERE DEHYDRATION',
                            saterm%like%'RASH'~'SKIN RASH',
                            saterm=='EARPAIN'~'EAR PAIN',
                            TRUE ~ saterm ))%>%
    mutate(saterm = iconv(saterm, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(saterm = str_remove_all(saterm, "\\s*\\([^)]*\\)")) %>%
    mutate(saterm = str_replace_all(saterm, " - ", "_")) %>%
    mutate(saterm = str_replace_all(saterm, "/| / ", "_")) %>%
    mutate(saterm = str_replace_all(saterm, " ", "_")) %>%
    arrange(desc(saoccur))%>%
    distinct(usubjid,saterm, .keep_all =T)
  
  
  
  if(dtplyr.step){
    return(out)
  } else {
    return(out %>% as_tibble())
  }
}


#' Process data on comorbidities
#' @param input Either the path of the symptoms/comorbidities data file (CDISC format) or output of \code{import.symptom.and.comorbidity.data}
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble stringr tidyfast
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted comorbidity data as a tibble or \code{dtplyr_step}
#' @export process.comorbidity.data
process.comorbidity.data <- function(input,  minimum=100, dtplyr.step = FALSE){
  
  comorbid <- imp_sa_com %>%
    filter(sacat=="MEDICAL HISTORY") %>%
    filter(!is.na(sacat))%>%
    filter(!is.na(saterm))%>%
    arrange(desc(saoccur))%>%
    group_by(saterm) %>% 
    arrange(desc(saoccur))%>%
    mutate(n = sum(!is.na(saoccur))) %>%
    filter(n >= eval(!!minimum))%>%
    ungroup()%>%
    mutate(saterm = paste0("comorbid_",saterm)) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = saterm, values_from = saoccur) 
  if(dtplyr.step){
    return(comorbid %>% lazy_dt(immutable = FALSE))
  } else {
    return(comorbid %>% as_tibble())
  }
}


#' Process data on symptoms
#' @param input Either the path of the symptoms/comorbidities data file (CDISC format) or output of \code{import.symptom.and.comorbidity.data}
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble tidyfast dtplyr
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted symptom data as a tibble or \code{dtplyr_step}
#' @export process.symptom.data
process.symptom.data <- function(input,  minimum=100, dtplyr.step = FALSE){
  
  
  symptom_w <- imp_sa%>%mutate(studyid=substr(usubjid,1, 7))%>%filter(studyid!="CVZXZMV")%>%
    filter(sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION") %>%
    filter(saterm!="covid-19_symptoms")%>%
    arrange(desc(saoccur))%>%
    group_by(saterm) %>% 
    arrange(desc(saoccur))%>%
    mutate(n = sum(!is.na(saoccur))) %>%
    filter(n >= eval(!!minimum))%>%
    ungroup()%>%
    mutate(saterm = paste0("symptoms_",saterm)) %>%
    #mutate(saterm = glue("symptoms_{saterm}", .envir = .SD)) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = saterm, values_from = saoccur) %>%
    as.data.frame()
  
  date_onset<-imp_sa%>%
    ungroup()%>%
    filter(sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION" & saoccur==TRUE) %>%
    mutate(sastdtc=as.character(sastdtc))%>%
    mutate(sastdtc = replace(sastdtc, sastdtc =="" , NA))%>%
    mutate(sastdtc=substr(sastdtc,1, 10))%>%
    mutate(sastdtc=as_date(sastdtc))%>%
    filter(sastdtc >= "2020-01-01")%>%
    filter(sastdtc < date_pull)%>%
    arrange(sastdtc)%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, "date_onset"=sastdtc)
  
  covid_clinic_diagn<- imp_sa%>%
    filter(sacat=="CLINICALLY-DIAGNOSED COVID-19")%>%
    mutate(saoccur=case_when(is.na(sapresp)~TRUE,
                             TRUE~saoccur))%>%
    arrange(desc(saoccur))%>%
    distinct(usubjid, .keep_all =T)%>%
    rename("clin_diag_covid_19"=saoccur)%>%
    select(usubjid,clin_diag_covid_19)
  
  
  symptomatic<-imp_sa%>%mutate(studyid=substr(usubjid,1, 7))%>%filter(studyid!="CVZXZMV")%>%
    ungroup()%>%
    filter(sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION")%>%
    mutate(symptomatic=case_when(saterm=="asymptomatic" & saoccur==TRUE~FALSE,
                                 saterm=="asymptomatic" & saoccur==FALSE~TRUE,
                                 TRUE~saoccur
    ))%>%
    arrange(desc(symptomatic))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, symptomatic)
  
  symptom<- date_onset%>%
    full_join(covid_clinic_diagn, by=c("usubjid"))%>%
    full_join(symptomatic, by = c("usubjid"))%>%
    full_join(symptom_w, by = c("usubjid"))
  
  
  
  if(dtplyr.step){
    return(symptom %>% lazy_dt(immutable = FALSE))
  } else {
    return(symptom %>% as_tibble())
  }
}
  




#' Process data on pregnancy (as comorbidity)
#' @param file.name Path of the dispositions data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble stringr
#' @return Formatted pregnancy data as a tibble or \code{dtplyr_step}
#' @export process.pregnancy.data



process.pregnancy.data <- function(file.name, dtplyr.step = FALSE){
  comorbid_pregnancy <- rp%>%
    #comorbid_pregnancy <- shared.data.import(pregnancy.file.name, dtplyr.step = FALSE)%>%
    #comorbid_pregnancy <- rp_open%>%
    filter(rptestcd=="PREGIND") %>%
    mutate(comorbid_pregnancy=rpstresc) %>%
    mutate(comorbid_pregnancy = case_when(comorbid_pregnancy == "Y" ~ TRUE,
                                          comorbid_pregnancy == "N" ~ FALSE,
                                          TRUE ~ NA)) %>%
    select(usubjid,comorbid_pregnancy)
  if(dtplyr.step){
    return(comorbid_pregnancy %>% lazy_dt(immutable = FALSE))
  } else {
    return(comorbid_pregnancy %>% as_tibble())
  }
}



#' Process data on ICU admission
#' @param file.name Path of the healthcare encounters data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble tidyfast dtplyr
#' @importFrom data.table as.data.table
#' @return Formatted symptom data as a tibble or \code{dtplyr_step}
#' @export process.ICU.data
process.ICU.data <- function(file.name, dtplyr.step = FALSE){
  icu <- ho%>%
    mutate(hooccur = case_when(hooccur == "Y" ~ TRUE,
                               hooccur == "N" ~ FALSE,
                               TRUE ~ NA)) %>%
    filter(!is.na (hooccur))%>%
    select(usubjid, hodecod, hostdtc, hoendtc, hooccur, hostdy,hoendy)%>% 
    mutate(hostdtc=substr(hostdtc,1, 10))%>%
    mutate(hostdtc=as_date(hostdtc))%>%
    mutate(hoendtc=substr(hoendtc,1, 10))%>%
    mutate(hoendtc=as_date(hoendtc))
  
  last_ho_datea<-icu%>%
    filter(hooccur==TRUE)%>%
    filter(hostdtc >= "2020-01-01"|hostdtc<date_pull )%>%
    arrange(desc(hostdtc))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid,hostdtc)      
  
  last_ho_dates<-icu%>%
    filter(hooccur==TRUE)%>%
    filter(hoendtc>= "2020-01-01"|hoendtc<date_pull)%>%
    arrange(desc(hoendtc))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid,hoendtc)%>%
    left_join(last_ho_datea, by = c("usubjid"))%>%
    mutate(date_ho_last=case_when(is.na(hoendtc) ~ hostdtc,
                                  is.na(hostdtc) ~ hoendtc,
                                  hostdtc>hoendtc ~ hostdtc,
                                  hostdtc<=hoendtc ~ hoendtc))%>% 
    select(usubjid,date_ho_last)
  
  icu <-icu%>%
    filter(hodecod=="INTENSIVE CARE UNIT")%>%
    arrange(desc(hostdtc))%>%
    distinct(usubjid, .keep_all =T)%>%
    rename(ever_icu=hooccur)%>%
    rename(icu_in=hostdtc)%>%
    mutate(icu_in=as_date(icu_in))%>%
    mutate(icu_in=replace(icu_in,icu_in < "2020-01-01" | icu_in >date_pull,NA))%>%
    rename(icu_out=hoendtc)%>%
    mutate(icu_out=as_date(icu_out))%>%
    mutate(icu_out=replace(icu_out,icu_out < "2020-01-01" | icu_out>date_pull,NA))%>%
    select(-c(hodecod))%>%
    full_join(last_ho_dates, by = c("usubjid"))
  
  
  if(dtplyr.step){
    return(icu)
  } else {
    return(icu %>% as_tibble())
  }
}



#' Process data on treatments
#' @param file.name Path of the intervention data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble stringr
#' @return Formatted treatment data (long format) as a tibble or \code{dtplyr_step}
#' @export process.treatment.data
process.treatment.data <- function(file.name,  dtplyr.step = FALSE){
  
   
  #int<-int%>%filter(studyid!="CVZXZMV")
  treatment<-int%>%
    filter(inpresp =="Y") %>%
    filter(inevintx!="BEFORE HOSPITAL ADMISSION")%>%
    mutate(inoccur = case_when(inoccur == "Y" ~ TRUE,
                               inoccur == "N" ~ FALSE,
                               TRUE ~ NA))%>%
    filter(!is.na(inoccur))%>%
    filter(incat!="MEDICAL HISTORY" | is.na (incat))%>%
    mutate(intrt_original=intrt)%>%
    mutate(intrt=toupper(intrt))%>%
    mutate(intrt=as.character(intrt))%>%
    mutate(inmodify=as.character(inmodify))%>%
    mutate(incat=as.character(incat))%>%
    mutate(intrt=case_when(inmodify!=""~inmodify,
                           TRUE ~ intrt))%>%
    mutate(intrt=case_when(incat=="EXTRACORPOREAL"~'EXTRACORPOREAL',
                           incat=="INVASIVE VENTILATION"~'INVASIVE VENTILATION',
                           incat=="FACE MASK"~'NASAL OR MASK OXYGEN THERAPY',
                           incat=="NASAL / MASK OXYGEN THERAPY"~'NASAL OR MASK OXYGEN THERAPY',
                           incat=="INVASIVE VENTILATION"~'INVASIVE VENTILATION',
                           
                           incat=="NON-INVASIVE VENTILATION "~'NON-INVASIVE VENTILATION ',
                           incat=="OTHER INTEVENTIONS"~'OTHER INTERVENTIONS',
                           incat=="PRONE POSITIONING"~'PRONE POSITIONING',
                           incat=="PRONE VENTILATION"~'PRONE VENTILATION',
                           incat=="ANTIBIOTIC AGENTS"~ "ANTIBIOTIC AGENTS",
                           incat=="ANTIFUNGAL AGENTS"~ "ANTIFUNGAL AGENTS",
                           incat=="ANTIVIRAL AGENTS"~ "ANTIVIRAL AGENTS",
                           incat=="CORTICOSTEROIDS"~ "CORTICOSTEROIDS",
                           incat=="ANTIMALARIAL AGENTS"~ "ANTIMALARIAL AGENTS",
                           incat=="NSAIDS"~"NON-STEROIDAL ANTI-INFLAMMATORY (NSAIDS)",
                           TRUE~intrt)) %>%
    mutate(intrt=case_when(intrt%like%'ECMO'~'EXTRACORPOREAL',
                           intrt=='EXTRA CORPOREAL LIFE SUPPORT'~'EXTRACORPOREAL',
                           intrt=='EXTRACORPOREAL SUPPORT'~'EXTRACORPOREAL',
                           intrt=='PRONE POSITIONING WITH UNKNOWN VENTILATION'~'PRONE POSITION VENTILATION',
                           intrt=='PRONE VENTILATION'~'PRONE POSITION VENTILATION',
                           
                           
                           intrt=='CONTINUOUS RENAL REPLACEMENT THERAPIES (CRRT)'~'RENAL REPLACEMENT THERAPIES',
                           intrt%like%'RENAL REPLACEMENT THERAPY' |
                             intrt%like% 'DIALYSIS'~ 'RENAL REPLACEMENT THERAPIES',
                           intrt%like% 'HEMOFILTRATION'~ 'RENAL REPLACEMENT THERAPIES',
                           intrt=='ERP CVVH'~ 'RENAL REPLACEMENT THERAPIES',
                           ###IMV
                           intrt=='INVASIVE MECHANICAL LUNG VENTILATION'~'INVASIVE VENTILATION',
                           intrt=='INVASIVE MECHANICAL VENTILATION'~'INVASIVE VENTILATION',
                           intrt=='MECHANICAL VENTILATION'~'INVASIVE VENTILATION',
                           intrt=='RE-INTUBATION'~'INVASIVE VENTILATION',
                           intrt=='INVASIVE VENTILATION'~'INVASIVE VENTILATION',
                           intrt%like%'APRV'~'INVASIVE VENTILATION',
                           intrt=='INTUBATION AND MECHANICAL VENTILATION'~'INVASIVE VENTILATION',
                           intrt=='MECHANICAL SUPPORT'~'INVASIVE VENTILATION',
                           intrt%like%'EXTUBATION'~'INVASIVE VENTILATION',
                           intrt=="VENTILATED"~'INVASIVE VENTILATION',
                           
                           ###NIV
                           intrt%like%'CPAP'~'NON-INVASIVE VENTILATION',
                           intrt%like%'BIPAP'~'NON-INVASIVE VENTILATION',
                           intrt%like%'NON-INVASIVE MECHANICAL VENTILATION (BIPAP, CPAP, OCNAF (OPTIFLOW) ...)'~'NON-INVASIVE VENTILATION',
                           intrt%like%'NON-INVASIVE VENTILATION'~'NON-INVASIVE VENTILATION',
                           intrt=='NON-INVASIVE MECHANICAL VENTILATION'~'NON-INVASIVE VENTILATION',
                           intrt=='NON-INVASIVE POSITIVE PRESSURE VENTILATION'~'NON-INVASIVE VENTILATION',
                           intrt=='NON-INVASIVE RESPIRATORY SUPPORT'~'NON-INVASIVE VENTILATION',
                           TRUE ~ intrt))%>%
    mutate(intrt=case_when(intrt%like%'OTHER INTERVENTION'~'OTHER INTERVENTIONS',
                           intrt%like%'CHEMOTHERAPY'| intrt%like%'ANTI-DIABETIC MEDICATIONS'|intrt%like%'BRONCHOSCOPY'|
                             intrt%like%'PROTON PUMP INHIBITORS'|intrt%like%'STATINS'|intrt%like%'MORPHINE'|
                             intrt%like%'HALOPERIDOL'|intrt%like%'OLANZAPINE'~'OTHER INTERVENTIONS',
                           
                           intrt=='OTHER TARGETED COVID-19 MEDICATIONS'~'OTHER INTERVENTIONS',
                           intrt=='OTHER TREATMENTS FOR COVID19'~'OTHER INTERVENTIONS',
                           intrt%like%"NON-STEROIDAL"~"NON-STEROIDAL ANTI-INFLAMMATORY",
                           intrt%like%"NON STEROIDAL"~"NON-STEROIDAL ANTI-INFLAMMATORY",                           
                           TRUE ~ intrt))%>%
    
    mutate(intrt=case_when(intrt=='NASAL CANULA'|intrt=='NASAL CANNULA'~'NASAL OXYGEN THERAPY',
                           intrt%like%'SURGICAL FEEDING TUBE'~'TOTAL PARENTERAL NUTRITION',
                           
                           intrt=='FACE MASK'~'MASK OXYGEN THERAPY',
                           
                           ####HFNC
                           intrt=='OXYGEN THERAPY WITH HIGH FLOW NASAL CANULA'~'HIGH-FLOW NASAL CANULA OXYGEN THERAPY',
                           intrt=='HIGH-FLOW NASAL CANNULA OXYGEN THERAPY'~'HIGH-FLOW NASAL CANULA OXYGEN THERAPY',
                           
                           ###Prone positioning
                           intrt%like%'PRONACI'~'PRONE POSITIONING',
                           intrt=='PRONE POSITIONING'~'PRONE POSITIONING',
                           
                           
                           intrt%like%'TRACHEOSTOMY'~'TRACHEOSTOMY',
                           intrt%like%'NITRIC OXIDE'~'INHALED NITRIC OXIDE',
                           
                           ###Corticosteroids
                           intrt=="CORTICOSTEROID"~ "CORTICOSTEROIDS",
                           intrt=="DEXAMETHASONE"~ "CORTICOSTEROIDS",
                           intrt=="BETAMETHASONE"~ "CORTICOSTEROIDS",
                           intrt%like%"PREDNISOLONE"~ "CORTICOSTEROIDS",
                           intrt=="ORAL STEROIDS"~ "CORTICOSTEROIDS",
                           intrt=="STEROIDS"~ "CORTICOSTEROIDS",
                           intrt=="STEROIDS"~ "convalescent_plasma",
                           intrt%like%"HYDROCORTISONE"~ "CORTICOSTEROIDS",
                           
                           intrt%like%"BLOOD TRANSFUSION OR BLOOD PRODUCT"~ "BLOOD TRANSFUSION OR BLOOD PRODUCT",
                           TRUE ~ intrt))%>%
    mutate(intrt=case_when(intrt%like%"ANTIVIRAL" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"ARV" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"ANTIRETROVIRAL" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"RIBAVIRIN" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"LOPINAVIR AND RITONAVIR" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"LOPINAVIR" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"OSELTAMIVIR" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"REMDESIVIR" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"REMDESIVIR" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"NEURAMINIDASE INHIBITORS" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"ZANAMIVIR" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"RIBAVARIN" ~ "ANTIVIRAL AGENTS",
                           intrt%like%"FLUCLOXACILLIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"ANTIBIOTIC"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"AMIKACIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"AMOX"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"AUGUMENTIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"AZITHROMYCIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"AZITHRYOMYCIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"BENZY"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"AUGUMENTIN"~ "ANTIBIOTIC AGENTS",
                           TRUE ~ intrt))%>%
    mutate(intrt=case_when(intrt%like%"AZITHRYOMYCIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"CEFTR"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"CEFR"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"DOXYCYCLINE"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"CHLORAMPHENICOL"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"CIPROFLOXACIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"GENTAMICIN"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"MEROPENEM"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"METRONIDAZOLE"~ "ANTIBIOTIC AGENTS",
                           intrt%like%"ANTIMALARIAL" | intrt%like%"CHLOROQUINE" ~ "ANTIMALARIAL AGENTS",
                           intrt%like%"ANTIFUNGAL" ~ "ANTIFUNGAL AGENTS",
                           intrt %like% "OROGASTRIC"~"NASO/ NASOGASTRIC ORAL/OROGASTRIC FLUIDS",
                           intrt %like% "NGT OR OGT REQUIRED FOR NUTRITION"~"NASO/ NASOGASTRIC ORAL/OROGASTRIC FLUIDS",
                           intrt%like%'DOBUTAMINE' |  intrt%like%'DOPAMINE' |  intrt%like%'MILRINONE' 
                           |  intrt%like%'LEVOSIMENDAN' |  intrt%like%'EPINEPHRINE' |  intrt%like%'NOREPINEPRINE'
                           |  intrt%like%'INOTROPES' |intrt%like%'VASOPRESS' |intrt%like%'NORADRENALINE' |
                             intrt%like%'ADRENALINE' |intrt%like%'BETA BLOCKER' ~'INOTROPES / VASOPRESSORS',
                           TRUE ~ intrt))%>%
    mutate(intrt=case_when(intrt%like%'IMMUNOGLOBULI' ~ "convalescent_plasma",
                           intrt%like%'IMMUNOSUPPRES' ~ "IMMUNOSUPPRESSANTS",
                           intrt%like%'IMMUNOSTIMULANTS' ~ "IMMUNOSUPPRESSANTS",
                           intrt%like%'IMMUNOTHERAPY' ~ "IMMUNOSUPPRESSANTS",
                           intrt=="IL6 INHIBITOR" ~ "IMMUNOSUPPRESSANTS",
                           intrt=="TOCILIZUMAB" ~ "IMMUNOSUPPRESSANTS",
                           intrt%like%"INTERFERON" ~ "IMMUNOSTIMULANTS",
                           
                           intrt%like%"HEPARIN" ~ "THERAPEUTIC ANTICOAGULANT",
                           intrt%like%"NOXAPARIN" ~ "THERAPEUTIC ANTICOAGULANT",
                           intrt=="ENOXAPARIN" ~ "THERAPEUTIC ANTICOAGULANT",
                           
                           intrt%like%"SPIRONOLACTONE" ~ "DIURETICS",
                           intrt%like%"DIURETIC" ~ "DIURETICS",
                           
                           intrt%like%"NITROUS OXIDE" ~ "inhaled_nitric_oxide",
                           intrt=="CPR" ~ "Cardiopulmonary resuscitation",
                           
                           intrt%like%"EXPERIMENTAL AGENT" ~ "EXPERIMENTAL AGENTS",
                           intrt%like%"SARILUMAB" ~ "EXPERIMENTAL AGENTS",
                           intrt%like%"IV FLUID" ~ "INTRAVENOUS FLUIDS",
                           intrt%like%"I.V. SOLUTIONS" ~ "INTRAVENOUS FLUIDS",
                           intrt %like% "ANGIOTENSIN" | intrt %like% "ACE"~ "AGENTS ACTING ON THE RENIN-ANGIOTENSIN SYSTEM",
                           intrt%like%"ANTIINFLAMMATORY" ~ "ANTIINFLAMMATORY",
                           TRUE ~ intrt))%>%    as.data.frame()%>%
    select(studyid,usubjid,'treatment'=intrt,inoccur,intrt_original,inmodify,incat, inevintx, 
           indur,indtc,instdtc,inendtc,indy)%>%
    mutate(treatment = iconv(treatment, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(treatment = str_remove_all(treatment, "\\s*\\([^)]*\\)")) %>%
    mutate(treatment = str_replace_all(treatment, " - ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "-", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "/| / ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, " ", "_"))
  
  
 
 
  if(dtplyr.step){
    return(treatment)
  } else {
    return(treatment %>% as_tibble())
  }
}

#' Process data on the most common treatments
#' @param input Either the path of the interventions data file (CDISC format) or output of \code{process.treatment.data}
#' @param minimum The minimum number of times a treatment need appear to be considered "common"; default 1000.
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble dtplyr tidyfast
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted common treatment data (wide format) as a tibble or \code{dtplyr_step}
#' @export process.common.treatment.data

process.common.treatment.data <- function(file.name, minimum=10, dtplyr.step = FALSE){
  
  date_in_last <- imp_int %>% 
    filter(inoccur==TRUE)%>% 
    mutate(date_in_last=substr(indtc,1, 10))%>%
    mutate(date_in_last=as_date(date_in_last))%>%
    filter(date_in_last >= "2020-01-01"| date_in_last<date_pull)%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, date_in_last )
  
  treatment <- imp_int%>%
    #filter(!is.na(indtc))%>%
    group_by(treatment)%>% 
    arrange(desc(inoccur))%>%
    mutate(n = sum(!is.na(inoccur)))%>%
    filter(n >= eval(!!minimum))%>%
    ungroup()%>%
    filter(treatment!="extracorporeal" & 
             treatment!="inhaled_nitric_oxide" &
          treatment!="oxygen_therapy" &
           treatment!="prone_position_ventilation" &
            treatment!="prone_ventilation" &
           treatment!="respiratory_support" &
            treatment!="tracheostomy" &
           treatment!="prone_positioning")%>%
    #mutate(treatment=replace(treatment,treatment=="cpr","cardiopulmonary_resuscitation"))%>%
    filter(treatment!="covid_19_vaccination")%>%
    filter(treatment!="supplemental_oxygen_fio2")%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid, treatment, .keep_all =T)%>% 
    #mutate(treatment = paste0("treat_",treatment)) %>%
    mutate(treatment = glue("treat_{treatment}", treatment = treatment))%>%
    as.data.table()%>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = inoccur)%>%
    as.data.frame()%>%
    full_join(date_in_last)
  
  ####calculating oxygen therapy overall
  treat_oxy <- imp_int%>%
    mutate(treatment=case_when(treatment=="extracorporeal" | 
             treatment=="inhaled_nitric_oxide" |
             treatment=="prone_position_ventilation" |
             treatment=="respiratory_support" |
             treatment=="tracheostomy" |
               treatment=="high_flow_nasal_cannula" |
               treatment=="invasive_ventilation" |
               treatment=="mask_oxygen_therapy" |
               treatment=="nasal_oxygen_therapy" |
               treatment=="non_invasive_ventilation"~"treat_oxygen_therapy",
             TRUE~treatment))%>%
    filter(treatment=="treat_oxygen_therapy")%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid, treatment, .keep_all =T)%>%select(usubjid,"treat_oxygen_therapy"=inoccur)
  
  
  ###adding duration for inasive_ventilation and non_invasive_ventilation
  
  indur<-imp_int%>%select(usubjid,treatment, inoccur,indur,indtc,instdtc,inendtc,indy)%>%
    filter(treatment=="invasive_ventilation"|treatment=="non_invasive_ventilation")%>%
    mutate(treatment=case_when(treatment=='non_invasive_ventilation'~'dur_niv',
                               treatment=='invasive_ventilation'~'dur_imv',
                               TRUE~treatment))%>%
    mutate(indur_clean=as.numeric(gsub("[^0-9.]", "",indur)))%>%
    filter(!is.na(indur_clean)  | indur_clean!="")%>%
    distinct(usubjid,treatment, .keep_all =T)%>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = indur_clean)%>%
    as_tibble()
  
  
  
  vent_st_instdtc<-imp_int%>%select(usubjid,treatment, inoccur,indur,indtc,instdtc)%>%
    filter(treatment=="invasive_ventilation"|treatment=="non_invasive_ventilation")%>%
    mutate(treatment=case_when(treatment=='non_invasive_ventilation'~'date_niv_st',
                               treatment=='invasive_ventilation'~'date_imv_st',
                               TRUE~treatment))%>%
    filter(inoccur==TRUE)%>%
    mutate(instdtc=substr(instdtc,1, 10))%>%
    mutate(instdtc=as_date(instdtc))%>%
    filter(!is.na(instdtc))%>%
    arrange(instdtc)%>%
    distinct(usubjid,treatment, .keep_all =T)%>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = instdtc)%>%
    as_tibble()

  vent_st_indtc<-imp_int%>%
    filter(inevintx=="00:00-24:00 ON DAY OF ASSESSMENT")%>%
    filter(treatment=="invasive_ventilation"|treatment=="non_invasive_ventilation")%>%
    mutate(treatment=case_when(treatment=='non_invasive_ventilation'~'date_niv_indtc_st',
                               treatment=='invasive_ventilation'~'date_imv_indtc_st',
                               TRUE~treatment))%>%
    filter(inoccur==TRUE)%>%
    mutate(indtc=substr(indtc,1, 10))%>%
    mutate(indtc=as_date(indtc))%>%
    filter(!is.na(indtc))%>%
    arrange(instdtc)%>%
    distinct(usubjid,treatment, .keep_all =T)%>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = indtc)%>%
    as_tibble()
    


    treatment <-treatment%>%
    full_join(treat_oxy)%>%
    full_join(indur)%>%
    full_join(vent_st_instdtc)%>%
    full_join(vent_st_indtc)%>%
    #full_join(vent_at_adm)%>%
    mutate(date_imv_st=case_when(is.na(date_imv_st)~date_imv_indtc_st,
                                 TRUE~date_imv_st))%>%
    select(-c(date_imv_indtc_st))%>%
    rename("date_niv_st"=date_niv_indtc_st)
  
  if(dtplyr.step){
    return(treatment) %>% lazy_dt(immutable = FALSE)
  } else {
    return(treatment %>% as_tibble())
  }
  
}

#' Process data on the most common icu treatments
#' @param input Either the path of the interventions data file (CDISC format) or output of \code{process.treatment.data}
#' @param minimum The minimum number of times a treatment need appear to be considered "common"; default 1000.
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble dtplyr tidyfast
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted common treatment data (wide format) as a tibble or \code{dtplyr_step}
#' @export process.common.treatment.data

process.treatment.icu.data <- function(file.name,imp_icu,imp_dm,imp_ds, minimum=10, dtplyr.step = FALSE){
  
  adm_date<-imp_dm%>%
    select(usubjid,date_admit)
  
  #tabindy_icu<-tabyl(imp_treat_icu$indy)
  
  icu_ever<-imp_icu%>%
    filter(ever_icu==TRUE)%>%
    filter(!is.na(icu_in))%>%
    left_join(adm_date)%>%
    left_join(imp_ds)%>%
    mutate(icu_dy_in=icu_in-date_admit+1)%>%
    mutate(icu_dy_in=as.double(icu_dy_in))%>%
    mutate(icu_dy_in=case_when(icu_dy_in<0~NA_real_,
                               TRUE~icu_dy_in))%>%
    mutate(icu_dy_out=case_when(!is.na(icu_out)~icu_dy_in+(icu_out-icu_in),
                                is.na(icu_out)&!is.na(date_ho_last)~icu_dy_in+(date_ho_last-icu_in),
                                is.na(icu_out)&is.na(date_ho_last)~icu_dy_in+(date_outcome-icu_in),
                                #is.na(icu_out)&is.na(date_ho_last)~icu_dy_in+(date_outcome-icu_in),
                                TRUE~NA_real_))%>%
    mutate(icu_dy_out=as.integer(icu_dy_out))%>%
    mutate(hoendy=case_when(is.na(hoendy)~icu_dy_out,
                            TRUE~hoendy))%>%
    select(usubjid,hostdy,hoendy)#%>%
    #filter(!is.na(hoendy)& hoendy>-1 &!is.na(hostdy)& hostdy>-1)%>%
    #filter(!is.na(hoendy) & !is.na(hostdy))%>%
    #left_join(imp_int)%>%
  treat_oxy <- imp_int%>%
    mutate(treatment=case_when(treatment=="extracorporeal" | 
                                 treatment=="inhaled_nitric_oxide" |
                                 treatment=="prone_position_ventilation" |
                                 treatment=="respiratory_support" |
                                 treatment=="tracheostomy" |
                                 treatment=="high_flow_nasal_cannula" |
                                 treatment=="invasive_ventilation" |
                                 treatment=="mask_oxygen_therapy" |
                                 treatment=="nasal_oxygen_therapy" |
                                 treatment=="non_invasive_ventilation"~"treat_oxygen_therapy",
                               TRUE~treatment))%>%
    filter(treatment=="treat_oxygen_therapy")%>%
    arrange(desc(inoccur))%>%
    left_join(icu_ever,by = c("usubjid"))%>%
    mutate(indy=as.numeric(indy))%>%
    mutate(hostdy=as.numeric(hostdy))%>%
    mutate(hoendy=as.numeric(hoendy))%>%
    mutate(int_icu=case_when((indy>=hostdy)~ TRUE, 
                             TRUE ~ FALSE))%>%
    filter(int_icu==TRUE)%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid, treatment, .keep_all =T)%>%
    select(usubjid,"icu_treat_oxygen_therapy"=inoccur)
  
    
  imp_treat_icu<-imp_int%>%
    filter(!is.na(indy))%>%
    group_by(treatment) %>% 
    arrange(desc(inoccur))%>%
    mutate(n = sum(!is.na(inoccur))) %>%
    filter(n >= eval(!!minimum)) %>%
    ungroup()%>%
    mutate(treatment=replace(treatment,treatment=="cpr","cardiopulmonary_resuscitation"))%>%
    filter(treatment!="covid_19_vaccination")%>%
    filter(treatment!="supplemental_oxygen_fio2")%>%
    filter(treatment!="extracorporeal" & 
             treatment!="inhaled_nitric_oxide" &
             treatment!="oxygen_therapy" &
             treatment!="prone_position_ventilation" &
             treatment!="prone_ventilation" &
             treatment!="respiratory_support" &
             treatment!="tracheostomy" &
             treatment!="prone_positioning")%>%
    arrange(desc(inoccur))%>%
    #mutate(indtc=as.Date(indtc))%>%
    #filter(indtc>= "2020-01-01"|indtc<date_pull)%>%
    left_join(icu_ever,by = c("usubjid"))%>%
    #filter((indy>=hostdy)&(indy<=hoendy))%>%
    mutate(indy=as.numeric(indy))%>%
    mutate(hostdy=as.numeric(hostdy))%>%
    mutate(hoendy=as.numeric(hoendy))%>%
    #mutate(int_icu=case_when((indy>=hostdy)&(indy<=hoendy) ~ TRUE, 
     #                        TRUE ~ FALSE))%>%
    mutate(int_icu=case_when((indy>=hostdy)~ TRUE, 
                             TRUE ~ FALSE))%>%
    #mutate(int_icu=case_when(indtc>=icu_in ~ TRUE, 
    #                         TRUE ~ FALSE))%>%
    filter(int_icu==TRUE)%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid, treatment, .keep_all =T)%>%
    mutate(treatment = glue("icu_treat_{treatment}", treatment = treatment)) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = inoccur)%>%
    full_join(treat_oxy)

    
  
  
  if(dtplyr.step){
    return(imp_treat_icu) %>% lazy_dt(immutable = FALSE)
  } else {
    return(imp_treat_icu %>% as_tibble())
  }
  
}



#' Process data on vital sign
#' @param file.name Path of the dispositions data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble dtplyr tidyfast
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted vital sign (wide format) as a tibble or \code{dtplyr_step}
#' @export process.vital.sign.data
process.vital.sign.data <- function(file.name, dtplyr.step = FALSE){
  vital_sign <- vs %>%
    select(usubjid, vstestcd, vscat,vsstresn,vsstresu, vsdtc, vso2src) %>%
    filter(vscat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION" | vscat=="SIGNS AND SYMPTOMS AT ADMISSION")%>%
    
    mutate(vsstresn=as.numeric(vsstresn))%>%
    mutate(vsstresn=case_when(vstestcd=="OXYSAT"& vsstresn< 1~ NA_real_,
                              vstestcd=="OXYSAT"& vsstresn> 100~ NA_real_,
                              
                              vstestcd=="BMI"& vsstresn< 0~ NA_real_,
                              vstestcd=="BMI"& vsstresn> 100~ NA_real_,
                              
                              vstestcd=="DIABP"& vsstresn< 0~ NA_real_,
                              vstestcd=="DIABP"& vsstresn> 300~ NA_real_,
                              
                              vstestcd=="HEIGHT"& vsstresn< 0~ NA_real_,
                              vstestcd=="HEIGHT"& vsstresn> 250~ NA_real_,
                              
                              vstestcd=="HR"& vsstresn< 0~ NA_real_,
                              vstestcd=="HR"& vsstresn> 250~ NA_real_,
                              
                              vstestcd=="MAP"& vsstresn< 0~ NA_real_,
                              vstestcd=="MAP"& vsstresn> 250~ NA_real_,
                              
                              vstestcd=="MUARMCIR"& vsstresn< 0~ NA_real_,
                              vstestcd=="MUARMCIR"& vsstresn> 100~ NA_real_,
                              
                              vstestcd=="PULSE"& vsstresn< 0~ NA_real_,
                              vstestcd=="PULSE"& vsstresn> 250~ NA_real_,
                              
                              vstestcd=="RESP"& vsstresn< 0~ NA_real_,
                              vstestcd=="RESP"& vsstresn> 60~ NA_real_,
                              
                              vstestcd=="SYSBP"& vsstresn< 0~ NA_real_,
                              vstestcd=="SYSBP"& vsstresn> 250~ NA_real_,
                              
                              vstestcd=="TEMP"& vsstresn< 30~ NA_real_,
                              vstestcd=="TEMP"& vsstresn> 44~ NA_real_,
                              
                              vstestcd=="WEIGHT"& vsstresn< 0~ NA_real_,
                              vstestcd=="WEIGHT"& vsstresn> 300~ NA_real_,
                              
                              TRUE~vsstresn))%>%
    filter(!is.na(vsstresn))%>%
    arrange(desc(vsdtc))%>%
    distinct(usubjid,vstestcd, .keep_all =T)%>%
    mutate(vso2src=case_when(vso2src==""&vstestcd=="OXYSAT"~'UNKNOWN',
                             TRUE~vso2src))%>%
    mutate(vso2src= str_replace_all(vso2src, " ", "_"))%>%
    mutate(vstestcd=case_when(vstestcd=="OXYSAT"~paste0(vstestcd,"_",vso2src),
                              TRUE~vstestcd))%>%
    mutate(vstestcd = paste0("vs_",vstestcd)) %>%
    #mutate(vstestcd = glue("vs_{vstestcd}", vstestcd = vstestcd))%>%
    mutate(vstestcd = iconv(vstestcd, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = vstestcd,  values_from = vsstresn)%>%
    as.data.frame() %>%
    mutate(vs_oxysat=case_when(!is.na(vs_oxysat_oxygen_therapy)~vs_oxysat_oxygen_therapy,
                               !is.na(vs_oxysat_room_air)~vs_oxysat_room_air,
                               TRUE~vs_oxysat_unknown))
  
  
  
  if(dtplyr.step){
    return(vital_sign)
  } else {
    return(vital_sign %>% as_tibble())
  }
  
}  


#' Process data on laboratory
#' @param file.name Path of the dispositions data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble dtplyr tidyfast
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted laboratory (wide format) as a tibble or \code{dtplyr_step}
#' @export process.laboratory.data
process.laboratory.data <- function(file.name, dtplyr.step = FALSE){
  laboratory <- lb%>%
    select(usubjid, lbdy, lbtestcd, lbcat,lborres,lbdtc) %>%
    mutate(lborres=replace(lborres,lborres=="",NA))%>%
    mutate(studyid=substr(usubjid,1, 7))%>%
    mutate(lbcat=case_when(lbdy==1 & (studyid=="CVCCPUK"| 
                                        studyid=="CVMEWUS" | 
                                        studyid=="CORE"|
                                        studyid=="CVTDWXD"|
                                        studyid=="CVTTYLU"|
                                        studyid=="CVZXZMV"|
                                        studyid=="CVKBQEI") ~"LABORATORY RESULTS ON ADMISSION",
                           #lbdy==1 & studyid=="CVMEWUS"~"LABORATORY RESULTS ON ADMISSION",
                           TRUE~as.character(lbcat)))%>%
    filter(lbcat=="LABORATORY RESULTS ON ADMISSION")%>%
    filter(lbtestcd=="ALT"|
             lbtestcd=="APTT"|
             lbtestcd=="CRP"|
             lbtestcd=="LYM"|
             lbtestcd=="NEUT"|
             lbtestcd=="PT"|
             lbtestcd=="WBC"|
             lbtestcd=="BILI"|
             lbtestcd=="AST"|
             lbtestcd=="UREAN")%>%
    mutate(lborres=as.numeric(lborres))%>%
    filter(!is.na(lborres))%>%
    arrange(desc(lbdtc))%>%
    distinct(usubjid,lbtestcd, .keep_all =T)%>%
    mutate(lborres=case_when(lbtestcd=="NEUT" & lborres>100 ~ lborres/1000,
                             
                             lbtestcd=="LYM" & lborres>100 ~ lborres/1000,
                             
                             lbtestcd=="WBC" & lborres>100 ~ lborres/1000, 
                             
                             lbtestcd=="ALT" & lborres>2000 ~ NA_real_,
                             lbtestcd=="ALT" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="AST" & lborres>2000 ~ NA_real_,
                             lbtestcd=="AST" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="BILI" & lborres>2000 ~ NA_real_,
                             lbtestcd=="BILI" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="CRP" & lborres>500 ~ NA_real_,
                             lbtestcd=="CRP" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="PT" & lborres>105 ~ NA_real_,
                             lbtestcd=="PT" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="UREAN" & lborres>100 ~ NA_real_,
                             lbtestcd=="UREAN" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="APTT" & lborres>2000 ~ NA_real_,
                             lbtestcd=="APTT" & lborres<0 ~ NA_real_,
                             
                             TRUE ~ lborres ))%>%
    mutate(lborres=case_when(lbtestcd=="NEUT" & lborres>100 ~ NA_real_,
                             lbtestcd=="NEUT" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="LYM" & lborres>100 ~ NA_real_,
                             lbtestcd=="LYM" & lborres<0 ~ NA_real_,
                             
                             lbtestcd=="WBC" & lborres>100 ~ NA_real_,
                             lbtestcd=="WBC" & lborres<0 ~ NA_real_,
                             
                             TRUE ~ lborres ))%>%
    
        mutate(lbtestcd  = paste0("lab_",lbtestcd )) %>%
    #mutate(lbtestcd = glue("lab_{lbtestcd}", lbtestcd = lbtestcd)) %>%
    mutate(lbtestcd = iconv(lbtestcd, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = lbtestcd,  values_from = lborres)
  
  
  if(dtplyr.step){
    return(laboratory)
  } else {
    return(laboratory%>% as_tibble())
  }
  
}  





#' Process data on outcomes
#' @param file.name Path of the dispositions data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble stringr
#' @return Formatted outcome data (long format) as a tibble or \code{dtplyr_step}
#' @export process.outcome.data
process.outcome.data <- function(file.name, dtplyr.step = FALSE){
  outcome <- ds%>%
    select(usubjid, dsterm, "date_outcome" = dsstdtc, dsmodify) %>%
    mutate(date_outcome=substr(date_outcome,1, 10))%>%
    mutate(date_outcome=as_date(date_outcome))%>%
    mutate(date_outcome=replace(date_outcome,date_outcome< "2020-01-01",NA))%>%
    mutate(date_outcome=replace(date_outcome,date_outcome>date_pull,NA))%>%
    mutate(outcome=tolower(dsterm))%>%
    mutate(outcome=case_when(outcome=="palliative"~"transferred",
                             outcome=="transferred to another unit"~"ongoing care",
                             outcome=="Ongoing health care needs NOT related to COVID episode"~"discharge",
                             outcome==""~NA_character_,
                             TRUE~outcome))%>%
    mutate(outcome=case_when(outcome%like%"hospitalis"~"ongoing care",
                             outcome%like%"hospitaliz"~"ongoing care",
                             outcome%like%"ongoing"~"ongoing care",
                             outcome=="in hospital"~"ongoing care",
                             
                             outcome%like%"death"~"death",
                             outcome=="died"~"death",
                             outcome=="deceased"~"death",
                             outcome=="died (non-covid)"~"death",
                             
                             
                             #outcome=="Death In Hospital"~"Death",
                             outcome=="alive"~"discharge",
                             outcome%like%"discharge"~"discharge",
                             outcome%like%"transfer"~"transferred",
                             outcome=="long term care facility"~"transferred",
                             outcome=="quarantine center"~"transferred",
                             outcome=="missing in database"~"unknown outcome",
                             outcome=="unknown"~"unknown outcome",
                             outcome=="not recorded"~"unknown outcome",
                             TRUE ~ outcome))%>%
  group_by(usubjid) %>% 
    mutate(count=1)%>% 
    mutate(n = sum(count)) %>%
    filter(n == 1)%>%
  select(-c(dsterm,dsmodify,n,count))
  
  
  if(dtplyr.step){
    return(outcome)
  } else {
    return(outcome %>% as_tibble())
  }
  
}


