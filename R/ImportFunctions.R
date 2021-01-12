
#' Shared pre-processing of input CSV files
#' @param file.name Path of the data file (CDISC format)
#' @param excluded.columns Columns to be removed
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dtplyr dplyr tibble
#' @importFrom data.table fread
#' @return The contents of \code{file.name} as a tibble or \code{dtplyr_step}
#' @keywords internal
#' @export shared.data.import

date_pull<-as_date("2020-11-30") 

shared.data.import <- function(file.name, 
                               excluded.columns = c("STUDYID", "DOMAIN"),
                               required.columns = character(), 
                               dtplyr.step = FALSE, 
                               immutable = FALSE){
  
  blank.columns <- as.list(rep(NA, length(required.columns)))
  names(blank.columns) <- required.columns
  
  out <- fread(file.name, showProgress = FALSE) 
  
  out <- out %>%
    add_column(out, !!!blank.columns[setdiff(names(blank.columns), names(out))]) %>%
    lazy_dt(immutable = immutable) %>%
    select(-all_of(excluded.columns)) %>%
    rename_all(function(x){tolower(x)})
  if(dtplyr.step){
    return(out)
  } else {
    return(out %>% as_tibble())
  }
}


#' Import demographic data
#' @param file.name Path of the demographics data file (CDISC format)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble 
#' @return Formatted demographic data as a tibble or \code{dtplyr_step}
#' @export import.demographic.data
import.demographic.data <- function(file.name, dtplyr.step = FALSE){
  
  country.lookup <- ISOcodes::ISO_3166_1 %>% as_tibble %>% select(Alpha_3, Name)
  #regexp <- "[[:digit:]]+"  # process string
  
  out <- shared.data.import(file.name,
                            dtplyr.step = TRUE) %>%
    mutate(country = replace(country, country == "", NA)) %>%
    left_join(country.lookup, by = c("country" = "Alpha_3")) %>%
    select(-country) %>%
    rename(country = Name) %>%
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
    mutate(studyid=substr(usubjid,1, 7))%>%
    mutate(siteid_final=invid)%>%
    mutate(siteid_final=case_when(invid=="00741cca_network"~ substr(subjid,1, 12),
                                invid=="227inserm"~ sub("\\-.*", "",subjid),
                                invid=="00689us_nhlbi_peta"~ sub("\\-.*", "",subjid),
                                invid==""~studyid,
                                studyid=="CVPRQTA"~"CVPRQTA",
                                TRUE~invid))%>%
    mutate(sex = case_when(sex == "M" ~ "Male",
                           sex == "F" ~ "Female",
                           TRUE ~ NA_character_)) %>%
    mutate(date_admit=substr(date_admit,1, 10))%>%
    mutate(date_admit=as_date(date_admit))%>%
    mutate(date_admit=replace(date_admit,date_admit >date_pull,NA))%>%
    select(usubjid, studyid, siteid_final, date_admit, age, sex, ethnic, country)
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

date_pull<-as_date("2020-11-30")
import.microb.data <- function(file.name, dtplyr.step = FALSE){
  
  mb<-shared.data.import(file.name, dtplyr.step = TRUE)
  
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
import.symptom.and.comorbidity.data <- function(file.name, minimum=100, dtplyr.step = FALSE){
  
  out <- shared.data.import(file.name, 
                            dtplyr.step = TRUE, 
                            immutable = TRUE) %>% # this will often by used twice, so should be immutable
    select(usubjid, saterm, sacat,  samodify, sapresp, saoccur, sastdtc) %>%
    filter(sacat=="MEDICAL HISTORY" | sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION") %>%
    mutate(sacat=replace(sacat,saterm=="MALNUTRITION","MEDICAL HISTORY"))%>%#temporary correction
    filter( sapresp=="Y") %>%
    mutate(saoccur = case_when(saoccur == "Y" ~ TRUE,
                               saoccur == "N" ~ FALSE,
                               TRUE ~ NA)) %>%
    filter(!is.na(saoccur)) %>%
    mutate(saterm=toupper(saterm))%>%#to add
    mutate(saterm=case_when(saterm=='CARDIAC ARRHYTHMIA'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CARDIAC DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CHORNIC CARDIAC DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CHRONIC HEART DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CONGENITAL CA'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'CONGENTIAL CARDIOPATHY'~'CHRONIC CARDIAC DISEASE',
                            saterm=='CORONARY DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='HEART FAILURE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='OROVALVA DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm=='RHEUMATIC HEART DISEASE'~'CHRONIC CARDIAC DISEASE',
                            saterm%like%'TUBERCULOSIS'~'TUBERCULOSIS',
                            saterm%like%'MALIGNANCY'~'MALIGNANT NEOPLASM',
                            saterm%like%'SPECIFIC CANCERS'~'MALIGNANT NEOPLASM',
                 
                            saterm=='CHRONIC HEMATOLOGICAL DISEASE'~'CHRONIC HEMATOLOGIC DISEASE',
                            saterm=='CHRONIC LIVER DISEASE'~'LIVER DISEASE',
                            
                            saterm%like%'CHRONIC RENAL FAILURE'~'CHRONIC KIDNEY DISEASE',
                            
                            saterm%like%'CHRONIC LUNG DISEASE'~'CHRONIC PULMONARY DISEASE',
                            saterm%like%'CHROMIC PULMONARY DISEASE'~'CHRONIC PULMONARY DISEASE',
                            
                            saterm%like%'CHRONIC NEUROLOGICAL'~'CHRONIC NEUROLOGICAL DISORDER',
                            saterm%like%'CURRENT SMOK'~'SMOKING',
                            saterm%like%'DIABETES'~'DIABETES',
                            saterm=='HISTORY OF PERIPHERAL OR CARDIAC REVASCULARIZATION'~'HISTORY OF PERIPHERAL OR CARDIAC REVASCULARIZATION',
                            saterm=='HISTORY OF SMOKING'~'SMOKING',
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
                            
                            saterm%like%'COUGH'~'COUGH',
                            saterm%like%'COUTH'~'COUGH',
                            saterm%like%'FEVER'~'HISTORY OF FEVER',
                            saterm=='SEIZURE'~'SEIZURES',
                            saterm%like%'TRANSPLANT'~'TRANSPLANTATION',
                            saterm%like%'ANOSMIA'~'LOSS OF SMELL',
                            saterm%like%'AGEUSIA'~'LOSS OF TASTE',
                            saterm=="LOSS OF TASTE OR LOSS OF SMELL"~'LOSS OF SMELL/TASTE',
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
  if(is.character(input)){
    # assume it's a path
    comorbid <- import.symptom.and.comorbidity.data(input, TRUE)
  } else {
    comorbid <- input
    if(is_tibble(comorbid)){
      comorbid <- comorbid %>% as.data.table %>% lazy_dt(immutable = FALSE)
    }
  }
  
  comorbid <- comorbid %>%
    filter(sacat=="MEDICAL HISTORY") %>%
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
  if(is.character(input)){
    # assume it's a path
    symptom <- import.symptom.and.comorbidity.data(input, TRUE)
  } else {
    symptom <- input
    if(is_tibble(symptom)){
      symptom <- symptom %>% as.data.table %>% lazy_dt(immutable = FALSE)
    }
  }
  
  symptom_w <- symptom %>%
    filter(sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION") %>%
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
  
  date_onset<-symptom %>%
    ungroup()%>%
    filter(sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION" & saoccur==TRUE) %>%
    mutate(sastdtc=as.character(sastdtc))%>%
    mutate(sastdtc = replace(sastdtc, sastdtc =="" , NA))%>%
    mutate(sastdtc=substr(sastdtc,1, 10))%>%
    mutate(sastdtc=as_date(sastdtc))%>%
    filter(sastdtc >= "2020-01-01")%>%
    filter(sastdtc < date_pull%>%
    arrange(sastdtc)%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, "date_onset"=sastdtc)
  
  
  symptomatic<-symptom%>%
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
    full_join(symptomatic, by=c("usubjid"))%>%
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
  comorbid_pregnancy <- shared.data.import(file.name, dtplyr.step = TRUE)%>%
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
  icu <- shared.data.import(file.name, dtplyr.step = FALSE) %>%
    mutate(hooccur = case_when(hooccur == "Y" ~ TRUE,
                               hooccur == "N" ~ FALSE,
                               TRUE ~ NA)) %>%
    filter(!is.na (hooccur))%>%
    select(usubjid, hodecod, hostdtc, hoendtc, hooccur)%>% 
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
  
  out <- shared.data.import(file.name,
                            dtplyr.step = TRUE)
  
  treatment<-out%>%
    filter(inpresp =="Y") %>%
    mutate(inoccur = case_when(inoccur == "Y" ~ TRUE,
                               inoccur == "N" ~ FALSE,
                               TRUE ~ NA))%>%
    filter(!is.na(inoccur))%>%
    filter(incat!="MEDICAL HISTORY")%>%
    mutate(intrt=toupper(intrt))%>%
    mutate(intrt=case_when(inmodify!=""~inmodify,
                          TRUE ~ intrt))%>%
    mutate(intrt=case_when(incat=="EXTRACORPOREAL"~'EXTRACORPOREAL',
                           incat=="INVASIVE VENTILATION"~'INVASIVE VENTILATION',
                           incat=="NASAL / MASK OXYGEN THERAPY"~'NASAL / MASK OXYGEN THERAPY',
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
    select(usubjid, "treatment" = intrt, inoccur, indtc, incat) %>%
    mutate(treatment=case_when(treatment%like%'ECMO'~'EXTRACORPOREAL',
                               treatment=='EXTRA CORPOREAL LIFE SUPPORT'~'EXTRACORPOREAL',
                               treatment=='EXTRACORPOREAL SUPPORT'~'EXTRACORPOREAL',
                               
                               treatment=='CONTINUOUS RENAL REPLACEMENT THERAPIES (CRRT)'~'RENAL REPLACEMENT THERAPIES',
                               treatment%like%'RENAL REPLACEMENT THERAPY' |treatment%like% 'DIALYSIS'~ 'RENAL REPLACEMENT THERAPIES',
                               
                               treatment=='INVASIVE MECHANICAL LUNG VENTILATION'~'INVASIVE VENTILATION',
                               treatment=='INVASIVE MECHANICAL VENTILATION'~'INVASIVE VENTILATION',
                               treatment=='RE-INTUBATION'~'INVASIVE VENTILATION',
                               treatment=='INVASIVE VENTILATION'~'INVASIVE VENTILATION',
                               
                               
                               treatment%like%'CPAP'~'NON-INVASIVE VENTILATION',
                               treatment%like%'BIPAP'~'NON-INVASIVE VENTILATION',
                               treatment%like%'NON-INVASIVE MECHANICAL VENTILATION (BIPAP, CPAP, OCNAF (OPTIFLOW) ...)'~'NON-INVASIVE VENTILATION',
                               treatment%like%'NON-INVASIVE VENTILATION'~'NON-INVASIVE VENTILATION',
                               
                               treatment%like%'OTHER INTERVENTION'~'OTHER INTERVENTIONS',
                               treatment=='OTHER TARGETED COVID-19 MEDICATIONS'~'OTHER INTERVENTIONS',
                               treatment=='OTHER TREATMENTS FOR COVID19'~'OTHER INTERVENTIONS',
                               
                               
                               treatment=='OXYGEN THERAPY'~'NASAL / MASK OXYGEN THERAPY',
                               treatment=='OXYGEN THERAPY WITH HIGH FLOW NASAL CANULA'~'HIGH-FLOW NASAL CANULA OXYGEN THERAPY',
                               treatment=='HIGH-FLOW NASAL CANNULA OXYGEN THERAPY'~'HIGH-FLOW NASAL CANULA OXYGEN THERAPY',
                               
                               treatment=='PRONACIÓN'~'PRONE POSITIONING',
                               treatment=='PRONE POSITIONING'~'PRONE POSITIONING',
                               
                               
                               treatment%like%'TRACHEOSTOMY'~'TRACHEOSTOMY',
                               
                               treatment%like%'NITRIC OXIDE'~'INHALED NITRIC OXIDE',
                               
                               
                               
                               treatment=="CORTICOSTEROID"~ "CORTICOSTEROIDS",
                               treatment=="DEXAMETHASONE"~ "CORTICOSTEROIDS",
                               treatment%like%"PREDNISOLONE"~ "CORTICOSTEROIDS",
                               treatment=="ORAL STEROIDS"~ "CORTICOSTEROIDS",
                               
                               treatment%like%"BLOOD TRANSFUSION OR BLOOD PRODUCT"~ "BLOOD TRANSFUSION OR BLOOD PRODUCT",
                               
                               treatment%like%"ANTIVIRAL" ~ "ANTIVIRAL AGENTS",
                               treatment%like%"LOPINAVIR AND RITONAVIR" ~ "ANTIVIRAL AGENTS",
                               treatment%like%"OSELTAMIVIR" ~ "ANTIVIRAL AGENTS",
                               treatment%like%"REMDESIVIR" ~ "ANTIVIRAL AGENTS",
                               treatment%like%"REMDESIVIR" ~ "ANTIVIRAL AGENTS",
                               treatment%like%"NEURAMINIDASE INHIBITORS" ~ "ANTIVIRAL AGENTS",
                               treatment%like%"RIBAVARIN" ~ "ANTIVIRAL AGENTS",
                               
                               
                               
                               treatment%like%"ANTIBIOTIC"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"AMIKACIN"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"AMOX"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"AUGUMENTIN"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"AZITHRYOMYCIN"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"BENZY"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"AUGUMENTIN"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"AZITHRYOMYCIN"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"CEFTR"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"CEFR"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"DOXYCYCLINE"~ "ANTIBIOTIC AGENTS",
                               
                               treatment%like%"CHLORAMPHENICOL"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"CIPROFLOXACIN"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"GENTAMICIN"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"MEROPENEM"~ "ANTIBIOTIC AGENTS",
                               treatment%like%"METRONIDAZOLE"~ "ANTIBIOTIC AGENTS",
                               
                               treatment%like%"ANTIMALARIAL" | treatment%like%"CHLOROQUINE" ~ "ANTIMALARIAL AGENTS",
                               
                               treatment%like%"ANTIFUNGAL" ~ "ANTIFUNGAL AGENTS",
                               
                               treatment %like% "OROGASTRIC"~"NASO/ NASOGASTRIC ORAL/OROGASTRIC FLUIDS",
                               treatment %like% "NGT OR OGT REQUIRED FOR NUTRITION"~"NASO/ NASOGASTRIC ORAL/OROGASTRIC FLUIDS",
                               
                               
                               
                               treatment%like%'DOBUTAMINE' |  treatment%like%'DOPAMINE' |  treatment%like%'MILRINONE' 
                               |  treatment%like%'LEVOSIMENDAN' |  treatment%like%'EPINEPHRINE' |  treatment%like%'NOREPINEPRINE'
                               |  treatment%like%'VASOPRESS' ~'INOTROPES / VASOPRESSORS',
                               
                               treatment%like%'IMMUNOSUPPRES' ~ "IMMUNOSUPPRESSANTS",
                               
                               treatment=="IL6 INHIBITOR" ~ "IMMUNOSUPPRESSANTS",
                               treatment=="TOCILIZUMAB" ~ "IMMUNOSUPPRESSANTS",
                               
                               treatment%like%"INTERFERON" ~ "IMMUNOSTIMULANTS",
                               
                               treatment%like%"HEPARIN" ~ "THERAPEUTIC ANTICOAGULANT",
                               treatment%like%"ENOXAPARINA" ~ "THERAPEUTIC ANTICOAGULANT",
                               
                               treatment%like%"SPIRONOLACTONE" ~ "DIURETICS",
                               
                               
                               treatment%like%"EXPERIMENTAL AGENT" ~ "EXPERIMENTAL AGENTS",
                               treatment%like%"IV FLUID" ~ "INTRAVENOUS FLUIDS",
                               treatment %like% "ANGIOTENSIN" | treatment %like% "ACE"~ "AGENTS ACTING ON THE RENIN-ANGIOTENSIN SYSTEM",
                               TRUE ~ treatment))%>%
    as.data.frame()%>%

    mutate(treatment = iconv(treatment, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(treatment = str_remove_all(treatment, "\\s*\\([^)]*\\)")) %>%
    mutate(treatment = str_replace_all(treatment, " - ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "-", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "/| / ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, " ", "_"))#%>%
  
  
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

process.common.treatment.data <- function(input, minimum=1000, dtplyr.step = FALSE){
  if(is.character(input)){
    # assume it's a path
    treatment_all <- process.treatment.data(input, TRUE)
  } else {
    treatment_all <- input
    if(is_tibble(treatment_all)){
      treatment_all <- treatment_all %>% as.data.table  %>% lazy_dt(immutable = FALSE)
    }
  }
  
  
  date_in_last <- treatment_all %>% 
    filter(inoccur==TRUE)%>% 
    mutate(date_in_last=substr(indtc,1, 10))%>%
    mutate(date_in_last=as_date(date_in_last))%>%
    filter(date_in_last >= "2020-01-01"| date_in_last<date_pull)%>%
    arrange(desc(date_in_last))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, date_in_last )
  
  treatment <- treatment_all %>%
    group_by(treatment) %>% 
    arrange(desc(inoccur))%>%
    mutate(n = sum(!is.na(inoccur))) %>%
    filter(n >= eval(!!minimum)) %>%
    ungroup()%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid, treatment, .keep_all =T)%>% 
    #mutate(treatment = paste0("treat_",treatment)) %>%
    mutate(treatment = glue("treat_{treatment}", treatment = treatment)) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = inoccur)%>%
    as.data.frame()%>%
    full_join(date_in_last)
  
  if(dtplyr.step){
    return(treatment) %>% lazy_dt(immutable = FALSE)
  } else {
    return(treatment %>% as_tibble())
  }
  
}




#' Process dates on IMV and NIV
#' @param input Either the path of the interventions data file (CDISC format) or output of \code{process.treatment.data}
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble dtplyr tidyfast lubridate
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted start (in) and end (out) dates for IMV and NIV treatment (wide format) as a tibble or \code{dtplyr_step}
#' @export process.common.treatment.data

process.IMV.NIV.ECMO.data <- function(input, dtplyr.step = FALSE){
  if(is.character(input)){
    # assume it's a path
    treatment_all <- process.treatment.data(input, TRUE)
    
  } else {
    treatment_all <- input
    if(is_tibble(treatment_all)){
      treatment_all <- treatment_all %>% as.data.table  %>% lazy_dt(immutable = FALSE)
    }
  }
  
  ventilation <- treatment_all %>% 
    filter(treatment == 'non_invasive_ventilation' |
             treatment == 'invasive_ventilation' |
             treatment == "extracorporeal")%>%
    mutate(indtc=substr(indtc,1, 10))%>%
    mutate(indtc=as_date(indtc))%>%
    mutate(indtc=replace(indtc,indtc < "2020-01-01" | indtc >date_pull,NA))%>%
    mutate(treatment=case_when(treatment=='non_invasive_ventilation'~'niv',
                               treatment=='invasive_ventilation'~'imv',
                               TRUE~treatment
    ))%>%
    as_tibble()
  
  
  
  vent_st<-ventilation%>% 
    filter(inoccur==TRUE)%>%
    arrange(indtc)%>%
    distinct(usubjid,treatment, .keep_all =T)%>%
    mutate(treatment = glue("{treatment}_st", treatment = treatment)) %>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = indtc)%>%
    as_tibble()
  
  vent_en<-ventilation%>% 
    filter(inoccur==TRUE)%>%
    arrange(desc(indtc))%>%
    distinct(usubjid,treatment, .keep_all =T)%>% 
    mutate(treatment = glue("{treatment}_en", treatment = treatment)) %>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = indtc)%>%
    as_tibble() 
  
  ventilation <- full_join(vent_st,vent_en)
  
  
  if(dtplyr.step){
    return(ventilation) %>% lazy_dt(immutable = FALSE)
  } else {
    return(ventilation %>% as_tibble())
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
  vital_sign <- shared.data.import(file.name, dtplyr.step = TRUE) %>%
    select(usubjid, vstestcd, vscat,vsstresn,vsstresu, vsdtc, vso2src) %>%
    filter(vscat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION" | vscat=="SIGNS AND SYMPTOMS AT ADMISSION")%>%
    
    mutate(vsstresn=as.numeric(vsstresn))%>%
    mutate(vsstresn=case_when(vstestcd=="OXYSAT"& vsstresn< 1~ NA_real_,
                              vstestcd=="OXYSAT"& vsstresn> 100~ NA_real_,
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
  laboratory <- shared.data.import(file.name, dtplyr.step = TRUE) %>%
    select(usubjid, lbdy, lbtestcd, lbcat,lborres,lbdtc) %>%
    mutate(lborres=replace(lborres,lborres=="",NA))%>%
    mutate(studyid=substr(usubjid,1, 7))%>%
    mutate(lbcat=case_when(lbdy==1 & studyid=="CVCCPUK"~"LABORATORY RESULTS ON ADMISSION",
                           lbdy==1 & studyid=="CVMEWUS"~"LABORATORY RESULTS ON ADMISSION",
                           TRUE~lbcat))%>%
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
                             lbtestcd=="ALT" & lborres>9999 ~ NA_real_, 
                             lbtestcd=="ALT" & lborres<0 ~ NA_real_,
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
  outcome <- shared.data.import(file.name, dtplyr.step = TRUE) %>%
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
                             
                             outcome%like%"death"~"death",
                             outcome=="died"~"death",
                             
                             #outcome=="Death In Hospital"~"Death",
                             outcome%like%"discharge"~"discharge",
                             outcome%like%"transfer"~"transferred",
                             outcome=="long term care facility"~"transferred",
                             outcome=="quarantine center"~"transferred",
                             outcome=="missing in database"~"unknown outcome",
                             outcome=="unknown"~"unknown outcome",
                             TRUE ~ outcome))%>%
    select(-c(dsterm,dsmodify))
  
  
  if(dtplyr.step){
    return(outcome)
  } else {
    return(outcome %>% as_tibble())
  }
  
}


#' Fully process data
#' @param demog.file.name Path of the demographics data file (CDISC format)
#' @param microb.file.name Path of the demographics data file (CDISC format)
#' @param symptoms.file.name Path of the symptoms data file (CDISC format, optional)
#' @param pregnancy.file.name Path of the RP data file (CDISC format, optional)
#' @param minimum.treatments The minimum number of instances of a treatment required for inclusion as a column
#' @param ICU.file.name Path of the healthcare encounters data file (CDISC format, optional)
#' @param treatment.file.name Path of the intervention data file (CDISC format, optional)
#' @param vit_sign.file.name  Path of the VS data file (CDISC format, optional)
#' @param laboratory.file.name Path of the LB data file (CDISC format, optional)
#' @param outcome.file.name Path of the dispositions data file (CDISC format, optional)
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble
#' @return Formatted outcome data as a tibble or \code{dtplyr_step}
#' @export process.all.data
process.all.data <- function(demog.file.name, microb.file.name=NA, symptoms.file.name = NA, pregnancy.file.name = NA,
                             ICU.file.name = NA, treatment.file.name = NA, vit_sign.file.name = NA, 
                             outcome.file.name = NA, laboratory.file.name= NA, minimum.comorb=100, minimum.sympt=100, minimum.treatments = 1000, 
                             dtplyr.step = FALSE){
  
  demographic <- import.demographic.data(demog.file.name, dtplyr.step = FALSE)
  
  if(!is.na(microb.file.name)){
    microb <- import.microb.data(microb.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(microb, by = c("usubjid")) 
  }
  
  if(!is.na(symptoms.file.name)){
    comorb.sympt.temp <-  import.symptom.and.comorbidity.data(symptoms.file.name, dtplyr.step = TRUE)
    
    comorbid <- process.comorbidity.data(comorb.sympt.temp, minimum.comorb, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(comorbid, by = c("usubjid"))
  }
  
  if(!is.na(pregnancy.file.name)){
    comorbid_pregnancy <- process.pregnancy.data(pregnancy.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(comorbid_pregnancy, by = c("usubjid")) 
  }
  
  
  if(!is.na(symptoms.file.name)){
    comorb.sympt.temp <-  import.symptom.and.comorbidity.data(symptoms.file.name, dtplyr.step = TRUE)
    symptom <- process.symptom.data(comorb.sympt.temp, minimum.sympt, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(symptom, by = c("usubjid")) 
  }
  
  
  if(!is.na(treatment.file.name)){
    treatment <- process.common.treatment.data(treatment.file.name, minimum.treatments, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(treatment, by = c("usubjid"))
  }
  
  
  
  
  if(!is.na(ICU.file.name)){
    icu <- process.ICU.data(ICU.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(icu, by = c("usubjid")) 
  }
  
  if(!is.na(treatment.file.name)){
    icu <- process.ICU.data(ICU.file.name, dtplyr.step = FALSE)
    treatment_all <- process.treatment.data(treatment.file.name,  dtplyr.step = FALSE)
    
    icu_ever<-icu%>%
      filter(ever_icu==TRUE)%>%
      filter(!is.na(icu_in))
    #filter(!is.na(icu_out))
    
    icu_treat<-treatment_all%>%
      filter(!is.na(indtc))%>%
      filter(indtc>= "2020-01-01"|indtc<date_pull)%>%
      left_join(icu_ever,by = c("usubjid"))%>%
      mutate(int_icu=case_when(indtc>=icu_in ~ TRUE, 
                               TRUE ~ FALSE))%>%
      filter(int_icu==TRUE)%>%
      arrange(desc(inoccur))%>%
      distinct(usubjid, treatment, .keep_all =T)%>%
      group_by(treatment) %>% 
      arrange(desc(inoccur))%>%
      mutate(n = sum(!is.na(inoccur))) %>%
      filter(n >= eval(1000)) %>%
      ungroup()%>%
      mutate(treatment = glue("icu_treat_{treatment}", treatment = treatment)) %>%
      as.data.table() %>%
      dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = inoccur)
    demographic <- demographic %>%
      left_join(icu_treat, by = c("usubjid"))
  } 
  if(!is.na(treatment.file.name)){
    ventilation <- process.IMV.NIV.ECMO.data(treatment.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
    left_join(ventilation, by = c("usubjid"))
  }
  
  if(!is.na(vit_sign.file.name)){
    vit_sign <- process.vital.sign.data(vit_sign.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(vit_sign, by = c("usubjid"))
  }
  
  if(!is.na(laboratory.file.name)){
    laboratory <- process.laboratory.data(laboratory.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(laboratory, by = c("usubjid"))
  }
  
  if(!is.na(outcome.file.name)){
    outcome <- process.outcome.data(outcome.file.name, dtplyr.step = FALSE)
    demographic <- demographic%>%
      left_join(outcome, by = c("usubjid"))
    
  }
  
  if(dtplyr.step){
    return(demographic)
  } else {
    return(demographic %>% as_tibble())
  }
  
  
}

