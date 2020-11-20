
#' Shared pre-processing of input CSV files
#' @param file.name Path of the data file (CDISC format)
#' @param excluded.columns Columns to be removed
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dtplyr dplyr tibble
#' @importFrom data.table fread
#' @return The contents of \code{file.name} as a tibble or \code{dtplyr_step}
#' @keywords internal
#' 
#' 
#' 
#' 
#' @export shared.data.import
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
  
  out <- shared.data.import(file.name,
                            required.columns = c("SUBJID",
                                                  "USUBJID",
                                                 "AGE",
                                                 "AGEU",
                                                 "SEX",
                                                 "ETHNIC",
                                                 "SITEID",
                                                 "COUNTRY",
                                                 "RFSTDTC",
                                                 "INVID"),
                            dtplyr.step = TRUE) %>%
    select(usubjid,siteid,invid,  rfstdtc, age, ageu, sex, ethnic, country,subjid)%>%
    mutate(country = replace(country, country == "", NA)) %>%
    left_join(country.lookup, by = c("country" = "Alpha_3")) %>%
    select(-country) %>%
    rename(country = Name) %>%
    rename(date_admit=rfstdtc)%>%
    rename(site = invid) %>%
    as.data.frame()%>%
    mutate(age_d=case_when(ageu=="MONTHS"~12,
                           ageu=="YEARS" ~ 1,
                           ageu=="DAYS" ~ 365.25,
                           TRUE~ NA_real_))%>%
    mutate(age2=age/age_d)%>%
    select(-(age))%>%
    rename(age=age2)%>%
    mutate(age=replace(age,age<0,NA))%>%
    mutate(agegp10 = cut(age, right = FALSE, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 120))) %>%
    mutate(agegp5 = cut(age, right = FALSE, breaks = c(0,5, 10,15, 20,25, 30,35, 40,45, 50,55,
                                                       60,65, 70,75, 80,85, 90, 95, 100, 120))) %>%
    mutate(ethnic = iconv(ethnic, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(ethnic = str_remove_all(ethnic, "\\s*\\([^)]*\\)")) %>%
    mutate(ethnic = str_replace_all(ethnic, " - ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, "-", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, "/| / ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, " ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, ",", "_")) %>%
    mutate(ethnic = replace(ethnic, ethnic == "n_a" | ethnic == "na" | ethnic == "", NA))%>%
    mutate(studyid=substr(usubjid,1, 7))%>%
    mutate(CCA_Network=substr(subjid,1, 12))%>%
    separate(subjid, c("siteid_finala","patient"), sep = "-")%>%
    #select(usubjid,CCA_Network, studyid, siteid,siteid_finala,patient, site,  date_admit, age,sex, ethnic, country)%>%
    mutate(siteid_finala=as.character(siteid_finala))%>%
    mutate(siteid_final= case_when(is.na(patient) ~ site,
                                   patient=="" ~ site,
                                  site=="00741cca_network"~ CCA_Network,
                                   TRUE ~ siteid_finala)) %>%
    mutate(siteid_final=replace(siteid_final,studyid=="CVPSICL","QECH"))%>%
    mutate(siteid_final=replace(siteid_final,studyid=="CVTDWXD","CVTDWXD"))%>%
    mutate(siteid_final=paste0("text_",siteid_final))%>%
    mutate(sex = case_when(sex == "M" ~ "Male",
                           sex == "F" ~ "Female",
                           TRUE ~ NA_character_)) %>%
    mutate(date_admit=substr(date_admit,1, 10))%>%
    mutate(date_admit=as_date(date_admit))%>%
    select(usubjid, studyid, siteid_final, date_admit, age, agegp5, agegp10, sex, ethnic, country  )
  
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
import.symptom.and.comorbidity.data <- function(file.name, dtplyr.step = FALSE){

  out <- shared.data.import(file.name, 
                            required.columns = c("USUBJID",
                                                 "SATERM",
                                                 "SACAT",
                                                 "SAPRESP",
                                                 "SAOCCUR",
                                                 "SASTDTC"),
                            dtplyr.step = TRUE, immutable = TRUE) %>% # this will often by used twice, so should be immutable
    select(usubjid, saterm, sacat, sapresp, saoccur, sastdtc) %>%
    mutate(sacat=replace(sacat,saterm=="MALNUTRITION","MEDICAL HISTORY"))%>%#temporary correction
    mutate(sacat=replace(sacat,saterm=="COVID-19 SYMPTOMS","SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION"))%>%#temporary correction
    mutate(saterm=case_when(sacat=="MEDICAL HISTORY"&saterm=="CARDIAC ARRHYTHMIA" ~  "CHRONIC CARDIAC DISEASE",
                            sacat=='MEDICAL HISTORY'&saterm=='CARDIAC ARRHYTHMIA'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CARDIAC ARRHYTHMIA'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CHRONIC CARDIAC DISEASE, INCLUDING CONGENITAL DISEASE (NOT HYPERTENSION)'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CHRONIC CARDIAC DISEASE, INCLUDING CONGENITAL HEART DISEASE (NOT HYPERTENSION)'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CHRONIC HEART DISEASE, INCLUDING CONGENITAL HEART DISEASE (NOT HYPERTENSION)'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CHRONIC HEMATOLOGICAL DISEASE'~'CHRONIC HEMATOLOGIC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CHRONIC LIVER DISEASE'~'LIVER DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CHRONIC LUNG DISEASE (NOT ASTHMA)'~'CHRONIC PULMONARY DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CHRONIC NEUROLOGICAL DISEASE'~'CHRONIC NEUROLOGICAL DISORDER',
                            sacat=='MEDICAL HISTORY'&saterm=='CONGENTIAL CARDIOPATHY'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CORONARY DISEASE'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='CURRENT SMOKER'~'SMOKING',
                            sacat=='MEDICAL HISTORY'&saterm=='CURRENT SMOKING'~'SMOKING',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES - TYPE 1'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES - TYPE 2'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES (ANY) WITH COMPLICATIONS'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES (ANY) WITHOUT COMPLICATIONS'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES MELLITUS'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES MELLITUS TYPE 1'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES MELLITUS TYPE 2'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES WITH COMPLICATIONS'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='DIABETES WITHOUT COMPLICATIONS'~'DIABETES',
                            sacat=='MEDICAL HISTORY'&saterm=='HEART FAILURE'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='HISTORY OF PERIPHERAL OR CARDIAC REVASCULARIZATION'~'HISTORY OF PERIPHERAL OR CARDIAC REVASCULARIZATION',
                            sacat=='MEDICAL HISTORY'&saterm=='HISTORY OF SMOKING'~'SMOKING',
                            sacat=='MEDICAL HISTORY'&saterm=='HIV'~'AIDS/HIV',
                            sacat=='MEDICAL HISTORY'&saterm=='MILD LIVER DISEASE'~'LIVER DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='MODERATE OR SEVERE LIVER DISEASE'~'LIVER DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='OROVALVA DISEASE'~'CHRONIC CARDIAC DISEASE',
                            sacat=='MEDICAL HISTORY'&saterm=='OTHER RELEVANT RISK FACTOR'~'OTHER COMORBIDITIES',
                            sacat=='MEDICAL HISTORY'&saterm=='OTHER RELEVANT RISK FACTORS'~'OTHER COMORBIDITIES',
                            sacat=='MEDICAL HISTORY'&saterm=='OTHER RISK FACTOR'~'OTHER COMORBIDITIES',
                            sacat=='MEDICAL HISTORY'&saterm=='RHEUMATOLOGICAL DISORDERS'~'RHEUMATOLOGIC DISORDER',
                            sacat=='MEDICAL HISTORY'&saterm=='SMOKER'~'SMOKING',
                            sacat=='MEDICAL HISTORY'&saterm=='SMOKER - CURRENT'~'SMOKING',
                            sacat=='MEDICAL HISTORY'&saterm=='SMOKER - FORMER'~'SMOKING - FORMER',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='FEEDING INTOLERANCE (PAEDIATRICS)'~'ANOREXIA',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH - NON-PRODUCTIVE'~'COUGH - NO SPUTUM',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH - PRODUCTIVE'~'COUGH - WITH SPUTUM',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH WITH SPUTUM PRODUCTION'~'COUGH - WITH SPUTUM',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH - WITH HAEMOPTYSIS'~'COUGH BLOODY SPUTUM / HAEMOPTYSIS',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH BLOODY SPUTUM / HAEMOPTYSIS'~'COUGH BLOODY SPUTUM / HAEMOPTYSIS',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH BLOODY SPUTUM/HAEMOPTYSIS'~'COUGH BLOODY SPUTUM / HAEMOPTYSIS',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH WITH BLOODY SPUTUM/HAEMOPTYSIS'~'COUGH BLOODY SPUTUM / HAEMOPTYSIS',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='COUGH WITH HAEMOPTYSIS'~'COUGH BLOODY SPUTUM / HAEMOPTYSIS',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='FEVER'~'HISTORY OF FEVER',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='ANOSMIA'~'LOSS OF SMELL',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='ANOSMIA (LOSS OF SMELL OR TASTE)'~'LOSS OF SMELL',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='AGEUSIA'~'LOSS OF TASTE',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='AGEUSIA (LOSS OF TASTE)'~'LOSS OF TASTE',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='JOINT PAIN'~'MUSCLE ACHES/JOINT PAIN',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='JOINT PAIN (ARTHRALGIA)'~'MUSCLE ACHES/JOINT PAIN',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='MUSCLE ACHES (MYALGIA)'~'MUSCLE ACHES/JOINT PAIN',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='MUSCLE ACHES/JOINT PAIN'~'MUSCLE ACHES/JOINT PAIN',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='OTHER SIGN OR SYMPTOM'~'OTHER SIGNS AND SYMPTOMS',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='LOWER CHEST WALL INDRAWING'~'SHORTNESS OF BREATH',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='RASH'~'SKIN RASH',
                            sacat=='SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION'&saterm=='SKIN ULCERS'~'SKIN ULCERS',
                            TRUE ~ saterm ))%>%
    filter(-c(sacat=='MEDICAL HISTORY'& saterm=='DRINKS BEER')) %>%
    mutate(saterm = iconv(saterm, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(saterm = str_remove_all(saterm, "\\s*\\([^)]*\\)")) %>%
    mutate(saterm = str_replace_all(saterm, " - ", "_")) %>%
    mutate(saterm = str_replace_all(saterm, "/| / ", "_")) %>%
    mutate(saterm = str_replace_all(saterm, " ", "_")) %>%
    distinct(usubjid, saterm, .keep_all =T) 
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
process.comorbidity.data <- function(input, dtplyr.step = FALSE){
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
    filter(sacat=="MEDICAL HISTORY" & sapresp=="Y") %>%
    mutate(saterm = glue("comorbid_{saterm}", .envir = .SD)) %>%
    mutate(saoccur = case_when(saoccur == "Y" ~ TRUE,
                               saoccur == "N" ~ FALSE,
                               TRUE ~ NA)) %>%
    arrange(desc(saoccur))%>%
    distinct(usubjid,saterm, .keep_all =T)%>%
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
process.symptom.data <- function(input, dtplyr.step = FALSE){
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
    filter(sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION" & sapresp=="Y") %>%
    mutate(saterm = glue("symptoms_{saterm}", .envir = .SD)) %>%
    mutate(saoccur = case_when(saoccur == "Y" ~ TRUE,
                               saoccur == "N" ~ FALSE,
                               TRUE ~ NA)) %>%
    arrange(desc(saoccur))%>%
    distinct(usubjid,saterm, .keep_all =T)%>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = saterm, values_from = saoccur) %>%
    as.data.frame()
  
  symptom_onset<-symptom%>%
    filter(sacat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION" & sapresp=="Y") %>%
    mutate(saoccur = case_when(saoccur == "Y" ~ TRUE,
                               saoccur == "N" ~ FALSE,
                               TRUE ~ NA))%>%
    filter(saoccur==TRUE)%>%
    mutate(sastdtc=as.character(sastdtc))%>%
    #as.character(sastdtc)%>%
    mutate(sastdtc = replace(sastdtc, sastdtc =="" , NA))%>%
    mutate(sastdtc=substr(sastdtc,1, 10))%>%
    arrange(sastdtc)%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, "date_onset"=sastdtc)%>%
    as.data.frame()%>%
    mutate(date_onset2=case_when(usubjid=='CVPPNSH_53-3'~'2020-02-06',
                                 usubjid=='CVVECMO_288-015'~'2020-03-10',
                                 usubjid=='CVRAPID_PET026-01128'~'2020-03-11',
                                 usubjid=='CVRAPID_PET053-00508'~'2020-03-13',
                                 usubjid=='CVTDWXD_RD260115'~'2020-03-13',
                                 usubjid=='CVVCORE_030-0025'~'2020-03-15',
                                 usubjid=='CVTDWXD_RD260112'~'2020-03-15',
                                 usubjid=='CVRAPID_PET024-00923'~'2020-03-16',
                                 usubjid=='CVTDWXD_RD130004'~'2020-03-17',
                                 usubjid=='CVRAPID_00711-0008'~'2020-03-19',
                                 usubjid=='CVVCORE_445-5001'~'2020-03-20',
                                 usubjid=='CVVCORE_489-0001'~'2020-03-21',
                                 usubjid=='CVVCORE_292-0012'~'2020-03-21',
                                 usubjid=='CVVCORE_203-0012'~'2020-03-22',
                                 usubjid=='CVVECMO_203-0012'~'2020-03-22',
                                 usubjid=='CVVCORE_512-0012'~'2020-03-23',
                                 usubjid=='CVVCORE_449-0243'~'2020-03-24',
                                 usubjid=='CVVCORE_450-0015'~'2020-03-25',
                                 usubjid=='CVVCORE_450-0012'~'2020-03-26',
                                 usubjid=='CVVCORE_321-0066'~'2020-03-28',
                                 usubjid=='CVVCORE_512-0007'~'2020-04-01',
                                 usubjid=='CVTDWXD_RD270088'~'2020-04-03',
                                 usubjid=='CVPRQTA_394-355'~'2020-04-07',
                                 usubjid=='CVVCORE_321-0303'~'2020-04-10',
                                 usubjid=='CVPRQTA_353-905'~'2020-04-10',
                                 usubjid=='CVVCORE_F199-28'~'2020-04-12',
                                 usubjid=='CVPRQTA_394-366'~'2020-04-12',
                                 usubjid=='CVVCORE_276-0314'~'2020-04-25',
                                 usubjid=='CVVCORE_468-0152'~'2020-04-25',
                                 usubjid=='CVVECMO_548-0040'~'2020-04-25',
                                 usubjid=='CVRAPID_00711-0328'~'2020-04-26',
                                 usubjid=='CVVCORE_321-0431'~'2020-04-28',
                                 usubjid=='CVVECMO_00565-0029'~'2020-05-07',
                                 usubjid=='CVVECMO_00668-0044'~'2020-05-12',
                                 usubjid=='CVRAPID_00570-0028'~'2020-05-17',
                                 usubjid=='CVVCORE_276-0610'~'2020-05-18',
                                 usubjid=='CVVCORE_276-0585'~'2020-05-21',
                                 usubjid=='CVVCORE_118-0031'~'2020-05-21',
                                 usubjid=='CVVCORE_276-0572'~'2020-05-26',
                                 usubjid=='CVVCORE_A-AF-002-003-0059'~'2020-05-26',
                                 usubjid=='CVPRQTA_384-206'~'2020-05-30',
                                 usubjid=='CVRAPID_00570-0103'~'2020-06-09',
                                 usubjid=='CVVCORE_A-AF-026-002-0067'~'2020-06-12',
                                 usubjid=='CVVCORE_A-AF-026-002-0062'~'2020-06-13',
                                 usubjid=='CVVCORE_A-AF-026-002-0039'~'2020-06-19',
                                 usubjid=='CVVECMO_435-039'~'2020-06-21',
                                 usubjid=='CVRAPID_00570-0227'~'2020-06-24',
                                 usubjid=='CVRAPID_00711-0290'~'',
                                 usubjid=='CVPRQTA_388-78'~'',
                                 usubjid=='CVPRQTA_400-17'~'',
                                 usubjid=='CVTDWXD_IC01029'~'',
                                 usubjid=='CVVCORE_00561-0064'~'',
                                 usubjid=='CVVCORE_321-0244'~'',
                                 usubjid=='CVVECMO_5170001'~'',
                                 usubjid=='CVRAPID_218-0079'~'',
                                 usubjid=='CVRAPID_103-0031'~'',
                                 usubjid=='CVRAPID_212-0049'~'',
                                 usubjid=='CVVCORE_00561-0040'~'',
                                 usubjid=='CVVCORE_657-0191'~'',
                                 usubjid=='CVVCORE_538-0050'~'',
                                 usubjid=='CVVCORE_505-0031'~'',
                                 usubjid=='CVVCORE_321-0442'~'',
                                 usubjid=='CVRAPID_216-0076'~'',
                                 usubjid=='CVVCORE_00727-0049'~'',
                                 usubjid=='CVRAPID_005-0089'~'',
                                 usubjid=='CVVCORE_F191-332'~'',
                                 usubjid=='CVPRQTA_394-616'~'',
                                 usubjid=='CVRAPID_433-E087'~'',
                                 usubjid=='CVVCORE_062-B077'~'',
                                 usubjid=='CVVCORE_520-0019'~'',
                                 usubjid=='CVVCORE_449-0053'~'',
                                 usubjid=='CVTDWXD_RD410067'~'',
                                 usubjid=='CVVCORE_F219-9'~'',
                                 usubjid=='CVVCORE_A-AF-007-002-0030'~'',
                                 usubjid=='CVRAPID_212-0132'~'2020-05-07',
                                 usubjid=='CVVCORE_321-0330'~'2020-04-02',
                                 usubjid=='CVVCORE_290-Flevoneg1'~'',
                                 TRUE ~ date_onset))%>%
      mutate(date_onset2=as_date(date_onset2))%>%
      mutate(date_onset=as_date(date_onset))
  symptom<- symptom_w%>%
    left_join(symptom_onset, by = c("usubjid"))
    
  
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
    filter(hooccur=="Y" | hooccur=="N")%>%
    mutate(hooccur = case_when(hooccur == "Y" ~ TRUE,
                               hooccur == "N" ~ FALSE,
                               TRUE ~ NA)) %>%
    select(usubjid, hodecod, hostdtc, hoendtc, hooccur)%>% 
    mutate(hostdtc=substr(hostdtc,1, 10))%>%
    mutate(hostdtc=as_date(hostdtc))%>%
    mutate(hoendtc=substr(hoendtc,1, 10))%>%
    mutate(hoendtc=as_date(hoendtc))
  
    last_ho_datea<-icu%>%
    filter(hooccur==TRUE)%>%
    arrange(desc(hostdtc))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid,hostdtc)      
 
    last_ho_dates<-icu%>%
    filter(hooccur==TRUE)%>%
    arrange(desc(hoendtc))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid,hoendtc)%>%
    left_join(last_ho_datea, by = c("usubjid"))%>%
    mutate(date_ho_last=case_when(is.na(hoendtc) ~ hostdtc,
                                       is.na(hostdtc) ~ hoendtc,
                                       hostdtc>hoendtc ~ hostdtc,
                                       hostdtc<=hoendtc ~ hoendtc)) 
    
  
    icu <-icu%>%
    mutate(hodecod = ifelse(hodecod=="HOSPITAL", "hospital", "icu")) %>%
    filter(hodecod=="icu")%>%
    arrange(desc(hostdtc))%>%
    distinct(usubjid, .keep_all =T)%>%
    rename(ever_icu=hooccur)%>%
    rename(icu_in=hostdtc)%>%
    mutate(icu_in=as.character(icu_in))%>%
    mutate(icu_in2=case_when(usubjid=='CVVCORE_00721-0001'~'2020-03-04',
                             usubjid=='CVVCORE_560-0001'~'2020-03-30',
                             usubjid=='CVRAPID_503-0023'~'2020-04-02',
                             usubjid=='CVVECMO_025-0006'~'2020-04-14',
                             usubjid=='CVVECMO_694-0044'~'2020-04-16',
                             usubjid=='CVVECMO_288-013'~'2020-04-17',
                             usubjid=='CVVECMO_294-0046'~'2020-04-17',
                             usubjid=='CVVCORE_403-005'~'2020-04-19',
                             usubjid=='CVVCORE_498-0020'~'2020-04-25',
                             usubjid=='CVVCORE_321-0445'~'2020-04-27',
                             usubjid=='CVVCORE_468-0152'~'2020-04-29',
                             usubjid=='CVVCORE_498-0019'~'2020-04-30',
                             usubjid=='CVVECMO_00674-0021'~'2020-05-01',
                             usubjid=='CVVCORE_692-0029'~'2020-05-05',
                             usubjid=='CVRAPID_00628-0004'~'2020-05-17',
                             usubjid=='CVVECMO_516-0025'~'2020-05-18',
                             usubjid=='CVTDWXD_RD260219'~'2020-06-11',
                             usubjid=='CVRAPID_00570-0144'~'2020-06-21',
                             usubjid=='CVVECMO_719-0087'~'2020-07-07',
                             usubjid=='CVRAPID_00570-0184'~'2020-07-11',
                             usubjid=='CVRAPID_00570-0155'~'2020-07-13',
                             usubjid=='CVRAPID_00570-0186'~'2020-07-17',
                             usubjid=='CVRAPID_00570-0401'~'2020-07-19',
                             usubjid=='CVRAPID_212-0024'~'',
                             usubjid=='CVRAPID_00570-0126'~'',
                             usubjid=='CVVCORE_538-0067'~'',
                             usubjid=='CVRAPID_00570-0178'~'',
                             usubjid=='CVVECMO_00670-0045'~'2020-03-13',
                             usubjid=='CVVECMO_288-015'~'2020-03-14',
                             usubjid=='CVVCORE_403-002'~'2020-03-17',
                             usubjid=='CVVCORE_291-0005'~'2020-03-17',
                             usubjid=='CVRAPID_PET025-01084'~'2020-03-19',
                             usubjid=='CVVCORE_499-0003'~'2020-03-24',
                             usubjid=='CVVECMO_499-0003'~'2020-03-24',
                             usubjid=='CVVCORE_291-0143'~'2020-03-25',
                             usubjid=='CVRAPID_PET051-00515'~'2020-03-26',
                             usubjid=='CVVCORE_468-0058'~'2020-03-27',
                             usubjid=='CVVCORE_449-0003'~'2020-03-28',
                             usubjid=='CVVECMO_025-0003'~'2020-03-29',
                             usubjid=='CVVECMO_288-044'~'2020-03-29',
                             usubjid=='CVVCORE_445-7048'~'2020-04-12',
                             usubjid=='CVVCORE_062-A034'~'2020-04-14',
                             usubjid=='CVTDWXD_RD390015'~'2020-04-19',
                             usubjid=='CVRAPID_00603-A006'~'2020-04-25',
                             usubjid=='CVRAPID_00711-0689'~'2020-05-07',
                             usubjid=='CVRAPID_00570-0046'~'2020-06-16',
                             usubjid=='CVVECMO_00665-0025'~'2020-06-19',
                             usubjid=='CVVECMO_719-0075'~'2020-06-27',
                             usubjid=='CVVECMO_719-0070'~'2020-07-26',
                             usubjid=='CVVCORE_498-0033'~'',
                             usubjid=='CVVECMO_294-0006'~'',
                                 TRUE ~ icu_in))%>%
              mutate(icu_in2=as_date(icu_in2))%>%
              mutate(icu_in=as_date(icu_in))%>%
    rename(icu_out=hoendtc)%>%
    mutate(icu_out=as.character(icu_out))%>%
    mutate(icu_out2=case_when(usubjid=='CVRAPID_503-0023'~'2020-04-18',
                              usubjid=='CVVCORE_498-0019'~'2020-04-30',
                              usubjid=='CVVECMO_00674-0021'~'2020-05-02',
                              usubjid=='CVVCORE_692-0029'~'2020-05-11',
                              usubjid=='CVRAPID_00628-0004'~'2020-05-18',
                              usubjid=='CVTDWXD_RD260219'~'2020-06-12',
                              usubjid=='CVRAPID_00570-0155'~'2020-07-16',
                              usubjid=='CVRAPID_00570-0186'~'2020-07-25',
                              usubjid=='CVRAPID_00570-0126'~'',
                              usubjid=='CVRAPID_213-0010'~'',
                              usubjid=='CVVCORE_421-A009'~'2020-03-27',
                              usubjid=='CVRAPID_PET041-00330'~'2020-03-28',
                              usubjid=='CVVECMO_719-0010'~'2020-04-01',
                              usubjid=='CVVECMO_719-0011'~'2020-04-02',
                              usubjid=='CVVCORE_00773-0001'~'2020-04-07',
                              usubjid=='CVTDWXD_RD070016'~'2020-04-15',
                              usubjid=='CVVECMO_00656-0005'~'2020-04-17',
                              usubjid=='CVVCORE_321-0317'~'2020-04-28',
                              usubjid=='CVTDWXD_RD410004'~'2020-05-13',
                              usubjid=='CVTDWXD_RD430111'~'2020-05-23',
                              usubjid=='CVTDWXD_RD410012'~'2020-06-04',
                              usubjid=='CVVCORE_512-0070'~'2020-06-04',
                              usubjid=='CVVECMO_00742-0002'~'2020-06-16',
                              usubjid=='CVRAPID_00570-0046'~'2020-06-18',
                              usubjid=='CVVECMO_00624-0061'~'2020-06-18',
                              usubjid=='CVRAPID_00711-0707'~'2020-06-21',
                              usubjid=='CVVCORE_726-0000'~'2020-07-01',
                              usubjid=='CVTDWXD_RD240294'~'2020-07-03',
                              usubjid=='CVVCORE_512-0103'~'2020-07-04',
                              usubjid=='CVRAPID_00570-0092'~'2020-07-06',
                              usubjid=='CVVCORE_00727-0001'~'2020-07-14',
                              usubjid=='CVRAPID_00570-0176'~'2020-07-27',
                              usubjid=='CVVCORE_498-0033'~'',
                              usubjid=='CVVCORE_326-0099'~'',
                              usubjid=='CVVECMO_253-0008'~'',
                              usubjid=='CVVCORE_286-0050'~'2020-03-20',
                              usubjid=='CVVECMO_694-0025'~'2020-04-08',
                              usubjid=='CVVECMO_000743-0002'~'2020-04-17',
                              usubjid=='CVVCORE_00633-0028'~'',
                              usubjid=='CVVCORE_449-0057'~'2020-04-14',
                              usubjid=='CVVCORE_00561-0070'~'',
                              usubjid=='CVVCORE_00555-2003'~'2020-04-14',
                              usubjid=='CVVECMO_00670-0018'~'2020-03-27',
                              usubjid=='CVVCORE_326-0002'~'2020-06-22',
                              usubjid=='CVVCORE_449-0014'~'2020-04-13',
                              usubjid=='CVVECMO_536-0016'~'2020-05-25',
                              usubjid=='CVVCORE_449-0009'~'2020-04-28',
                              usubjid=='CVVCORE_00721-0006'~'2020-03-15',
                              usubjid=='CVVCORE_00721-0010'~'2020-03-14',
                              usubjid=='CVRAPID_PET027-00615'~'2020-03-18',
                              usubjid=='CVVECMO_694-0024'~'2020-04-25',
                              TRUE ~ icu_out))%>%
    mutate(icu_out2=as_date(icu_out2))%>%
    mutate(icu_out=as_date(icu_out))%>%
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
process.treatment.data <- function(file.name, dtplyr.step = FALSE){
  
  out <- shared.data.import(file.name,
                            required.columns = c("USUBJID",
                                                 "INTRT",
                                                 "INOCCUR",
                                                 "INDTC"),
                            dtplyr.step = TRUE)
  
  #treatment <- shared.data.import(file.name, dtplyr.step = TRUE) %>%
  
  treatment_a<-out%>%
    filter(incat == "MEDICATION"  ) %>%
    mutate(studyid=substr(usubjid,1, 7))%>%
    filter(studyid == "CVVCORE" | studyid=="CVVECMO" | studyid=="CVPSICL") %>%
    filter(inpresp =="Y") %>%
    as_tibble() %>%
    dplyr::filter(intrt %like% "ANTIBIOTIC" | intrt %like% "ANTIVIRAL" 
                  | intrt %like% "ANTIFUNG" | intrt %like% "CORTICOSTEROID"
    )%>%
    mutate(intrt=case_when(intrt=="CORTICOSTEROID"~ "CORTICOSTEROIDS",
                           intrt=="ANTIVIRAL" | intrt=="ANTIVIRAL AGENT"~ "ANTIVIRAL AGENTS",
                           intrt=="ANTIBIOTIC" | intrt=="ANTIBIOTIC AGENT"~ "ANTIBIOTIC AGENTS",
                           intrt=="ANTIMALARIAL AGENT" ~ "ANTIMALARIAL AGENTS",
                           intrt=="ANTIFUNGAL AGENT" ~ "ANTIFUNGAL AGENTS"
                           ))%>%
    select(usubjid, "treatment" = intrt, inoccur, indtc)%>%
  as.data.frame()
    

  treatment<-out%>%
    filter(incat == "SUPPORTIVE CARE" | incat == "ANTIBIOTIC AGENTS" | incat == "ANTIFUNGAL AGENTS"
           | incat == "ANTIVIRAL AGENTS" | incat == "CORTICOSTEROIDS" | incat == "ANTIMALARIAL AGENTS") %>%
    filter(inpresp =="Y") %>%
    select(usubjid, "treatment" = intrt, inoccur, indtc, incat) %>%
    mutate(treatment=replace(treatment,incat=="ANTIBIOTIC AGENTS", "ANTIBIOTIC AGENTS"))%>%
    mutate(treatment=replace(treatment,incat=="ANTIFUNGAL AGENTS", "ANTIFUNGAL AGENTS"))%>%
    mutate(treatment=replace(treatment,incat=="ANTIVIRAL AGENTS", "ANTIVIRAL AGENTS"))%>%
    mutate(treatment=replace(treatment,incat=="CORTICOSTEROIDS", "CORTICOSTEROIDS"))%>%
    mutate(treatment=replace(treatment,incat=="ANTIMALARIAL AGENTS", "ANTIMALARIAL AGENTS"))%>%
    mutate(treatment=case_when(incat=='SUPPORTIVE CARE'&treatment=='AV ECLS/ECMO'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='BIPAP'~'NON-INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='CENTRAL ECLS/ECMO'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='CONTINUOUS RENAL REPLACEMENT THERAPIES (CRRT)'~'RENAL REPLACEMENT THERAPIES',
                               incat=='SUPPORTIVE CARE'&treatment=='CPAP'~'NON-INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='DIALYSIS/HEMOFILTRATION'~'RENAL REPLACEMENT THERAPIES',
                               incat=='SUPPORTIVE CARE'&treatment=='DIALYSIS/RENAL TREATMENT'~'RENAL REPLACEMENT THERAPIES',
                               incat=='SUPPORTIVE CARE'&treatment=='DOBUTAMINE'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='DOPAMINE < 5 UG/KG/MIN OR DOBUTAMINE OR MILRINONE OR LEVOSIMENDAN'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='DOPAMINE <5 UG/KG/MIN OR DOBUTAMINE OR MILRINONE OR LEVOSIMENDAN'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='DOPAMINE > 15 UG/KG/MIN OR EPINEPHRINE/NOREPINEPRINE > 0.1. UG/KG/MIN'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='DOPAMINE >15 UG/KG/MIN OR EPINEPHRINE/NOREPINEPHRINE >0.1 UG/KG/MIN'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='DOPAMINE 5-15 UG/KG/MIN OR EPINEPHRINE OR NOREPINEPHRINE < 0.1 UG/KG/MIN OR VASOPRESSIN OR PHENYLEPHRINE'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='DOPAMINE 5-15 UG/KG/MIN OR EPINEPHRINE/NOREPINEPHRINE <0.1 UG/KG/MIN OR VASOPRESSIN OR PHENYLEPHRINE'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='EXTRA CORPOREAL LIFE SUPPORT (ECLS / ECMO)'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='EXTRACORPOREAL (ECMO) SUPPORT'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='EXTRACORPOREAL MEMBRANE OXYGENATION (ECMO)'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='EXTRACORPOREAL MEMBRANE OXYGENATION (ECMO/ECLS)'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='EXTRACORPOREAL SUPPORT'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='EXTRACORPOREAL SUPPORT (ECMO)'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='INVASIVE MECHANICAL LUNG VENTILATION'~'INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='INVASIVE MECHANICAL VENTILATION'~'INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='NON-INVASIVE MECHANICAL VENTILATION (BIPAP, CPAP, OCNAF (OPTIFLOW) ...)'~'NON-INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='NON-INVASIVE VENTILATION'~'NON-INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='OTHER INTERVENTION OR PROCEDURE'~'OTHER INTERVENTIONS',
                               incat=='SUPPORTIVE CARE'&treatment=='OTHER INTERVENTIONS OR PROCEDURES'~'OTHER INTERVENTIONS',
                               incat=='SUPPORTIVE CARE'&treatment=='OTHER NON-INVASIVE VENTILATION TYPE'~'NON-INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='OXYGEN THERAPY'~'NASAL / MASK OXYGEN THERAPY',
                               incat=='SUPPORTIVE CARE'&treatment=='OXYGEN THERAPY WITH HIGH FLOW NASAL CANULA'~'NASAL / MASK OXYGEN THERAPY',
                               incat=='SUPPORTIVE CARE'&treatment=='PRONACIÓN'~'PRONE POSITIONING',
                               incat=='SUPPORTIVE CARE'&treatment=='PRONE POSITIONING'~'PRONE POSITIONING',
                               incat=='SUPPORTIVE CARE'&treatment=='RE-INTUBATION'~'INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='RENAL REPLACEMENT THERAPY (RRT) OR DIALYSIS'~'RENAL REPLACEMENT THERAPIES',
                               incat=='SUPPORTIVE CARE'&treatment=='RENAL REPLACEMENT THERAPY OR HEMODIALYSIS'~'RENAL REPLACEMENT THERAPIES',
                               incat=='SUPPORTIVE CARE'&treatment=='TRACHEOSTOMY'~'TRACHEOSTOMY',
                               incat=='SUPPORTIVE CARE'&treatment=='TRACHEOSTOMY INSERTED'~'TRACHEOSTOMY',
                               incat=='SUPPORTIVE CARE'&treatment=='UNKNOWN NON-INVASIVE VENTILATION TYPE'~'NON-INVASIVE VENTILATION',
                               incat=='SUPPORTIVE CARE'&treatment=='UNKNOWN TYPE ECLS/ECMO'~'EXTRACORPOREAL',
                               incat=='SUPPORTIVE CARE'&treatment=='VASOPRESSIN'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='VASOPRESSOR/INOTROPIC SUPPORT'~'INOTROPES / VASOPRESSORS',
                               incat=='SUPPORTIVE CARE'&treatment=='VV ECLS/ECMO'~'EXTRACORPOREAL',
                               TRUE ~ treatment))%>%
    select(usubjid, treatment, inoccur, indtc) %>%
    as.data.frame()%>%
    bind_rows(treatment_a)%>%
    mutate(treatment = iconv(treatment, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(treatment = str_remove_all(treatment, "\\s*\\([^)]*\\)")) %>%
    mutate(treatment = str_replace_all(treatment, " - ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "-", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "/| / ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, " ", "_")) %>%
    mutate(inoccur = case_when(inoccur == "Y" ~ TRUE,
                               inoccur == "N" ~ FALSE,
                               TRUE ~ NA))%>%
    filter(!is.na(inoccur))%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid,treatment, .keep_all =T) 
  
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

process.common.treatment.data <- function(input, minimum = 100, dtplyr.step = FALSE){
  if(is.character(input)){
    # assume it's a path
    treatment_all <- process.treatment.data(input, TRUE)
  } else {
    treatment_all <- input
    if(is_tibble(treatment_all)){
      treatment_all <- treatment_all %>% as.data.table  %>% lazy_dt(immutable = FALSE)
    }
  }
  
  treatment <- treatment_all %>%
    arrange(desc(inoccur))%>%
    distinct(usubjid, treatment, .keep_all =T)%>% 
    group_by(treatment) %>% 
    arrange(desc(inoccur))%>%
    mutate(n = sum(!is.na(inoccur))) %>%
    filter(n >= eval(!!minimum)) %>%
    mutate(treatment = glue("treat_{treatment}", treatment = treatment)) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = inoccur) 
  
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
process.IMV.NIV.data <- function(input, dtplyr.step = FALSE){
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
    as_tibble() %>%
    dplyr::filter(treatment %like% "ventilation")%>%
    mutate(indtc=substr(indtc,1, 10))%>%
    mutate(indtc=as_date(indtc))%>%
    #filter(!is.na(indtc))%>%
    as_tibble() %>%
    dplyr::mutate(vent=ifelse(treatment %like% "non", "niv", "imv"))%>%
    select(-(treatment))
  
  vent_ever <- ventilation %>%
    filter(vent=="imv" | vent=="niv")%>%
    arrange(desc(inoccur))%>%
    distinct(usubjid,vent, .keep_all =T)%>%
    mutate(vent = glue("ever_{vent}", vent = vent)) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = vent,  values_from = inoccur)%>%
    as.data.frame()
  
  niv_st<-ventilation%>% 
    filter(inoccur==TRUE & vent=="niv")%>%
    arrange(indtc)%>%
    distinct(usubjid,vent, .keep_all =T)%>%
    select(usubjid, "niv_st"=indtc)
  
  niv_en<-ventilation%>% 
    filter(inoccur==TRUE & vent=="niv")%>%
    arrange(desc(indtc))%>%
    distinct(usubjid,vent, .keep_all =T)%>%
    select(usubjid, "niv_en"=indtc)
 
   imv_st<-ventilation%>% 
    filter(inoccur==TRUE & vent=="imv")%>%
    arrange(indtc)%>%
    distinct(usubjid,vent, .keep_all =T)%>%
    select(usubjid, "imv_st"=indtc)
  
  imv_en<-ventilation%>% 
    filter(inoccur==TRUE & vent=="imv")%>%
    arrange(desc(indtc))%>%
    distinct(usubjid,vent, .keep_all =T)%>%
    select(usubjid, "imv_en"=indtc)
  
  ventilation<-vent_ever%>%
    left_join(niv_st, by = c("usubjid"))%>%
    left_join(niv_en, by = c("usubjid"))%>%
    left_join(imv_st, by = c("usubjid"))%>%
    left_join(imv_en, by = c("usubjid"))

  if(dtplyr.step){
    return(ventilation) %>% lazy_dt(immutable = FALSE)
  } else {
    return(ventilation %>% as_tibble())
  }    
  
}


#' Process dates latest treatment date
#' @param input Either the path of the interventions data file (CDISC format) or output of \code{process.treatment.data}
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble dtplyr tidyfast lubridate
#' @importFrom data.table as.data.table
#' @importFrom glue glue
#' @return Formatted start (in) and end (out) dates for IMV and NIV treatment (wide format) as a tibble or \code{dtplyr_step}
#' @export process.common.treatment.data
process.treatment.dates.data <- function(input, dtplyr.step = FALSE){
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
    arrange(desc(date_in_last))%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, date_in_last )
  
  
  if(dtplyr.step){
    return(date_in_last) %>% lazy_dt(immutable = FALSE)
  } else {
    return(date_in_last %>% as_tibble())
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
    select(usubjid, vstestcd, vscat,vsstresn,vsstresu, vsdtc) %>%
    filter(vscat=="SIGNS AND SYMPTOMS AT HOSPITAL ADMISSION" | vscat=="SIGNS AND SYMPTOMS AT ADMISSION")%>%
    filter(vstestcd=="HR" |
             vstestcd=="OXYSAT" |
             vstestcd=="RESP" |
             vstestcd=="SYSBP" |
             vstestcd=="TEMP")%>%
    mutate(vsstresn=as.numeric(vsstresn))%>%
    filter(!is.na(vsstresn))%>%
    arrange(desc(vsdtc))%>%
    distinct(usubjid,vstestcd, .keep_all =T)%>%
    mutate(vstestcd = glue("vs_{vstestcd}", vstestcd = vstestcd))%>%
    mutate(vstestcd = iconv(vstestcd, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = vstestcd,  values_from = vsstresn)%>%
    as.data.frame() %>%
    mutate(vs_oxysat=replace(vs_oxysat,vs_oxysat< 1 | vs_oxysat> 100, NA))

  
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
    select(usubjid, lbtestcd, lbcat,lborres,lbdtc) %>%
    filter(lbcat=="LABORATORY RESULTS ON ADMISSION")%>%
    filter(lbtestcd=="ALT"|
             lbtestcd=="APTT"|
             lbtestcd=="CRP"|
             lbtestcd=="LYM"|
             lbtestcd=="NEUT"|
             lbtestcd=="PT"|
             lbtestcd=="UREA"|
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
    mutate(lbtestcd = glue("lab_{lbtestcd}", lbtestcd = lbtestcd)) %>%
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
    select(usubjid, "outcome" = dsterm, "date_outcome" = dsstdtc) %>%
    mutate(outcome = str_to_title(outcome))%>%
    mutate(date_outcome=substr(date_outcome,1, 10))%>%
    mutate(date_outcome2=case_when(usubjid=='CVVCORE_247-0004'~'2020-02-26',
                                    usubjid=='CVVCORE_657-0014'~'2020-03-11',
                                    usubjid=='CVVCORE_425-0010'~'2020-03-16',
                                    usubjid=='CVKMNLC_1090-88'~'2020-03-18',
                                    usubjid=='CVVCORE_304-0213'~'2020-03-19',
                                    usubjid=='CVRAPID_PET029-00027'~'2020-03-23',
                                    usubjid=='CVRAPID_PET020-00608'~'2020-03-25',
                                    usubjid=='CVVCORE_062-A011'~'2020-03-26',
                                    usubjid=='CVVCORE_657-0205'~'2020-03-26',
                                    usubjid=='CVRAPID_PET028-00621'~'2020-03-26',
                                    usubjid=='CVVCORE_F135-136'~'2020-03-27',
                                    usubjid=='CVRAPID_PET001-00899'~'2020-03-27',
                                    usubjid=='CVRAPID_PET019-00179'~'2020-03-27',
                                    usubjid=='CVVCORE_538-0050'~'2020-03-28',
                                    usubjid=='CVVCORE_657-0368'~'2020-03-30',
                                    usubjid=='CVVCORE_F215-1'~'2020-03-31',
                                    usubjid=='CVTDWXD_RD070015'~'2020-04-01',
                                    usubjid=='CVPRQTA_399-127'~'2020-04-01',
                                    usubjid=='CVPRQTA_354-104'~'2020-04-02',
                                    usubjid=='CVPRQTA_394-504'~'2020-04-04',
                                    usubjid=='CVRAPID_212-0028'~'2020-04-06',
                                    usubjid=='CVVCORE_657-0193'~'2020-04-07',
                                    usubjid=='CVRAPID_00570-0032'~'2020-04-07',
                                    usubjid=='CVVCORE_657-0190'~'2020-04-09',
                                    usubjid=='CVVCORE_292-0011'~'2020-04-10',
                                    usubjid=='CVVCORE_F173-52'~'2020-04-10',
                                    usubjid=='CVVCORE_657-0192'~'2020-04-11',
                                    usubjid=='CVVCORE_321-0137'~'2020-04-13',
                                    usubjid=='CVVCORE_321-0158'~'2020-04-13',
                                    usubjid=='CVPRQTA_387-173'~'2020-04-14',
                                    usubjid=='CVVCORE_657-0233'~'2020-04-16',
                                    usubjid=='CVPRQTA_394-602'~'2020-04-17',
                                    usubjid=='CVRAPID_213-0029'~'2020-04-17',
                                    usubjid=='CVVCORE_657-0219'~'2020-04-18',
                                    usubjid=='CVVCORE_F279-2'~'2020-04-21',
                                    usubjid=='CVVCORE_496-0079'~'2020-04-22',
                                    usubjid=='CVVCORE_286-0685'~'2020-04-23',
                                    usubjid=='CVVCORE_321-0313'~'2020-04-23',
                                    usubjid=='CVVCORE_321-0308'~'2020-04-23',
                                    usubjid=='CVVCORE_321-0337'~'2020-04-23',
                                    usubjid=='CVVCORE_00561-0075'~'2020-04-23',
                                    usubjid=='CVVCORE_F195-23'~'2020-04-25',
                                    usubjid=='CVPRQTA_398-323'~'2020-04-26',
                                    usubjid=='CVVCORE_F191-211'~'2020-04-26',
                                    usubjid=='CVVCORE_F191-223'~'2020-04-27',
                                    usubjid=='CVVCORE_F125-110'~'2020-04-27',
                                    usubjid=='CVRAPID_433-S058'~'2020-05-01',
                                    usubjid=='CVVCORE_F170-18'~'2020-05-01',
                                    usubjid=='CVVCORE_F191-218'~'2020-05-03',
                                    usubjid=='CVVCORE_286-0531'~'2020-05-04',
                                    usubjid=='CVVCORE_321-0436'~'2020-05-04',
                                    usubjid=='CVTDWXD_RD390010'~'2020-05-04',
                                    usubjid=='CVVCORE_449-0052'~'2020-05-07',
                                    usubjid=='CVVCORE_276-0351'~'2020-05-07',
                                    usubjid=='CVVCORE_A-AF-004-002-0218'~'2020-05-08',
                                    usubjid=='CVVCORE_A-AF-011-001-0066'~'2020-05-09',
                                    usubjid=='CVVCORE_276-0531'~'2020-05-16',
                                    usubjid=='CVPRQTA_400-55'~'2020-05-18',
                                    usubjid=='CVTDWXD_RD100005'~'2020-05-18',
                                    usubjid=='CVVCORE_F255-16'~'2020-05-18',
                                    usubjid=='CVVCORE_449-0070'~'2020-05-19',
                                    usubjid=='CVVECMO_00624-0085'~'2020-05-26',
                                    usubjid=='CVVCORE_118-0012'~'2020-05-28',
                                    usubjid=='CVVECMO_00624-0053'~'2020-05-31',
                                    usubjid=='CVPRQTA_384-210'~'2020-06-01',
                                    usubjid=='CVPRQTA_384-211'~'2020-06-01',
                                    usubjid=='CVVCORE_657-0185'~'2020-06-02',
                                    usubjid=='CVPRQTA_394-777'~'2020-06-03',
                                    usubjid=='CVVECMO_000743-0010'~'2020-06-04',
                                    usubjid=='CVVCORE_321-0428'~'2020-06-06',
                                    usubjid=='CVVCORE_A-AF-024-003-0095'~'2020-06-24',
                                    usubjid=='CVVCORE_A-AF-026-002-0012'~'2020-07-03',
                                    usubjid=='CVRAPID_433-E136'~'2020-07-03',
                                    usubjid=='CVVCORE_276-0711'~'2020-07-03',
                                    usubjid=='CVVCORE_00727-0057'~'2020-09-02',
                                    usubjid=='CVVCORE_A-AF-011-001-0094'~'',
                                    usubjid=='CVPRQTA_400-43'~'',
                                    usubjid=='CVVCORE_403-005'~'',
                                    usubjid=='CVRAPID_00570-0144'~'',
                                    usubjid=='CVRAPID_00570-0178'~'',
                                    usubjid=='CVPRQTA_388-78'~'',
                                    usubjid=='CVPRQTA_400-17'~'',
                                    usubjid=='CVRAPID_213-0010'~'',
                                    usubjid=='CVRAPID_433-S071'~'',
                                    usubjid=='CVVECMO_792913'~'',
                                    usubjid=='CVPRQTA_399-126'~'',
                                    usubjid=='CVPRQTA_353-1083'~'',
                                    usubjid=='CVVCORE_F125-114'~'',
                                    usubjid=='CVPRQTA_399-109'~'',
                                    usubjid=='CVVCORE_657-0365'~'',
                                    usubjid=='CVTDWXD_RD390001'~'',
                                   usubjid=='CVPRQTA_394-236'~'2020-05-05',
                                   usubjid=='CVVCORE_F135-51'~'2020-03-13',
                                   usubjid=='CVTDWXD_RD270049'~'2020-04-17',
                                   usubjid=='CVPRQTA_353-1199'~'2020-04-12',
                                   usubjid=='CVTDWXD_RD390001'~'CVPRQTA_399-130',
                                    TRUE ~ date_outcome))%>%
    mutate(date_outcome2=as_date(date_outcome2))%>%
    mutate(date_outcome=as_date(date_outcome))%>%
    mutate(outcome=case_when(outcome=='CVVCORE_247-0004'~'2020-02-26',
                             outcome=="Currently Hospitalised"~"Ongoing care",
                             outcome=="Death"~"Death",
                             outcome=="Death In Hospital"~"Death",
                             outcome=="Discharge"~"Discharge",
                             outcome=="Discharge With Palliative Care"~"Transferred",
                             outcome=="Discharged"~"Discharge",
                             outcome=="Discharged Alive"~"Discharge",
                             outcome=="Hospital Discharge"~"Discharge",
                             outcome=="Hospitalization"~"Ongoing care",
                             outcome=="Hospitalized"~"Ongoing care",
                             outcome=="Long Term Care Facility"~"Transferred",
                             outcome=="Palliative Discharge"~"Transferred",
                             outcome=="Quarantine Center"~"Transferred",
                             outcome=="Transfer To Other Facility"~"Transferred",
                             outcome=="Transfer To Other Hospital/Facility"~"Transferred",
                             outcome=="Transferred To Another Facility"~"Transferred",
                             outcome=="Transferred To Another Unit"~"Ongoing care",
                             outcome=="Missing In Database"~"Unknown outcome",
                             TRUE ~ NA_character_))

  
  if(dtplyr.step){
    return(outcome)
  } else {
    return(outcome %>% as_tibble())
  }
  
}

#' Fully process data
#' @param demog.file.name Path of the demographics data file (CDISC format)
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
process.all.data <- function(demog.file.name, symptoms.file.name = NA, pregnancy.file.name = NA,
                             ICU.file.name = NA, treatment.file.name = NA, vit_sign.file.name = NA, 
                             outcome.file.name = NA, laboratory.file.name= NA, minimum.treatments = 100, 
                             dtplyr.step = FALSE){
  
  demographic <- import.demographic.data(demog.file.name, dtplyr.step = FALSE)
  
  if(!is.na(symptoms.file.name)){
    comorb.sympt.temp <-  import.symptom.and.comorbidity.data(symptoms.file.name, dtplyr.step = TRUE)
    
    comorbid <- process.comorbidity.data(comorb.sympt.temp, dtplyr.step = FALSE)
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
    symptom <- process.symptom.data(comorb.sympt.temp, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(symptom, by = c("usubjid")) 
  }
  

  if(!is.na(treatment.file.name)){
    treatment <- process.common.treatment.data(treatment.file.name, minimum.treatments, FALSE)
    demographic <- demographic %>%
      left_join(treatment, by = c("usubjid"))
  }
  
  
  if(!is.na(treatment.file.name)){
    date_in_last <- process.treatment.dates.data(treatment.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(date_in_last, by = c("usubjid"))
  }
  
  
  
  if(!is.na(ICU.file.name)){
    icu <- process.ICU.data(ICU.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(icu, by = c("usubjid")) 
  }
  
  if(!is.na(treatment.file.name)){
    icu <- process.ICU.data(ICU.file.name, dtplyr.step = FALSE)
    treatment_all <- process.treatment.data(treatment.file.name, FALSE)
    treatment_icu<-icu%>%
      filter(ever_icu==TRUE)%>%
      filter(!is.na(icu_in))%>%
      filter(!is.na(icu_out))%>%
      left_join(treatment_all, by = c("usubjid"="usubjid"))%>%
      filter(!is.na(indtc))%>%
      mutate(int_icu=case_when(indtc>=icu_in & indtc<=icu_out ~ TRUE, 
                               TRUE ~ FALSE))%>%
      arrange(desc(inoccur))%>%
      distinct(usubjid, treatment, .keep_all =T)%>% 
      arrange(desc(inoccur))%>%
      distinct(usubjid,treatment, .keep_all =T)%>%
      mutate(treatment = glue("icu_treat_{treatment}", treatment = treatment)) %>%
      as.data.table() %>%
      dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = inoccur)
    demographic <- demographic %>%
      left_join(treatment_icu, by = c("usubjid"))
  }
  #if(!is.na(treatment.file.name)){
  #  ventilation <- process.IMV.NIV.data(treatment.file.name, dtplyr.step = FALSE)
  #  demographic <- demographic %>%
  #    left_join(ventilation, by = c("usubjid"))
  #}
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
