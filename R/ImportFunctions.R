
#' Shared pre-processing of input CSV files
#' @param file.name Path of the data file (CDISC format)
#' @param excluded.columns Columns to be removed
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dtplyr dplyr tibble
#' @importFrom data.table fread
#' @return The contents of \code{file.name} as a tibble or \code{dtplyr_step}
#' @keywords internal
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
                            required.columns = c("USUBJID",
                                                 "AGE",
                                                 "SEX",
                                                 "ETHNIC",
                                                 "COUNTRY",
                                                 "RFSTDTC",
                                                 "INVID"),
                            dtplyr.step = TRUE) %>%
    select(usubjid, age, sex, ethnic, country, "date_admit" = rfstdtc, invid) %>%
    mutate(country = replace(country, country == "", NA)) %>%
    left_join(country.lookup, by = c("country" = "Alpha_3")) %>%
    select(-country) %>%
    rename(country = Name) %>%
    rename(site = invid) %>%
    mutate(sex = case_when(sex == "M" ~ "Male",
                           sex == "F" ~ "Female",
                           TRUE ~ NA_character_)) %>%
    mutate(ethnic = replace(ethnic, ethnic == "", NA))%>%
    mutate(date_admit=substr(date_admit,1, 10))%>%
    mutate(date_admit=as_date(date_admit))
  
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
    mutate(sastdtc=as_date(sastdtc))%>%
    arrange(sastdtc)%>%
    distinct(usubjid, .keep_all =T)%>%
    select(usubjid, "date_onset"=sastdtc)%>%
    as.data.frame()
  
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
    select(usubjid, hodecod, hostdtc, hoendtc, hooccur) %>%
    mutate(hodecod = ifelse(hodecod=="HOSPITAL", "hospital", "icu")) %>%
    filter(hodecod=="icu")%>%
    arrange(desc(hostdtc))%>%
    distinct(usubjid, .keep_all =T)%>%
    mutate(hostdtc=substr(hostdtc,1, 10))%>%
    mutate(hostdtc=as_date(hostdtc))%>%
    mutate(hoendtc=substr(hoendtc,1, 10))%>%
    mutate(hoendtc=as_date(hoendtc))%>%
    rename(ever_icu=hooccur)%>%
    rename(icu_in=hostdtc)%>%
    rename(icu_out=hoendtc)%>%
    select(-c(hodecod))
    #as.data.table() %>%
    #dt_pivot_wider(id_cols = usubjid, names_from = hodecod,  values_from = c("in", "out", ever))#%>%
    #lazy_dt(immutable = FALSE)#%>%
    #select(usubjid, ever_hospital, "hospital_in" = hostdtc_hospital,"hospital_out" = hoendtc_hospital,
     #     ever_icu, "icu_in" = hostdtc_icu,  "icu_out" = hoendtc_icu) #%>%
    #select(-starts_with("hospital_")) %>%
    #filter(is.na(icu_in)==FALSE) 
  
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
  treatment<-out%>%
    filter(incat == "SUPPORTIVE CARE" | incat == "ANTIBIOTIC AGENTS" | incat == "ANTIFUNGAL AGENTS"
           | incat == "ANTIVIRAL AGENTS" | incat == "CORTICOSTEROIDS") %>%
    filter(inpresp =="Y") %>%
    select(usubjid, "treatment" = intrt, inoccur, indtc, incat) %>%
    mutate(treatment=replace(treatment,incat=="ANTIBIOTIC AGENTS", "ANTIBIOTIC AGENTS"))%>%
    mutate(treatment=replace(treatment,incat=="ANTIFUNGAL AGENTS", "ANTIFUNGAL AGENTS"))%>%
    mutate(treatment=replace(treatment,incat=="ANTIVIRAL AGENTS", "ANTIVIRAL AGENTS"))%>%
    mutate(treatment=replace(treatment,incat=="CORTICOSTEROIDS", "CORTICOSTEROIDS"))%>%
    select(usubjid, treatment, inoccur, indtc) %>%
    mutate(treatment = iconv(treatment, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(treatment = str_remove_all(treatment, "\\s*\\([^)]*\\)")) %>%
    mutate(treatment = str_replace_all(treatment, " - ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "-", "_")) %>%
    mutate(treatment = str_replace_all(treatment, "/| / ", "_")) %>%
    mutate(treatment = str_replace_all(treatment, " ", "_")) %>%
    mutate(inoccur = case_when(inoccur == "Y" ~ TRUE,
                               inoccur == "N" ~ FALSE,
                               TRUE ~ NA))# %>%
    #distinct(usubjid, treatment, .keep_all =T) 
  
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
    mutate(date_outcome=as_date(date_outcome))
  
  if(dtplyr.step){
    return(outcome)
  } else {
    return(outcome %>% as_tibble())
  }
  
}

#' Fully process data
#' @param demog.file.name Path of the demographics data file (CDISC format)
#' @param symptoms.file.name Path of the symptoms data file (CDISC format, optional)
#' @param ICU.file.name Path of the healthcare encounters data file (CDISC format, optional)
#' @param treatment.file.name Path of the intervention data file (CDISC format, optional)
#' @param outcome.file.name Path of the dispositions data file (CDISC format, optional)
#' @param minimum.treatments The minimum number of instances of a treatment required for inclusion as a column
#' @param dtplyr.step Return the output as \code{dtplyr_step} to avoid unnecessary future calls to \code{as_tibble} or \code{as.data.table}
#' @import dplyr tibble
#' @return Formatted outcome data as a tibble or \code{dtplyr_step}
#' @export process.all.data
process.all.data <- function(demog.file.name, symptoms.file.name = NA, pregnancy.file.name = NA,
                             ICU.file.name = NA, treatment.file.name = NA, outcome.file.name = NA, 
                             minimum.treatments = 100, dtplyr.step = FALSE){
  
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
  

  
  if(!is.na(ICU.file.name)){
    icu <- process.ICU.data(ICU.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(icu, by = c("usubjid")) 
  }
  if(!is.na(treatment.file.name)){
    treatment <- process.common.treatment.data(treatment.file.name, minimum.treatments, FALSE)
    demographic <- demographic %>%
      left_join(treatment, by = c("usubjid"))
  }
  if(!is.na(treatment.file.name)){
    ventilation <- ventilation <- process.IMV.NIV.data(treatment.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(ventilation, by = c("usubjid"))
  }
  if(!is.na(outcome.file.name)){
    outcome <- process.outcome.data(outcome.file.name, dtplyr.step = FALSE)
    demographic <- demographic %>%
      left_join(outcome, by = c("usubjid"))
  }
  
  
  if(dtplyr.step){
    return(demographic)
  } else {
    return(demographic %>% as_tibble())
  }
}
