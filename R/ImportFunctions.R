
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
    select(siteid,invid, usubjid, rfstdtc, age, ageu, sex, ethnic, country,subjid)%>%
    mutate(country = replace(country, country == "", NA)) %>%
    left_join(country.lookup, by = c("country" = "Alpha_3")) %>%
    select(-country) %>%
    rename(country = Name) %>%
    rename(date_admit=rfstdtc)%>%
    rename(site = invid) %>%
    as.data.frame()%>%
    mutate(age_d=NA)%>%
    mutate(age_d=replace(age_d,ageu=="MONTHS",12))%>%
    mutate(age_d=replace(age_d,ageu=="YEARS",1))%>%
    mutate(age2=age/age_d)%>%
    select(-(age))%>%
    rename(age=age2)%>%
    mutate(ethnic = iconv(ethnic, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    mutate(ethnic = str_remove_all(ethnic, "\\s*\\([^)]*\\)")) %>%
    mutate(ethnic = str_replace_all(ethnic, " - ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, "-", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, "/| / ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, " ", "_")) %>%
    mutate(ethnic = str_replace_all(ethnic, ",", "_")) %>%
    mutate(ethnic = replace(ethnic, ethnic == "n_a" | ethnic == "na" | ethnic == "", NA))%>%
    mutate(studyid=substr(usubjid,1, 7))%>%
    separate(subjid, c("siteid_finala","patient"), sep = "-")%>%
    select(studyid, siteid,siteid_finala,patient, site, usubjid, date_admit, age,sex, ethnic, country)%>%
    mutate(siteid_finala=as.character(siteid_finala))%>%
    mutate(siteid_final= case_when(is.na(patient) ~ site,
                                   patient=="" ~ site,
                                  #dataset=="CVTDWXD" ~ siteid,
                                  #dataset=="CVPSICL"~"QECH",
                                  site=="00741cca_network"~"00741cca_network",
                                   TRUE ~ siteid_finala
                                      )) %>%
    mutate(siteid_final=replace(siteid_final,studyid=="CVPSICL","QECH"))%>%
    mutate(siteid_final=replace(siteid_final,studyid=="CVTDWXD","CVTDWXD"))%>%
    mutate(siteid_final=paste0("text_",siteid_final))%>%
    mutate(sex = case_when(sex == "M" ~ "Male",
                           sex == "F" ~ "Female",
                           TRUE ~ NA_character_)) %>%
        mutate(date_admit=substr(date_admit,1, 10))%>%
    #mutate(date_admit=as_date(date_admit))%>%
   mutate(date_admit2=case_when(usubjid=='CVVCORE_247-0004' ~ '2020-02-25',
                                 usubjid=='CVVECMO_007-0001'~'2020-02-29',
                                 usubjid=='CVPPNSH_53-4'~'2020-03-07',
                                 usubjid=='CVVCORE_304-0027'~'2020-03-07',
                                 usubjid=='CVVCORE_657-0007'~'2020-03-08',
                                 usubjid=='CVVCORE_657-0014'~'2020-03-10',
                                 usubjid=='CVVCORE_481-0155'~'2020-03-11',
                                 usubjid=='CVVCORE_425-0010'~'2020-03-13',
                                 usubjid=='CVVCORE_531-1004'~'2020-03-16',
                                 usubjid=='CVKMNLC_1090-88'~'2020-03-16',
                                 usubjid=='CVVCORE_449-0208'~'2020-03-17',
                                 usubjid=='CVVCORE_304-0213'~'2020-03-18',
                                 usubjid=='CVPRQTA_354-386'~'2020-03-18',
                                 usubjid=='CVTDWXD_RD300006'~'2020-03-19',
                                 usubjid=='CVVCORE_520-0011'~'2020-03-20',
                                 usubjid=='CVVCORE_497-0022'~'2020-03-20',
                                 usubjid=='CVVCORE_657-0229'~'2020-03-20',
                                 usubjid=='CVVCORE_594-0077'~'2020-03-21',
                                 usubjid=='CVVCORE_449-5016'~'2020-03-23',
                                 usubjid=='CVVCORE_445-7000'~'2020-03-23',
                                 usubjid=='CVVCORE_062-A014'~'2020-03-23',
                                 usubjid=='CVRAPID_PET020-00608'~'2020-03-23',
                                 usubjid=='CVRAPID_216-0046'~'2020-03-23',
                                 usubjid=='CVVCORE_522-0002'~'2020-03-24',
                                 usubjid=='CVVCORE_F135-136'~'2020-03-24',
                                 usubjid=='CVVCORE_030-0024'~'2020-03-24',
                                 usubjid=='CVVCORE_538-0030'~'2020-03-24',
                                 usubjid=='CVVCORE_489-0001'~'2020-03-25',
                                 usubjid=='CVVCORE_512-0012'~'2020-03-25',
                                 usubjid=='CVVCORE_449-0243'~'2020-03-25',
                                 usubjid=='CVRAPID_00711-0061'~'2020-03-25',
                                 usubjid=='CVVCORE_062-A011'~'2020-03-25',
                                 usubjid=='CVVCORE_497-0057'~'2020-03-25',
                                 usubjid=='CVVCORE_531-1009'~'2020-03-25',
                                 usubjid=='CVVCORE_304-0088'~'2020-03-25',
                                 usubjid=='CVVCORE_449-0118'~'2020-03-25',
                                 usubjid=='CVVCORE_657-0205'~'2020-03-25',
                                 usubjid=='CVVCORE_449-0152'~'2020-03-25',
                                 usubjid=='CVVECMO_00670-0041'~'2020-03-26',
                                 usubjid=='CVTDWXD_RD260032'~'2020-03-26',
                                 usubjid=='CVTDWXD_RD430009'~'2020-03-26',
                                 usubjid=='CVVCORE_F191-216'~'2020-03-26',
                                 usubjid=='CVVCORE_465-0005'~'2020-03-26',
                                 usubjid=='CVVCORE_F191-223'~'2020-03-26',
                                 usubjid=='CVVCORE_657-0368'~'2020-03-26',
                                 usubjid=='CVVECMO_00546-0017'~'2020-03-27',
                                 usubjid=='CVVCORE_062-A018'~'2020-03-27',
                                 usubjid=='CVVCORE_215-0012'~'2020-03-27',
                                 usubjid=='CVVCORE_449-0148'~'2020-03-27',
                                 usubjid=='CVVCORE_F135-145'~'2020-03-27',
                                 usubjid=='CVVCORE_520-0014'~'2020-03-27',
                                 usubjid=='CVVCORE_465-0008'~'2020-03-28',
                                 usubjid=='CVVCORE_445-7010'~'2020-03-28',
                                 usubjid=='CVVCORE_F191-230'~'2020-03-28',
                                 usubjid=='CVVCORE_449-0089'~'2020-03-28',
                                 usubjid=='CVVCORE_F141-20'~'2020-03-28',
                                 usubjid=='CVVCORE_450-0015'~'2020-03-29',
                                 usubjid=='CVRAPID_PET031-01378'~'2020-03-29',
                                 usubjid=='CVVCORE_445-7028'~'2020-03-29',
                                 usubjid=='CVVCORE_292-0011'~'2020-03-29',
                                 usubjid=='CVVCORE_560-0001'~'2020-03-29',
                                 usubjid=='CVVCORE_F191-222'~'2020-03-29',
                                 usubjid=='CVVCORE_497-0076'~'2020-03-29',
                                 usubjid=='CVVCORE_657-0353'~'2020-03-29',
                                 usubjid=='CVVCORE_450-0012'~'2020-03-30',
                                 usubjid=='CVVCORE_449-0144'~'2020-03-30',
                                 usubjid=='CVVCORE_F191-224'~'2020-03-30',
                                 usubjid=='CVVCORE_449-0106'~'2020-03-30',
                                 usubjid=='CVVCORE_F191-245'~'2020-03-30',
                                 usubjid=='CVVCORE_497-0089'~'2020-03-30',
                                 usubjid=='CVTDWXD_RD070057'~'2020-04-01',
                                 usubjid=='CVVCORE_531-1014'~'2020-04-01',
                                 usubjid=='CVTDWXD_RD270088'~'2020-04-05',
                                 usubjid=='CVRAPID_00711-0290'~'2020-04-06',
                                 usubjid=='CVTDWXD_IC01031'~'2020-04-06',
                                 usubjid=='CVVCORE_657-0193'~'2020-04-06',
                                 usubjid=='CVVECMO_360-0007'~'2020-04-07',
                                 usubjid=='CVVCORE_00553-0004'~'2020-04-07',
                                 usubjid=='CVVCORE_092-1012'~'2020-04-07',
                                 usubjid=='CVVCORE_657-0190'~'2020-04-08',
                                 usubjid=='CVVCORE_00573-0015'~'2020-04-09',
                                 usubjid=='CVVCORE_657-0192'~'2020-04-10',
                                 usubjid=='CVVCORE_F199-28'~'2020-04-12',
                                 usubjid=='CVRAPID_214-0011'~'2020-04-14',
                                 usubjid=='CVVCORE_F191-327'~'2020-04-14',
                                 usubjid=='CVVCORE_F279-2'~'2020-04-14',
                                 usubjid=='CVVCORE_657-0233'~'2020-04-15',
                                 usubjid=='CVVCORE_657-0219'~'2020-04-15',
                                 usubjid=='CVVECMO_694-0044'~'2020-04-16',
                                 usubjid=='CVVCORE_445-7070'~'2020-04-17',
                                 usubjid=='CVVCORE_062-A037'~'2020-04-18',
                                 usubjid=='CVVCORE_215-0163'~'2020-04-18',
                                 usubjid=='CVRAPID_211-0038'~'2020-04-18',
                                 usubjid=='CVVCORE_496-0079'~'2020-04-21',
                                 usubjid=='CVVCORE_F195-23'~'2020-04-23',
                                 usubjid=='CVVCORE_292-0024'~'2020-04-24',
                                 usubjid=='CVVECMO_000743-0004'~'2020-04-24',
                                 usubjid=='CVRAPID_00711-0328'~'2020-04-26',
                                 usubjid=='CVTDWXD_RD420006'~'2020-04-27',
                                 usubjid=='CVRAPID_433-P019'~'2020-04-27',
                                 usubjid=='CVRAPID_433-E082'~'2020-04-27',
                                 usubjid=='CVVCORE_449-0070'~'2020-04-27',
                                 usubjid=='CVVCORE_276-0314'~'2020-04-28',
                                 usubjid=='CVRAPID_433-S058'~'2020-04-28',
                                 usubjid=='CVRAPID_433-E070'~'2020-04-28',
                                 usubjid=='CVVCORE_321-0429'~'2020-04-30',
                                 usubjid=='CVVCORE_A-AF-004-002-0216'~'2020-05-01',
                                 usubjid=='CVVCORE_A-AF-011-004-0166'~'2020-05-03',
                                 usubjid=='CVVCORE_A-AF-004-002-0218'~'2020-05-06',
                                 usubjid=='CVVCORE_449-0052'~'2020-05-06',
                                 usubjid=='CVVCORE_A-AF-011-001-0066'~'2020-05-07',
                                 usubjid=='CVVCORE_A-AF-011-004-0165'~'2020-05-07',
                                 usubjid=='CVRAPID_433-E101'~'2020-05-08',
                                 usubjid=='CVRAPID_00711-0725'~'2020-05-10',
                                 usubjid=='CVPRQTA_400-55'~'2020-05-12',
                                 usubjid=='CVVECMO_00565-0030'~'2020-05-15',
                                 usubjid=='CVRAPID_00652-AM006'~'2020-05-18',
                                 usubjid=='CVRAPID_00570-0028'~'2020-05-19',
                                 usubjid=='CVVCORE_276-0610'~'2020-05-21',
                                 usubjid=='CVVCORE_276-0585'~'2020-05-22',
                                 usubjid=='CVRAPID_00570-0019'~'2020-05-26',
                                 usubjid=='CVVCORE_276-0572'~'2020-05-28',
                                 usubjid=='CVPRQTA_384-210'~'2020-05-28',
                                 usubjid=='CVRAPID_433-E068'~'2020-05-29',
                                 usubjid=='CVVCORE_A-AF-002-003-0064'~'2020-05-30',
                                 usubjid=='CVPRQTA_384-211'~'2020-05-31',
                                 usubjid=='CVVECMO_000743-0011'~'2020-05-31',
                                 usubjid=='CVPRQTA_394-777'~'2020-06-02',
                                 usubjid=='CVVCORE_A-AF-026-002-0023'~'2020-06-09',
                                 usubjid=='CVVCORE_007-0152'~'2020-06-13',
                                 usubjid=='CVVECMO_00703-0016'~'2020-06-19',
                                 usubjid=='CVVCORE_A-AF-024-003-0095'~'2020-06-23',
                                 usubjid=='CVVCORE_276-0891'~'2020-06-30',
                                 usubjid=='CVRAPID_00570-0408'~'2020-07-01',
                                 usubjid=='CVRAPID_00570-0334'~'2020-07-20',
                                 usubjid=='CVVECMO_00667-0005'~'2020-07-27',
                                 usubjid=='CVVCORE_276-0906'~'2020-07-31',
                                 usubjid=='CVVCORE_153-0002'~'2020-08-15',
                                 usubjid=='CVVCORE_00673-0020'~'2020-08-19',
                                 usubjid=='CVVCORE_203-0052'~'',
                                 usubjid=='CVVECMO_203-0052'~'',
                                 usubjid=='CVVCORE_A-AF-007-002-0060'~'',
                                 usubjid=='CVPRQTA_388-78'~'',
                                 usubjid=='CVPRQTA_400-17'~'',
                                 usubjid=='CVVECMO_694-0083'~'',
                                 usubjid=='CVVCORE_F218-10'~'',
                                 usubjid=='CVPRQTA_392-37'~'',
                                 usubjid=='CVPRQTA_394-641'~'',
                                 usubjid=='CVVCORE_520-0048'~'',
                                 usubjid=='CVVCORE_F135-258'~'',
                                 usubjid=='CVVECMO_00634-0007'~'',
                                 usubjid=='CVVCORE_A-AF-004-003-0020'~'',
                                TRUE ~ date_admit))%>%
                  mutate(date_admit2=as_date(date_admit2))%>%
    select(studyid, siteid_final, usubjid, date_admit, date_admit2, age, sex, ethnic, country  )
  
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
                                 TRUE ~ date_onset))%>%
      mutate(date_onset2=as_date(date_onset2))
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
    filter(!is.na(vsstresn))%>%
    mutate(vstestcd = glue("vs_{vstestcd}", vstestcd = vstestcd)) %>%
    mutate(vstestcd = iconv(vstestcd, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    arrange(desc(vsdtc))%>%
    distinct(usubjid,vstestcd, .keep_all =T)%>%
    as.data.table() %>%
    dt_pivot_wider(id_cols = usubjid, names_from = vstestcd,  values_from = vsstresn) 
   
  
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
    mutate(lborres=as.numeric(lborres))%>%
    filter(!is.na(lborres))%>%
    mutate(lbtestcd = glue("lab_{lbtestcd}", lbtestcd = lbtestcd)) %>%
    mutate(lbtestcd = iconv(lbtestcd, to ="ASCII//TRANSLIT") %>% tolower()) %>%
    arrange(desc(lbdtc))%>%
    distinct(usubjid,lbtestcd, .keep_all =T)%>%
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
    mutate(date_outcome=as_date(date_outcome))
  
  
  mutates(date_outcome=case_when(usubjid=="CVVCORE_286-0685" ~ 2020-04-23,
                                 usubjid=="CVPRQTA_398-323" ~2020-04-26,
                                 usubjid=="CVRAPID_00570-0032" ~2020-04-07
                                 
                                 
                                 
                                 
                                 
                                 
  ))
  
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
      left_join(outcome, by = c("usubjid"))%>%
      mutate(t_on_ad=date_admit-date_onset)%>%
      mutate(t_ad_icu=icu_in-date_admit)%>%
      mutate(t_ad_imv=imv_st-date_admit)%>%
      mutate(t_ad_niv=niv_st-date_admit)%>%
      mutate(icu_dur=icu_out-icu_in)%>%
      mutate(ho_dur=date_outcome-date_admit)%>%
      mutate(imv_dur=imv_en-imv_st)%>%
      mutate(niv_dur=niv_en-niv_st)#%>%
     # select(-c("comorbid_covid-19_symptoms","comorbid_drinks_beer", 
      #          "symptoms_covid-19_symptoms", "symptoms_hematuria",
       #         "symptoms_hemoglobinuria",
        #        "symptoms_leukocyturia",
         #      "symptoms_proteinuria"))
  }
  
  
  
  if(dtplyr.step){
    return(demographic)
  } else {
    return(demographic %>% as_tibble())
  }
}
