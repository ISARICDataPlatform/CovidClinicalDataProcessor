#libraries

library("ff")
library(stringr)
library(plyr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(Hmisc)
library(dtplyr) 
library(data.table)
library(tidyfast)
library(glue)
library(lubridate)
library(readr)
library(janitor)
library(tools)
library(stringy)







folder <- "C:/Users/baruj003/Desktop/21/working_R/oxford/CovidClinicalDataProcessor/data"

setwd(folder)
memory.limit(size=160000)

folder <- "C:/Users/marti/OneDrive/Documents/ISARIC/data/2021-04-07"
setwd(folder)

#'Importing csv files
dm<-read.csv("DM.csv")
colnames(dm) <- tolower(colnames(dm))
rp<-read.csv("RP.csv")
colnames(rp) <- tolower(colnames(rp))
ho<-read.csv("HO.csv")
colnames(ho) <- tolower(colnames(ho))
mb<-read.csv("MB.csv")
colnames(mb) <- tolower(colnames(mb))
vs<-read.csv("VS.csv")
colnames(vs) <- tolower(colnames(vs))
save(vs,file="vs.rda")
lb<-read.csv("LB.csv")
colnames(lb) <- tolower(colnames(lb))
save(lb,file="lb.rda")
ds<-read.csv("DS.csv")
colnames(ds) <- tolower(colnames(ds))

sa7<-
  read_csv(
   "sa_1_saterm_no_quotes.csv",
    col_names = TRUE,
    col_types = NULL,
    locale = default_locale(),
    na = c("", "NA"),
    quoted_na = TRUE,
    #quote = quote,
    comment = "",
    trim_ws = TRUE,
    skip = 0,
    n_max = Inf,
    guess_max = min(8000000, 12000000000),
    progress = show_progress(),
    skip_empty_rows = TRUE
  )

colnames(sa) <- tolower(colnames(sa))
save(sa,file="sa.rda")
load("sa.rda")




int4 <- read.csv.ffdf(file="in.csv", header=TRUE, VERBOSE=TRUE, quote = '"',
                      first.rows=1000, next.rows=5000000, 
                      colClasses = c(rep("factor", 66), rep("NULL", 66)), na = '')
int4<-as.data.frame(int4)
colnames(int4) <- tolower(colnames(int4))
save(int,file="int.rda")




save(int4,file="int.rda")


int<-int_2
INT<-
  read_csv(
    "IN.csv",
    col_names = TRUE,
    col_types = NULL,
    locale = default_locale(),
    na = c("", "NA"),
    quoted_na = TRUE,
    quote = "\"",
    comment = "",
    trim_ws = TRUE,
    skip = 0,
    n_max = Inf,
    guess_max = min(8000000, 25000000),
    progress = show_progress(),
    skip_empty_rows = TRUE
  )
colnames(int) <- tolower(colnames(int))
save(int,file="int.rda")

###set date pull
date_pull<-as_date("2021-04-07") 

###calling data import functions

imp_dm <- import.demographic.data(dm, dtplyr.step = FALSE)
save(imp_dm, file = "imp_dm.rda")


imp_mb <- import.microb.data(mb, dtplyr.step = FALSE)
save(imp_mb, file = "imp_mb.rda")

imp_rp <- process.pregnancy.data(rp, dtplyr.step = FALSE)
save(imp_rp, file = "imp_rp.rda")

load("int.rda")
int4<-int
save(int, file = "int.rda")
in_FIO2<-int4%>%filter(intrt%like%"FIO2")
#incl_studyid<-unique(as.character(int$studyid))  

  

int<-int4%>%
  filter(studyid=="CVZXZMV")
imp_int_CVZXZMV<-process.treatment.data(int, dtplyr.step = FALSE)

int<-int4%>%
  filter(studyid=="CVCCPUK")
imp_int_CVCCPUK<-process.treatment.data(int, dtplyr.step = FALSE)


int<-int4%>%
  filter(studyid!="CVCCPUK")%>%
  filter(studyid!="CVZXZMV")
imp_int_rest<-process.treatment.data(int, dtplyr.step = FALSE)


imp_int<-imp_int_rest%>%
    rbind(imp_int_CVCCPUK)%>%
    rbind(imp_int_CVZXZMV)

tab_CVZXZMV_int<-int%>%tabyl(inevintx)
tab_CVZXZMV<-imp_int_CVZXZMV%>%tabyl(treatment)
tab<-imp_int%>%tabyl(treatment)


save(imp_int, file = "imp_int.rda")

load(file="imp_int.rda")

tab<-int%>%
  filter(inevintx=="00:00-24:00 ON DAY OF ASSESSMENT")
  mutate(indy=as.numeric(indy))%>%
  mutate(inevintx=as.character(inevintx))%>%
  mutate(inevintx=case_when(inevintx=="WHILE HOSPITALIZED OR AT DISCHARGE"~"WHILE HOSPITALISED OR AT DISCHARGE",
                            inevintx%like%"WITHIN 14 DAYS OF"~"WITHIN 14 DAYS OF ADMISSION",
                            TRUE~inevintx))%>%
         filter(inpresp =="Y")%>%filter(inevintx!="BEFORE HOSPITAL ADMISSION")%>%tabyl(indy,inevintx)
tab<-int%>%filter(inpresp =="Y")%>%filter(inevintx!="BEFORE HOSPITAL ADMISSION")%>%tabyl(indy,inevintx)
INEVINTX


imp_treat<-process.common.treatment.data(imp_int, minimum=10, dtplyr.step = FALSE)
var<-as.data.frame(colnames(imp_treat))
save(imp_treat, file = "imp_treat.rda")

imp_icu<- process.ICU.data(ho, dtplyr.step = FALSE)
save(imp_icu, file = "imp_icu.rda")

imp_treat_icu<-process.treatment.icu.data(imp_int, imp_icu,imp_dm, minimum=100,dtplyr.step = FALSE)
save(imp_treat_icu, file = "imp_treat_icu2.rda")

imp_vs<- process.vital.sign.data(vs, dtplyr.step = FALSE)
save(imp_vs, file = "imp_vs.rda")

imp_lb <- process.laboratory.data(lb, dtplyr.step = FALSE)
save(imp_lb, file = "imp_lb.rda")

imp_ds <-process.outcome.data(ds, dtplyr.step = FALSE)
save(imp_ds, file = "imp_ds.rda")
tab<-tabyl(imp_ds$usubjid)



load("sa.rda")
sab<-sa

sa<-sab%>%
  filter(studyid=="CVZXZMV")
imp_sa_CVZXZMV<-import.symptom.and.comorbidity.data(sa, dtplyr.step = FALSE)


sa<-sab%>%
  filter(studyid=="CVCCPUK")
imp_sa_CVCCPUK<-import.symptom.and.comorbidity.data(sa, dtplyr.step = FALSE)


sa<-sab%>%
  filter(studyid!="CVCCPUK"&studyid!="CVZXZMV")
imp_sa_rest<-import.symptom.and.comorbidity.data(sa, dtplyr.step = FALSE)

imp_sa<-imp_sa_rest%>%
  rbind(imp_sa_CVCCPUK)%>%
  rbind(imp_sa_CVZXZMV)

imp_sa_com<-imp_sa
save(imp_sa_com, file = "imp_sa_com.rda")



imp_sa<-import.symptom.and.comorbidity.data(sa, dtplyr.step = FALSE)
tabyl(imp_sa$saterm)
save(imp_sa, file = "imp_sa.rda")

load(file="imp_sa_com.rda")
imp_comorb<-process.comorbidity.data(imp_sa_com, minimum=100, dtplyr.step = FALSE)
save(imp_comorb, file = "imp_comorb.rda")

imp_symptom<-process.symptom.data(imp_sa_sa, minimum=100, dtplyr.step = FALSE)
save(imp_symptom, file = "imp_symptom.rda")



load(file="imp_dm.rda")
load(file="imp_mb.rda")
load(file="imp_comorb.rda")
load(file="imp_rp.rda")
load(file="imp_symptom.rda")
load(file="imp_treat.rda")
load(file="imp_icu.rda")
load(file="imp_treat_icu.rda")
load(file="imp_lb.rda")
load(file="imp_vs.rda")
load(file="imp_ds.rda")

#########joining all data
import.tbl<-imp_dm%>%
  left_join(imp_mb, by = c("usubjid"))%>%
  left_join(imp_comorb, by = c("usubjid"))%>%
  left_join(imp_rp, by = c("usubjid"))%>%
  left_join(imp_symptom, by = c("usubjid"))%>%
  left_join(imp_treat, by = c("usubjid"))%>%
  left_join(imp_icu, by = c("usubjid"))%>%
  left_join(imp_treat_icu, by = c("usubjid"))%>%
  left_join(imp_lb, by = c("usubjid"))%>%
  left_join(imp_vs, by = c("usubjid"))%>%
  left_join(imp_ds, by = c("usubjid"))%>%
  group_by(usubjid) %>% 
  mutate(count=1)%>% 
  mutate(n = sum(count)) %>%
  filter(n == 1) %>%
  ungroup()


save(import.tbl, file = "import.tbl.rda")
load("import.tbl.rda")

#########calling preprocessing function

prepr.tbl<-data.preprocessing(import.tbl)

list_2<-as.data.frame(colnames(prepr.tbl))
save(prepr.tbl, file = "prepr.tbl.all.rda")

rmv<-exclud.sympt.comorb.tret(import.tbl)
prepr.tbl<-prepr.tbl%>%select(-c(all_of(rmv)))
save(prepr.tbl, file = "prepr.tbl.rda")
#########if needed launching randomization function on the imported data and then preprocess the data

random.import.tbl<-randomization(import.tbl)
save(random.import.tbl, file = "random.import.tbl.rda")
load("random.import.tbl.rda")

import.tbl<-random.import.tbl
random.prepr.tbl<-data.preprocessing(import.tbl)
save(random.prepr.tbl, file = "random.prepr.tbl.rda")
load("prepr.tbl.rda")
