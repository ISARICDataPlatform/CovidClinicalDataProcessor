#libraries
#install.packages('WDI')
library(WDI)
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
memory.limit(size=120000)

folder <- "C:/Users/marti/OneDrive/Documents/ISARIC/data/2021-05-24/2021-05-24"


setwd(folder)


#'Importing csv files
dm<-read.csv("DM.csv")
colnames(dm) <- tolower(colnames(dm))
rp<-read.csv("RP.csv")
colnames(rp) <- tolower(colnames(rp))
ho<-read.csv("HO.csv")
colnames(ho) <- tolower(colnames(ho))
mb<-read.csv("Internal_MB_2021-05-24_v2.csv")
colnames(mb) <- tolower(colnames(mb))
vs<-read.csv("Internal_VS_2021-05-24_v2.csv")
colnames(vs) <- tolower(colnames(vs))
save(vs,file="vs.rda")
lb<-read.csv("LB.csv")
colnames(lb) <- tolower(colnames(lb))
save(lb,file="lb.rda")
ds<-read.csv("DS.csv")
colnames(ds) <- tolower(colnames(ds))

sa<-
  read_csv(
   "SA.csv",
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

sa <- read.csv.ffdf(file="SA.csv", header=TRUE, VERBOSE=TRUE, quote = ,
                     first.rows=1000, next.rows=50000, 
                     colClasses = c(rep("factor", 66), rep("NULL", 66)), na = '')
sa<-as.data.frame(sa)
colnames(sa) <- tolower(colnames(sa))
save(sa,file="sa.rda")



int <- read.csv.ffdf(file="IN.csv", header=TRUE, VERBOSE=TRUE, quote = '"',
                      first.rows=1000, next.rows=50000, 
                      colClasses = c(rep("factor", 66), rep("NULL", 66)), na = '')
int<-as.data.frame(int)

colnames(int) <- tolower(colnames(int))
save(int,file="int.rda")


int<-
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
date_pull<-as_date("2021-05-24") 

###calling data import functions

imp_dm <- import.demographic.data(dm, dtplyr.step = FALSE)
save(imp_dm, file = "imp_dm.rda")
country<-imp_dm%>%tabyl(country)
write.table(country, "country.csv", sep=",", row.names=F, na="")


imp_mb <- import.microb.data(mb, dtplyr.step = FALSE)
save(imp_mb, file = "imp_mb.rda")

imp_rp <- process.pregnancy.data(rp, dtplyr.step = FALSE)
save(imp_rp, file = "imp_rp.rda")

load("sa.rda")

imp_sa<-import.symptom.and.comorbidity.data(sa, dtplyr.step = FALSE)
save(imp_sa, file = "imp_sa.rda")
load("imp_sa.rda")

imp_comorb<-process.comorbidity.data(imp_sa, minimum=100, dtplyr.step = FALSE)
save(imp_comorb, file = "imp_comorb.rda")

imp_symptom<-process.symptom.data(imp_sa, minimum=100, dtplyr.step = FALSE)
save(imp_symptom, file = "imp_symptom.rda")


load("int.rda")
imp_int<-process.treatment.data(int, dtplyr.step = FALSE)
save(imp_int, file = "imp_int.rda")

imp_treat<-process.common.treatment.data(imp_int, minimum=10, dtplyr.step = FALSE)
save(imp_treat, file = "imp_treat.rda")

imp_icu<- process.ICU.data(ho, dtplyr.step = FALSE)
save(imp_icu, file = "imp_icu.rda")

imp_vs<- process.vital.sign.data(vs, dtplyr.step = FALSE)
save(imp_vs, file = "imp_vs.rda")

imp_lb <- process.laboratory.data(lb, dtplyr.step = FALSE)
save(imp_lb, file = "imp_lb.rda")

imp_ds <-process.outcome.data(ds, dtplyr.step = FALSE)
save(imp_ds, file = "imp_ds.rda")
tab<-tabyl(imp_ds$outcome)

imp_treat_icu<-process.treatment.icu.data(imp_int, imp_icu,imp_dm,imp_ds, minimum=10,dtplyr.step = FALSE)
save(imp_treat_icu, file = "imp_treat_icu.rda")




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
  left_join(imp_ds, by = c("usubjid"))


save(import.tbl, file = "import.tbl.rda")


#########calling preprocessing function

load("import.tbl.rda")
prepr.tbl<-data.preprocessing(import.tbl)

list_2<-as.data.frame(colnames(prepr.tbl))
save(prepr.tbl, file = "prepr.tbl.all.rda")

rmv<-exclud.sympt.comorb.tret(import.tbl)
prepr.tbl<-prepr.tbl%>%select(-c(all_of(rmv)))
list_2<-as.data.frame(colnames(prepr.tbl))



#income<-imp_dm%>%select(usubjid,income)
#prepr.tbl<-prepr.tbl%>%left_join(income)
save(prepr.tbl, file = "prepr.tbl.rda")


load("prepr.tbl.rda")
test<-prepr.tbl%>%select(-c(cov_det_cronavir,
                            cov_det_sarscov2,
                            cov_id_cronavir,
                            cov_id_sarscov2,
                            cov_det_id,
                            clin_diag_covid_19))%>%
                left_join(imp_mb)%>%
                left_join(covid_clinic_diagn)

prepr.tbl<-test

exclusion<-import.tbl%>%tabyl(cov_det_id,clin_diag_covid_19)
write.table(exclusion, "exclusion.csv", sep=",", row.names=F, na="")
#########if needed launching randomization function on the imported data and then preprocess the data

random.import.tbl<-randomization(import.tbl)
save(random.import.tbl, file = "random.import.tbl.rda")
load("random.import.tbl.rda")

import.tbl<-random.import.tbl
random.prepr.tbl<-data.preprocessing(import.tbl)
save(random.prepr.tbl, file = "random.prepr.tbl.rda")
load("prepr.tbl.rda")
