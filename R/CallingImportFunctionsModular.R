folder <- "C:/Users/baruj003/Desktop/21/working_R/oxford/CovidClinicalDataProcessor/data"

setwd(folder)
memory.limit(size=80000)


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
lb<-read.csv("LB.csv")
colnames(lb) <- tolower(colnames(lb))
ds<-read.csv("DS.csv")
colnames(ds) <- tolower(colnames(ds))


sa<-
  read_csv(
    "sa_all_isaric.csv",
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
    guess_max = min(8000000, 20000000),
    progress = show_progress(),
    skip_empty_rows = TRUE
  )
colnames(sa) <- tolower(colnames(sa))
INT<-
  read_csv(
    "in_all_isaric.csv",
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
    guess_max = min(8000000, 20000000),
    progress = show_progress(),
    skip_empty_rows = TRUE
  )
colnames(INT) <- tolower(colnames(INT))

###set date pull
date_pull<-as_date("2021-02-15") 

###calling data import functions

imp_dm <- import.demographic.data(dm, dtplyr.step = FALSE)
save(imp_dm, file = "imp_dm.rda")


imp_mb <- import.microb.data(mb, dtplyr.step = FALSE)

save(imp_mb, file = "imp_mb.rda")

imp_rp <- process.pregnancy.data(rp, dtplyr.step = FALSE)
save(imp_rp, file = "imp_rp.rda")

imp_int<-process.treatment.data(int, dtplyr.step = FALSE)
save(imp_int, file = "imp_int.rda")

imp_treat<-process.common.treatment.data(imp_int, minimum=10, dtplyr.step = FALSE)
save(imp_treat, file = "imp_treat.rda")

imp_icu<- process.ICU.data(ho, dtplyr.step = FALSE)
save(imp_icu, file = "imp_icu.rda")

icu_ever<-imp_icu%>%
  filter(ever_icu==TRUE)%>%
  filter(!is.na(icu_in))

imp_treat_icu<-imp_int%>%
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
  filter(n >= eval(10)) %>%
  ungroup()%>%
  mutate(treatment = glue("icu_treat_{treatment}", treatment = treatment)) %>%
  as.data.table() %>%
  dt_pivot_wider(id_cols = usubjid, names_from = treatment,  values_from = inoccur)
save(imp_treat_icu, file = "imp_treat_icu.rda") 


imp_ventilation <- process.IMV.NIV.ECMO.data(imp_int, dtplyr.step = FALSE)
save(imp_ventilation, file = "imp_ventilation.rda") 


imp_vs<- process.vital.sign.data(vs, dtplyr.step = FALSE)
save(imp_vs, file = "imp_vs.rda")

imp_lb <- process.laboratory.data(lb, dtplyr.step = FALSE)
save(imp_lb, file = "imp_lb.rda")

imp_ds <-process.outcome.data(ds, dtplyr.step = FALSE)
save(imp_ds, file = "imp_ds.rda")

imp_sa<-import.symptom.and.comorbidity.data(sa, dtplyr.step = FALSE)
tabyl(imp_sa$saterm)
save(imp_sa, file = "imp_sa.rda")


imp_comorb<-process.comorbidity.data(imp_sa, minimum=100, dtplyr.step = FALSE)
save(imp_comorb, file = "imp_comorb.rda")

imp_symptom<-process.symptom.data(imp_sa, minimum=100, dtplyr.step = FALSE)
save(imp_symptom, file = "imp_symptom.rda")

clinic_diagn<-process.covid.clinic.diagn(imp_sa,dtplyr.step = FALSE)
save(clinic_diagn, file = "clinic_diagn.rda")


#########joining all data
inport.tbl<-imp_dm%>%
  left_join(imp_mb, by = c("usubjid"))%>%
  left_join(clinic_diagn, by = c("usubjid"))%>%
  left_join(imp_comorb, by = c("usubjid"))%>%
  left_join(imp_rp, by = c("usubjid"))%>%
  left_join(imp_symptom, by = c("usubjid"))%>%
  left_join(imp_treat, by = c("usubjid"))%>%
  left_join(imp_ventilation, by = c("usubjid"))%>%
  left_join(imp_icu, by = c("usubjid"))%>%
  left_join(imp_treat_icu, by = c("usubjid"))%>%
  left_join(imp_lb, by = c("usubjid"))%>%
  left_join(imp_vs, by = c("usubjid"))%>%
  left_join(imp_ds, by = c("usubjid"))



