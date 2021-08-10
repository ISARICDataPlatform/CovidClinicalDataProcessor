#Read the data (not processed data)
library(RColorBrewer)
library(incidence)
library(shiny)
library(shinydashboard)
library(magrittr)
library(lattice)
library(stringr)
library(plyr)
library(FSA)
library(DescTools)
library(vcd)
library(rcompanion)
library(ggplot2)
library(MASS)
library(sgr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(Hmisc)
library(RColorBrewer)
library(dtplyr) 
library(data.table)
library(tidyfast)
library(naniar)
library(shinyWidgets)
library(viridis)
library(hrbrthemes)
library(splitstackshape)
library(glue)
library(lubridate)
library(grid)
library(gtable)
library(gridExtra)

#This script was built to check the agregated tables for our graphs. 
#' Prepare Table6. key times variable
#' @param input.tbl Input tibble (output of \code{data.preprocessing})
#' @import dplyr purrr tidyr janitor
#' @return A \code{tibble} containing the input data for the Patient characteristics table
#' @export key.times.table
#'
table_outliers <- function(input.tbl){
  data<-select(input.tbl, c(starts_with("lab_"))) %>%
    mutate_at(vars(all_of(starts_with("lab_"))), function(x){as.numeric(x)})%>%
    pivot_longer(c(starts_with("lab_")), names_to = "parameter", values_to = "value")
  out<-select(input.tbl, c(starts_with("vs_"))) %>%
    mutate_at(vars(all_of(starts_with("vs_"))), function(x){as.numeric(x)})%>%
    pivot_longer(c(starts_with("vs_")), names_to = "parameter", values_to = "value")%>%
    rbind(data)%>%
    filter(!is.na(value))%>%
    filter(value>=0)%>%
    group_by(parameter)%>%
    summarise(median=median(value,na.rm=T),
              iqr=IQR(value,na.rm=T),
              minimum=min(value,na.rm=T),
              maximum=max(value,na.rm=T),
              lower_lim=quantile(value,0.025,na.rm=T), 
              upper_lim=quantile(value,0.975,na.rm=T))
              
}


library(flextable)


folder <- "C:/Users/baruj003/Desktop/21/working_R/oxford/CovidClinicalDataProcessor"
setwd(folder)


load("ISVARIC_dash_db.rda")

#Overall table
table_outlier_flex <- table_outliers(input.tbl)
ft_1 <- flextable(table_outlier_flex)
ft_1 <- add_header_lines(ft_1, values = "Median, IQR, Minimum, Maximum, lower_lim (2.5%), upper_lim (97.5%) for all ages")

#Low age table
ft_2_data <- filter(input.tbl, age<15)
table_outlier_flex <- table_outliers(ft_2_data)
ft_2 <- flextable(table_outlier_flex)
ft_2 <- add_header_lines(ft_2, values = "Median, IQR, Minimum, Maximum, lower_lim (2.5%), upper_lim (97.5%), <14 years")



#high age table
ft_3_data <- filter(input.tbl, age>14)
table_outlier_flex <- table_outliers(ft_3_data)
ft_3 <- flextable(table_outlier_flex)
ft_3 <- add_header_lines(ft_3, values = "Median, IQR, Minimum, Maximum, lower_lim (2.5%), upper_lim (97.5%), >=15 years")


