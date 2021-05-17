# 
# Project: Unpaid Carers during the pandemic
# Purpose: Cleaning the COVID-related variables
# Author: Anne Alarilla
# Date: 19/04/2021
# 


#Load libraries
library(tidyverse)
library(haven)
library(gtsummary)
library(survey)
library(janitor)
library(readxl)
library(scales)

# Functions ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)


# Load data ---------------------------------------------------------------

all<- readRDS(here::here('data','care_type','caring_pandemic_care_type.rds'))

all<-all%>% 
  replace_with_na_all_2(df=.,formule = ~.x <0) 


variables <- read_excel(here::here("Variables.xlsx"))

variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) %>% 
  filter(variable_type=="coronavirus")

c<-variables$Name

all_c<-all %>% 
  select(carer, carer_pre, care_hours,contains(c)) %>% 
  mutate(clinvuln_lab=factor(clinvuln_dv, levels=c(0,1,2), 
                             labels=c("No risk", "Clinically Vulnerable/Moderate Risk", "Clinically extremely vulnerable/High risk")),
         CV_symp=factor(factor(case_when(ff_hadsymp==1 | hadsymp==1| hassymp==1~1,
                                         ff_hadsymp==0 | hadsymp==2| hassymp==2~2),levels=c(1,2), labels=c("Yes", "No"))),
         longcovid_lab=factor(longcovid, levels=c(1,2), labels=c("Yes", "No")),
         testresult_lab= factor(testresult, levels=c(1,2,3,4), labels=c("Positive", "Negative", "Inconclusive", "Waiting for results")),
         fin_help=factor(ifelse(transfin>0,1,0), levels=c(1,0), labels=c("Yes", "No")),
         cvinvite_lab=factor(cvinvite, levels=c(1,2), labels=c("Yes", "No")),
         hadvac_lab=factor(case_when(hadcvvac %in% c(1,2) ~ 1,
                                     hadcvvac== 3 ~ 2,
                                     hadcvvac== 4 ~ 3), levels=c(1,2,3), labels=c("Atleast one dose", "Have an appointment", "No")),
         wah_lab= factor(case_when(wah %in% c(1,2,3)~1,
                                   wah==4~2), levels=c(1,2), labels=c("Yes","No")),
         prod_lab=factor(case_when(prodch %in% c(1,2) ~1,
                                   prodch==3 ~  2,
                                   prodch %in% c(4,5) ~3), levels=c(1,2,3),labels = c("More done", "The same", "Less done")))


