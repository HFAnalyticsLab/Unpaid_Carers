# 
# Project: Unpaid Carers during the pandemic
# Purpose: Cleaning the demographics
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
library(ggfittext)
library(xlsx)
library(scales)

# Functions ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

# Loading data and Variable list ------------------------------------------------------------

all<- readRDS(here::here('data','care_type','caring_pandemic_care_type.rds'))

variables <- read_excel(here::here("Variables.xlsx"))

variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) %>% 
  filter(variable_type=="demographics")
  
dem<-variables$Name

#Selecting Demographic variables

all_dem<-all %>% 
  select(pidp, carer, carer_pre, care_hours,contains(dem),probit_lasso_wgt_t25, psu, strata) %>% 
  mutate(sex_lab=factor(sex_cv, levels=c(1,2), labels=c("Male", "Female")),
         race=factor(case_when(racel_dv %in% (1:4)~ 1,
                        racel_dv %in% (5:8)~ 2,
                        racel_dv %in% (9:13)~ 3,
                        racel_dv %in% (14:16)~ 4,
                        racel_dv %in% c(17,97)~ 5),
         levels=c(1,3,4,2,5), 
         labels=c("White","Asian or Asian British", "Black or Black British", "Mixed","Other ethnic group")),
         
         gor_dv2=factor(gor_dv,levels=c(1:12), 
         labels=c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands",
        "East of England", "London","South East","South West", "Wales", "Scotland", "Northern Ireland")),
         
        age_band=cut(all$age, breaks=c(-Inf,24,34,44,54,64,74,84,Inf),
                             labels=c("16-24","25-34","35-44","45-54","55-64","65-74","75-84","85+")),
         
        employment_pre=factor(case_when(j_employ==1&j_jbhrs<35~1,
                                    j_employ==1&j_jbhrs>=35~2,
                                    j_employ==2~3),
                          levels=c(1,2,3),
                          labels=c("Employed-Full time", "Employed-Part Time", "Not currently employed")),
        nssec_pre=factor(j_jbnssec3_dv, levels=c(1,2,3),labels=c("Management & professional","Intermediate","Routine")),
        marital_status=factor(ifelse(j_mastat_dv %in% c(0,1),1,
                        ifelse(j_mastat_dv %in% c(2, 3, 10),2,
                               ifelse(j_mastat_dv %in% c(4, 5, 7, 8),3,
                                      ifelse(j_mastat_dv %in% c(6, 9), 4, NA)))),
                 level=c(1,2,3,4),
                 label=c("Single","In partnership","Separated","Widowed")),
        # child_u15=factor(case_when(j_nch14resp==0~0,
        #                              j_nch14resp==1~1,
        #                              j_nch14resp>=2~2),
        #                   levels=c(0,1,2),
        #                    labels=c("None", "One", "Two+")),
        child_u16=factor(case_when(j_nchunder16==0~0,
                                   j_nchunder16>0~1),
                         levels=c(0,1),
                         labels=c("None", "Atleast One")),
        hh_child_u16=factor(case_when(j_nchild_dv==0~0,
                                   j_nchild_dv>0~1),
                         levels=c(0,1),
                         labels=c("None", "Atleast One")),
        howlong_lab=factor(case_when(howlng_cv<=17 ~ 1,
                                     howlng_cv>17 & howlng_cv<35~2,
                                     howlng_cv>=35~3),levels=c(1,2,3), 
                            labels=c("Less than or equal to 17 hours per week",
                                      "More than 17 and Less than 35 hours per week", 
                                       "More than 35 hours per week")),
        keyworkerstatus_lab=factor(ifelse(keyworksector==9,"No","Yes")),
        keyworksec_lab=factor(keyworksector, levels=c(1,2,3,4,5,6,7,8,9),
                              labels=c("Health and Social Care (does not specify carers)",
                                       "Education and Childcare",
                                        "Key public sevices",
                                       "Local and national government",
                                       "Food and other necessary goods",
                                       "Public safety and national security",
                                       "Transport",
                                       "Utilities, communications and financial services",
                                       "Not a key worker")),
        race_plus=factor(case_when(racel_dv %in% c(1:2)~ 1, #White
                                   racel_dv %in% c(3,4)~2, 
                                   racel_dv== 9~3,
                                   racel_dv==10~4,
                                   racel_dv==11~5,
                                   racel_dv==12~6,
                                   racel_dv==14~7,
                                   racel_dv==15~8),
                                   levels=c(1:8), 
                    labels=c("White","Irish", "Indian", "Pakistani","Bangladeshi","Chinese",
                             "Caribbean", "African")),
        urban_label=factor(j_urban_dv, levels=c(1,2), labels=c("Urban area", "Rural Area")))

#Saving data
saveRDS(all_dem, here::here('data', 'care_type', 'demographics_all.rds'))


