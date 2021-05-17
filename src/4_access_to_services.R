# 
# Project: Unpaid Carers during the pandemic
# Purpose: Cleaning the access to services variables
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


variables <- read_excel(here::here("Variables.xlsx"))

variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) %>% 
  filter(variable_type=="services")

s<-variables$Name

all_s<-all %>% 
  select(pidp,carer, carer_pre, care_hours,contains(s),probit_lasso_wgt_t25, psu, strata) %>% 
  rename(treat_OT=hcond_treat1, 
         med_immune=hcond_treat2, 
         chemo_cancer=hcond_treat3,
         radio_cancer=hcond_treat4,
         Other_treat_med=hcond_treat5,
         None_of_above=hcond_treat6,
         NHS_treat_con_plan_prog=treatment1,
         NHS_Op_proc_plan=treatment2,
         NHS_chemo_radio_plan=treatment3,
         NHS_other_treat_plan=treatment4,
         NHS_no_treat_plan=treatment5)


all_s<- all_s %>% ##Check that these are for those who needs it##
  mutate(sum_treat_immune=rowSums(all_s[ ,c(5:9)], na.rm=TRUE),
         treat_immune=ifelse(sum_treat_immune>0,"Yes","No"),
         sum_NHS_treat = rowSums(all_s[ ,c(11:14)], na.rm=TRUE), 
         wait_for_NHS_treat= ifelse(sum_NHS_treat>0, "Yes", "No"),
         NHS_treat_con_plan_prog = factor(NHS_treat_con_plan_prog, levels=c(1,0), labels=c("Yes","No")),
         NHS_Op_proc_plan= factor(NHS_Op_proc_plan, levels=c(1,0), labels=c("Yes","No")),                                
         NHS_chemo_radio_plan= factor(NHS_chemo_radio_plan, levels=c(1,0), labels=c("Yes","No")),                         
         NHS_other_treat_plan= factor(NHS_other_treat_plan, levels=c(1,0), labels=c("Yes","No")),                         
                               #   ==1~1,
                               #           NHS_Op_proc_plan==1~2,
                               #           NHS_chemo_radio_plan==1~3,
                               #           NHS_other_treat_plan==1~4),
                               # levels=c(1,2,3,4), labels=c("Tests/consultations planned or in progress",
                               #                             "Operation / procedure planned",
                               #                             "Targeted, chemotherapy or radiotherapy planned or in progress",
                               #                             "Other treatment planned")),
         NHS_reason_canceltreat=factor(canceltreat, levels=c(1,2,3,4),
                            labels=c("Yes, cancelled/postponed by NHS", "Yes, alternative provided", "Yes, I cancelled or postponed", "No, treatment continued as planned")),
         nhs_access=factor(case_when(nhsnowgp2 %in% c(1,2)~ 1,
                              nhsnowgp2 %in% c(3,4)~2,
                              nhsnow1112 ==1 ~ 1,
                              nhsnow1112 %in% c(2,3)~2,
                              nhsnowip2 %in% c(1,4)~1,
                              nhsnowip2 %in% c(2,3)~2,
                              nhsnowop2 %in% c(1,2,5)~1,
                              nhsnowop2 %in% c(3,4)~2),levels=c(1,2), labels=c("Yes","No")),
         nhs_presciption_acess=factor(nhsnowpm2, levels=c(1,2,3), labels=c("Yes", "No", "Not Required")),
         nhs_access_own_cancel=factor(case_when(nhsnowgp2==4~1,
                                                nhsnowgp2==3~2,
                                                nhsnow1112==3~1,
                                                nhsnow1112==2~2,
                                                nhsnowip2==3~1,
                                                nhsnowip2==4~3,
                                                nhsnowop2==4~1,
                                                nhsnowop2==3~2,
                                                nhsnowop2==5~3),levels=c(1,2,3), 
         labels=c("I postponed, cancelled or decided not to seek help this time","I was not able to access", "Different treatment provided")),
         respite=factor(ifelse(respitenow>0,1,2), levels=c(1,2), labels=c("Yes","No")),
         respite_hours=factor(case_when(respitenow<=17 ~ 1,
                                        respitenow>17 & respitenow<35~2,
                                        respitenow>=35~3),levels=c(1,2,3), 
                              labels=c("Less than or equal to 17 hours per week",
                                       "More than 17 and Less than 35 hours per week", 
                                       "More than 35 hours per week")),
         chsc_access_fc_psy=factor(case_when(chscnowcarer2 %in% c(1,2,3)~1,
                                      chscnowcarer2==4~2,
                                      chscnowpsy2 %in% c(1,2,3)~1,
                                      chscnowpsy2==4~2), levels=c(1,2),
                            labels=c("Yes","No")),
         nowcarer_lab=factor(case_when(chscnowcarer2 %in% c(1:3)~1,
                                       chscnowcarer2==4~2),levels=c(1,2),
                              labels=c("Yes", "No")),
         psy_lab=factor(case_when(chscnowpsy2 %in% c(1:3)~1,
                                  chscnowpsy2==4 ~2),levels=c(1,2), labels=c("Yes","No")),
         nowcarer_pre_lab=factor(j_hlsv2, levels=c(1,0), labels=c("Yes","No")),
         psy_pre_lab=factor(j_hlsv7, levels=c(1,0), labels=c("Yes","No")),
         nhs_access_pre=factor(case_when(j_hl2gp<0 ~ 1,
                                         j_hl2gp==0 ~2,
                                         j_hl2hop<0 ~ 1,
                                         j_hl2hop==0 ~2,
                                         j_hosp==1 ~ 1,
                                         j_hosp==2 ~ 1),levels=c(1,2), labels=c("Yes","No")))
 
saveRDS(all_s, here::here('data', 'care_type', 'services_all.rds'))        
