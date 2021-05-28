
# 
# Project: Unpaid Carers during the pandemic
# Purpose: Cleaning the care type variables for COVID-19 sub study and joining Wave 10
# Author: Anne Alarilla
# Date: 19/04/2021
# 

#Load libraries
library(tidyverse)
library(haven)
library(gtsummary)
library(survey)

# Functions ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

replace_with_na_all_2 <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}


# Loading data ------------------------------------------------------------


all<- readRDS(here::here('data','care_type','caring_pandemic.rds'))

# Create Carers during pandemic and care variables -----------------------------------------

all<-all %>% 
  mutate(carer=case_when((aidhh== 1|caring==1)~1,
                         caring==2 & aidhh==2 ~2,
                         caring==2 & aidhh<0 ~2,
                         caring<0 & aidhh==2 ~2),
         care_hours= case_when(aidhh==1 & aidhrs_cv %in% as.character(c(4:7,9)) ~ 2,
                               caring==1 & carehow3==1 ~ 2,
                               aidhh==1 & aidhrs_cv %notin% as.character(c(4:7,9))~ 1,
                               caring==1 & carehow3==0~ 1,
                               carer==2 ~ 3),
         carer_pre=case_when((j_aidxhh==1|j_aidhh==1)~1,
                             j_aidxhh==2 & j_aidhh==2 ~2,
                             j_aidxhh==2 & j_aidhh<0 ~2,
                             j_aidxhh<0 & j_aidhh==2~2),
         care_hours_pre= case_when(carer_pre==1 & j_aidhrs %in% as.character(c(4:7,9))~ 2,
                                   carer_pre==1 & j_aidhrs %notin% as.character(c(4:7,9))~ 1,
                                   carer_pre==2 ~ 3),
         care_loc_pre=case_when(j_aidhh==1 &(j_aidxhh==2|j_aidxhh<0)~1,
                                j_aidxhh==1& (j_aidhh==2|j_aidhh<0)~2,
                                j_aidhh==1& j_aidxhh==1~3,
                                j_aidhh==2& j_aidxhh==2~4),
         care_loc_cv=case_when(aidhh==1& (caring==2|caring<0)~1,
                               caring==1& (aidhh==2|aidhh<0)~2,
                               aidhh==1&caring==1~3,
                               aidhh==2&caring==2~4))


##replacing all the NAs
all_lab<-all%>% 
  replace_with_na_all_2(df=.,formule = ~.x <0)


##Setting labels for the variables

all_lab$carer<-factor(all_lab$carer, levels=c(1,2), 
                       labels=c("Yes", "No"))

all_lab$care_hours<-factor(all_lab$care_hours, levels=c(3,1,2), labels=c("No caring","Low Level Caring", "High Level Caring"))

all_lab$care_loc_cv<- factor(all_lab$care_loc_cv, levels=c(1,2,3,4), 
                             labels=c("Within household only", "Outside of household only","Within and outside household", "No caring"))

all_lab$carer_pre<-factor(all_lab$carer_pre, levels=c(1,2), 
                           labels=c("Yes", "No"))
all_lab$care_hours_pre<-factor(all_lab$care_hours_pre, levels=c(3,1,2), 
                               labels=c("No caring","Low Level Caring", "High Level Caring"))

all_lab$care_loc_pre<-factor(all_lab$care_loc_pre, levels=c(1,2,3,4), 
                              labels=c("Within household only", "Outside of household only","Within and outside household", "No caring"))



summary(all_lab[,c("carer","care_hours","care_loc_cv","carer_pre","care_hours_pre","care_loc_pre")])


saveRDS(all_lab, here::here('data', 'care_type', 'caring_pandemic_care_type.rds'))
