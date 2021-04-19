
# 
# Project: Unpaid Carers with high care level during the pandemic
# Purpose: Cleaning the care type variables for COVID-19 sub study and Wave 10
# Author: Anne Alarilla
# Date: 19/04/2021
# 

#Load libraries
library(tidyverse)
library(haven)


# Functions ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

# Loading data ------------------------------------------------------------

during<- readRDS(here::here('data','care_type','wave6n7_covid.rds'))
pre <- readRDS(here::here('data','care_type','wave10.rds'))

#Removing labels
val_labels(during) <- NULL
var_label(during) <- NULL
val_labels(pre) <- NULL
var_label(pre) <- NULL

# Create Carers during pandemic and care type variable -----------------------------------------

during<-during %>% 
  ##unpaid carer, 1=yes, 2=no
  mutate(carer=case_when((aidhh== 1|caring==1)~1,
                         caring==2 & aidhh==2 ~2,
                         caring==2 & aidhh<0 ~2,
                         caring<0 & aidhh==2 ~2,
                         aidhh<0 & caring<0 ~-1),
         ##care_hours, 1: <20 hours/Non personal tasks 2: >= 20 hours/personal tasks 3=no care -1=missing     
         care_hours= case_when(aidhh==1 & aidhrs_cv %in% as.character(c(4:7,9)) ~ 2,
                               caring==1 & carehow3==1 ~ 2,
                               aidhh==1 & aidhrs_cv %notin% as.character(c(4:7,9))~ 1,
                               caring==1 & carehow3==0~ 1,
                               carer==2 ~ 3,
                               carer==-1|aidhrs_cv<0| carehow3<0~-1))

#Joining the during pandemic with pre pandemic data set

all<-during %>% 
  left_join(pre, by="pidp")

saveRDS(all, here::here('data', 'care_type', 'caring_pandemic.rds'))




