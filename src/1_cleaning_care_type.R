
# =======================================================
# Project: Unpaid Carers with high care level during the pandemic
# Purpose: Cleaning the care type variables for COVID-19 sub study and Wave 10
# Author: Anne Alarilla
# Date: 11/04/2021
# =======================================================

#Load libraries
library(tidyverse)
library(haven)


# Functions ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

# Loading data ------------------------------------------------------------

pre <- readRDS(here::here('data','care_type','wave10.rds'))
during<- readRDS(here::here('data','care_type','wave6n7_covid.rds'))

##pre 
##j_aidxhh= caring for person not living with you (sick, disabled, elderly)
##j_aidhh= caring for person living with you  (sick, disabled, elderly)
##j_aidhrs=hours of care both indoor and outdoor

#during
##cf_caring= did you provide help or support to family, friends or neighbours you do not live with
##cf_aidhh/ cg_aidhh= cares for sick, disabled, elderly living with you 
##cf_aidhrs_cv/ cg_aidhrs_cv= hours of caring inside the household
## For caring outdoors, care hours is not available but you do have the type of care
#We can extract the helping with personal needs 3

##Creating missing values so far, -10= did not take part in wave 7 covid sub study, -11= did not take part in wave 10
##everything else is specified by Understanding society 


# Caring pre pandemic -----------------------------------------------------

pre<-pre %>% 
  ##unpaid carer, 1=yes, 2=no -1=missing
  mutate(carer=case_when((j_aidxhh==1|j_aidhh==1)~1,
                         j_aidxhh==2 & j_aidhh==2 ~2,
                         j_aidxhh==2 & j_aidhh<0 ~2,
                         j_aidxhh<0 & j_aidhh==2~2,
                         j_aidxhh<0 & j_aidhh<0 ~ -1),
         ##care_hours, 1: <20 hours 2: >= 20 hours 3=no care
         care_hours= case_when(carer==1 & j_aidhrs %in% as.character(c(4:7,9))~ 2,
                               carer==1 & j_aidhrs %notin% as.character(c(4:7,9))~ 1,
                               carer==2 ~ 3,
                               carer==-1 ~ -1),
         ##1=same_only, 2=diff_only 3==same_and_diff -4==no_caring -1==missing
         care_loc=case_when(j_aidhh==1 &(j_aidxhh==2|j_aidxhh<0)~1,
                            j_aidxhh==1& (j_aidhh==2|j_aidhh<0)~2,
                            j_aidhh==1& j_aidxhh==1~3,
                            carer==2~-4,
                            carer<0~-1)) %>% 
  rename(diff_household=j_aidxhh,same_household=j_aidhh) 


# Caring during pandemic --------------------------------------------------


during<-during %>% 
  ##unpaid carer, 1=yes, 2=no
  mutate(carer=case_when((aidhh== 1|caring==1)~1,
                         caring==2 & aidhh==2 ~2,
                         caring==2 & aidhh<0 ~2,
                         caring<0 & aidhh==2 ~2,
                         aidhh<0 & caring<0 ~-1),
         ##care_hours, 1: <20 hours/Non personal tasks 2: >= 20 hours/personal tasks 3=no care -1=missing     
         care_hours= case_when(aidhh==1 & aidhrs %in% as.character(c(4:7,9)) ~ 2,
                               caring==1 & cf_carehow3==1 ~ 2,
                               aidhh==1 & aidhrs %notin% as.character(c(4:7,9))~ 1,
                               caring==1 & cf_carehow3==0~ 1,
                               carer==2 ~ 3,
                               carer==-1|aidhrs<0| cf_carehow3<0~-1),
         ##1=same_only, 2=diff_only 3==same_and_diff -4==no_caring -1==missing
         care_loc=case_when(aidhh==1& (caring==2|caring<0)~1,
                            caring==1& (aidhh==2|aidhh<0)~2,
                            aidhh==1& caring==1~3,
                            carer==2~-4,
                            carer<0~-1)) %>% 
  rename(same_household=aidhh, diff_household=caring)  


# Adding caring pre pandemic and during pandemic --------------------------


all_clean<-during %>% 
   select(pidp, carer, care_hours, same_household, diff_household,care_loc) %>% 
   rename(same_household_2=same_household, diff_household_2=diff_household, 
          carer_2=carer, care_hours_2=care_hours, care_loc_2=care_loc) %>% 
   left_join(pre %>% 
               select(pidp, carer, care_hours, same_household, diff_household,care_loc) %>% 
               rename(same_household_1=same_household, diff_household_1=diff_household,carer_1=carer, care_hours_1=care_hours, care_loc_1=care_loc),by="pidp") %>% 
  ##If NA means did not take part in wave 10 (replacing this to -11) 
    mutate_if(is.numeric, ~replace(., is.na(.), -11)) %>% 
    mutate(care_status=case_when(carer_1==1 & carer_2==1~1,
                                 carer_1==2& carer_2==1~2,
                                 carer_1==1 & carer_2==2~3,
                                 carer_1==-1|carer_2==-2~-1),
       care_loc_change=case_when(care_loc_1==1&care_loc_2==1~1, 
                                               care_loc_1==2&care_loc_2==2~1,
                                               care_loc_1==3&care_loc_2==3~1,
                                               care_loc_1==1&care_loc_2==2~2,
                                               care_loc_1==1&care_loc_2==3~2,
                                               care_loc_1==2&care_loc_2==1~2,
                                               care_loc_1==2&care_loc_2==3~2,
                                               care_loc_1==3&care_loc_2==1~2,
                                               care_loc_1==3&care_loc_2==2~2,
                                     #Missing: not caring during pandemic
                                               care_loc_2==-4~-3,
                                     #Missing: not caring pre pandemic
                                               care_loc_1==-4~-4,
                                     #Missing: no caring data pre
                                               carer_1<0~-1,
                                     #Missing:no caring data during
                                               carer_2<0~-2),
           care_type_change=case_when(care_hours_1==1&care_hours_2==1~1,
                                       care_hours_1==2&care_hours_2==2~1,
                                       care_hours_1==1&care_hours_2==2~2,
                                       care_hours_1==2&care_hours_2==1~2,
                                       #Missing: not caring during pandemic
                                       care_hours_2==3~-3,
                                       #Missing:not caring pre pandemic
                                       care_hours_1==3~-4,
                                       #Missing: no caring data pre
                                       carer_1<0~-1,
                                       #Missing:no caring data during
                                       carer_2<0~-2))

saveRDS(all_clean, here::here('data', 'care_type', 'caring_pandemic.rds'))
