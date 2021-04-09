##Creating frequencies table for care types

library(gtsummary)
library(labelled)
library(tidyverse)
library(officer)
library(haven)


# Functions ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

##page doc properties for outputs

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)


# Loading data ------------------------------------------------------------

pre <- readRDS(here::here('data','care_type','wave10.rds'))
during<- readRDS(here::here('data','care_type','wave6n7_covid.rds'))

var_label(pre)<- NULL
val_labels(pre)<-NULL
var_label(during)<- NULL
val_labels(during)<-NULL

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

try<-pre %>% 
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
         ##1=same_only, 2=diff_only 3==same_and_diff 4==no_caring -1==missing
         care_loc=case_when(j_aidhh==1 &(j_aidxhh==2|j_aidxhh<0)~1,
                            j_aidxhh==1& (j_aidhh==2|j_aidhh<0)~2,
                            j_aidhh==1& j_aidxhh==1~3,
                            carer==2~4,
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
         ##1=same_only, 2=diff_only 3==same_and_diff 4==no_caring -1==missing
         care_loc=case_when(aidhh==1& (caring==2|caring<0)~1,
                            caring==1& (aidhh==2|aidhh<0)~2,
                            aidhh==1& caring==1~3,
                            carer==2~4,
                            carer<0~-1)
         ) %>% 
  rename(same_household=aidhh, diff_household=caring)  



# Adding caring pre pandemic and during pandemic --------------------------


during<-during %>% 
   select(pidp, carer, care_hours, same_household, diff_household,care_loc) %>% 
   rename(same_household_2=same_household, diff_household_2=diff_household, 
          carer_2=carer, care_hours_2=care_hours, care_loc_2=care_loc) %>% 
   left_join(pre %>% 
               select(pidp, carer, care_hours, same_household, diff_household,care_loc) %>% 
               rename(same_household_1=same_household, diff_household_1=diff_household,carer_1=carer, care_hours_1=care_hours, care_loc_1=care_loc),by="pidp") %>% 
  ##If NA means did not take part in wave 10 (replacing this to -11) 
    mutate_if(is.numeric, ~replace(., is.na(.), -11)) %>% 
  ##care_loc_changes: 1=no 2=yes 3=no caring during pandemic 4=no caring pre pandemic -1=missing care data pre pandemic -2=missing care data during pandemic
    mutate(care_loc_change=case_when(care_loc_1==1&care_loc_2==1~1, 
                                               care_loc_1==2&care_loc_2==2~1,
                                               care_loc_1==3&care_loc_2==3~1,
                                               care_loc_1==1&care_loc_2==2~2,
                                               care_loc_1==1&care_loc_2==3~2,
                                               care_loc_1==2&care_loc_2==1~2,
                                               care_loc_1==2&care_loc_2==3~2,
                                               care_loc_1==3&care_loc_2==1~2,
                                               care_loc_1==3&care_loc_2==2~2,
                                               care_loc_2==4~3,
                                               care_loc_1==4~4,
                                               carer_1<0~-1,
                                               carer_2<0~-2),
    ##care_inten_change: 1=no 2=yes 3=no caring during pandemic 4=no caring pre pandemic -1=missing care data pre pandemic -2=missing care data during pandemic
           care_inten_change=case_when(care_hours_1==1&care_hours_2==1~1,
                                       care_hours_1==2&care_hours_2==2~1,
                                       care_hours_1==1&care_hours_2==2~2,
                                       care_hours_1==2&care_hours_2==1~2,
                                       care_hours_2==3~3,
                                       care_hours_1==3~4,
                                       carer_1<0~-1,
                                       carer_2<0~-2))








# Data tables ---------------------------------------------------------------

t<-during %>% 
   mutate(care_hours2_lab=as.character(case_when(care_hours_2==1~"Low level care (<20 hours per week/non personal tasks)",
                                  care_hours_2==2~"High level care (>=20 hours per week/personal tasks",
                                  care_hours_2==3~"No caring",
                                  care_hours_2<0~NA_character_)),
         care_loc2_lab=as.character(case_when(care_loc_2==1~"Within household only",
                                care_loc_2==2~"Outside the household only",
                                care_loc_2==3~"Within and Outside the household",
                                care_loc_2==4~"No caring",
                                care_loc_2<0~NA_character_)),
         carer_1_lab=as.character(case_when(carer_1==1~"Yes",
                                            carer_1==2~"No",
                                            carer_1<0~"Missing")),
         care_hours1_lab=as.character(case_when(care_hours_1==1~"Low level care (<20 hours per week/non personal tasks)",
                                                care_hours_1==2~"High level care(>=20 hours per week/personal tasks",
                                                care_hours_1==3~"No caring",
                                                care_hours_1<0~NA_character_)),
         care_loc_change_lab=as.character(case_when(care_loc_change==1~"No",
                                                    care_loc_change==2~"Yes",
                                                    care_loc_change==3~"No caring during pandemic"))) %>% 
  select(care_hours2_lab, care_loc2_lab, carer_1_lab, care_hours1_lab, care_loc_change_lab) %>% 
  tbl_summary(by=care_hours2_lab,label= list(carer_1_lab~ "If unpaid carer pre pandemic?", care_loc2_lab~"Caring Proximity during pandemic?",
                                             care_hours1_lab~"Caring Intensity pre pandemic (by hours per week/care tasks)",
                                             care_loc_change_lab~ "Change in care proxmity pre and during pandemic")) %>% 
  bold_labels() %>% 
  as_flex_table()

t<-width(t, width=1.5)

t2<-during %>% 
  select(-c(pidp, same_household_2, diff_household_2, same_household_1, diff_household_1)) %>% 
  filter(carer_2==1) %>% 
  mutate(care_hours2_lab=as.character(case_when(care_hours_2==1~"Low level care (<20 hours per week/non personal tasks)",
                                                care_hours_2==2~"High level care (>=20 hours per week/personal tasks",
                                                care_hours_2==3~"No caring",
                                                care_hours_2<0~NA_character_)),
         care_loc2_lab=as.character(case_when(care_loc_2==1~"Within household only",
                                              care_loc_2==2~"Outside the household only",
                                              care_loc_2==3~"Within and Outside the household",
                                              care_loc_2==4~"No caring",
                                              care_loc_2<0~NA_character_)),
         care_hours1_lab=as.character(case_when(care_hours_1==1~"Low level care (<20 hours per week/non personal tasks)",
                                                care_hours_1==2~"High level care(>=20 hours per week/personal tasks",
                                                care_hours_1==3~"No caring",
                                                care_hours_1<0~NA_character_)),
         pre_care=ifelse(carer_1<0,NA,carer_1)) %>% 
  select(care_hours2_lab, care_loc2_lab,pre_care, care_hours1_lab) %>% 
  tbl_summary(by=care_hours2_lab,label= list(care_loc2_lab~"Caring Proximity during pandemic?",
                                             care_hours1_lab~"Caring Intensity pre pandemic (by hours per week/care tasks)",
                                             pre_care~"If unpaid carer during pre pandemic?")) %>% 
  bold_labels() %>% 
  add_p %>% 
  as_flex_table()

t2<-width(t2, width=1.5)

tf <-here::here('outputs','caring_during_pandemic.docx')

save_as_docx(`Carers during the pandemic`= t,`With p-values`=t2, path =tf,pr_section = sect_properties)


