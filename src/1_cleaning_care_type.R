##Creating frequencies table for care types

library(gtsummary)
library(labelled)

#Functions
`%notin%` <- Negate(`%in%`)

#Loading data 
pre <- readRDS(here::here('data','care_type','wave10.rds'))
during<- readRDS(here::here('data','care_type','wave6_covid.rds'))

##
#pre 
##j_aidxhh= caring for person not living with you (sick, disabled, elderly)
##j_aidhh= caring for person living with you  (sick, disabled, elderly)
##j_aidhrs=hours of care both indoor and outdoor

#during
##cf_caring= did you provide help or support to family, friends or neighbours you do not live with
##cf_aidhh/ cg_aidhh= cares for sick, disabled, elderly living with you 
##cf_aidhrs_cv/ cg_aidhrs_cv= hours of caring inside the household
## For caring outdoors, care hours is not available but you do have the type of care
#We can extract the helping with personal needs, dealing with personal affairs, washing ironing
##providing or cooking meals (3,4,5,6)


# Pre pandemic (wave 10) --------------------------------------------------


##removing the labels
var_label(pre)<- NULL
val_labels(pre)<-NULL

pre<-pre %>% 
  ##unpaid carer, 1=yes, 2=no
  mutate(carer=ifelse((j_aidxhh==1|j_aidhh==1),1,2),
  ##care_hours, 1: <20 hours 2: >= 20 hours 3=no care
         care_hours= case_when(carer==1 & j_aidhrs %in% as.character(c(4:7,9))~ 2,
                               carer==1 & j_aidhrs %notin% as.character(c(4:7,9,NA))~ 1,
                               carer==2 ~ 3))%>%
  rename(diff_household=j_aidxhh,same_household=j_aidhh) 


pre %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week",
                                  care_hours==2~ ">= 20 hours per week",
                                  care_hours== 3 ~ "No caring responsbilities")) %>% 
  #care_lab= case_when(carer==1 ~ "Yes", carer==2 ~"No"),
  #diff_household_lab=ifelse(diff_household==1, "Yes", "No"),
  #same_household_lab= ifelse(same_household==1, "Yes","No")) %>% 
  select(-c(pidp, j_aidhrs,care_hours)) %>% 
  tbl_summary(label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                                                              same_household~"Caring for someone in the same household?"),
              percent="column") %>% 
  bold_labels() 

pre %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week",
                                  care_hours==2~ ">= 20 hours per week",
                                  care_hours== 3 ~ "No caring responsbilities")) %>% 
         #care_lab= case_when(carer==1 ~ "Yes", carer==2 ~"No"),
         #diff_household_lab=ifelse(diff_household==1, "Yes", "No"),
         #same_household_lab= ifelse(same_household==1, "Yes","No")) %>% 
  select(-c(pidp, j_aidhrs,care_hours)) %>% 
  tbl_summary(by=carehrs_label, missing= "ifany", label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                                                           same_household~"Caring for someone in the same household?"),
              percent="row") %>% 
  bold_labels() %>% 
  add_overall()


# During pandemic (wave 6 and 7 of covid sub study) -----------------------
var_label(during)<- NULL
val_labels(during)<-NULL

during<-during %>% 
  ##unpaid carer, 1=yes, 2=no
  mutate(carer=ifelse((aidhh== 1| caring==1),1,2),
         ##care_hours, 1: <20 hours/Non personal tasks 2: >= 20 hours/personal tasks 3=no care       
  care_hours= case_when(aidhh==1 & aidhrs %in% as.character(c(4:7,9)) ~ 2,
                        caring==1 & (cf_carehow3==1 | cf_carehow4==1 | cf_carehow5==1 | cf_carehow6==1 )~ 2,
                        aidhh==1 %notin% as.character(c(4:7,9,NA))~ 1,
                        caring==1 & (cf_carehow1==1 | cf_carehow2==1 | cf_carehow7==1 | cf_carehow8==1 | cf_carehow9==1 | cf_carehow10==1 )~ 1,
                        carer==2 ~ 3)) %>% 
  rename(same_household=aidhh, diff_household=caring)

during %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week/Non Personal Tasks",
                                  care_hours==2~ ">= 20 hours per week/ Personal Tasks",
                                  care_hours== 3 ~ "No caring responsbilities")) %>% 
  select(carehrs_label, carer, same_household, diff_household) %>% 
  tbl_summary(label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                          same_household~"Caring for someone in the same household?"),
              percent="column") %>% 
  bold_labels() 


during %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week/Non Personal Tasks",
                                  care_hours==2~ ">= 20 hours per week/ Personal Tasks",
                                  care_hours== 3 ~ "No caring responsbilities")) %>% 
  select(carehrs_label, carer, same_household, diff_household) %>% 
  tbl_summary(by=carehrs_label, missing= "ifany", label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                                                              same_household~"Caring for someone in the same household?"),
              percent="row") %>% 
  bold_labels() %>% 
  add_overall()




# Combining the two time points---------------------------------------------

##both
all<-pre %>% 
  select(pidp, carer, care_hours, same_household, diff_household) %>% 
  rename(same_household_1=same_household, diff_household_1=diff_household, carer_1=carer, care_hours_1=care_hours) %>% 
  full_join(during %>% 
              select(pidp, carer, care_hours, same_household, diff_household) %>% 
              rename(same_household_2=same_household, diff_household_2=diff_household, carer_2=carer, care_hours_2=care_hours),by="pidp") %>% 
  # 1=yes 2=no
  mutate(carer_all= ifelse(carer_1==1|carer_2==1,1,2),
         caring_cont=ifelse(carer_1==1&carer_2==1,1,2),
         caring_new=ifelse((is.na(carer_1) |carer_1==2)& carer_2==1,1,2),
         caring_stopped=ifelse((is.na(carer_2) |carer_2==2)& carer_1==1,1,2),
         same_household_cont=ifelse(same_household_1==1&same_household_2==1,1,2),
         diff_household_cont=ifelse(diff_household_1==1&diff_household_2==1,1,2),
         change_household=case_when(same_household_1==1 & diff_household_2==1 ~ 1,
                                    diff_household_1==1 & same_household_2==1 ~ 1,
                                    same_household_1==1 & same_household_2==1 ~ 2, 
                                    diff_household_1==1 & diff_household_2==1 ~ 2),
         high_care_int_cont=ifelse(care_hours_1==2& care_hours_2==2,1,2),
         low_care_int_cont=ifelse(care_hours_1==1& care_hours_2==1,1,2),
         change_care_intensity=case_when(care_hours_1==1& care_hours_2==2 ~ 1,
                                         care_hours_1==2& care_hours_2==1~ 1,
                                         care_hours_1==1 & care_hours_2==1 ~2,
                                         care_hours_1==2& care_hours_2==2 ~2,
                                         care_hours_1==3 & care_hours_2 %in%(1:2)~1,
                                         care_hours_1 %in% (1:2) & care_hours_2==3 ~1))
                                         
alll %>% 
  tbl_summary()
  
  all %>% 
    select(carer_all, caring_cont, caring_new, caring_stopped) %>% 
    tbl_summary(by=carer_all)
  
  
  
  
  