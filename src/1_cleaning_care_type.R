##Creating frequencies table for care types

library(gtsummary)
library(labelled)
library(tidyverse)
library(officer)


#Functions
`%notin%` <- Negate(`%in%`)


##page doc properties for outputs

sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)

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
  mutate(carer=case_when((j_aidxhh==1|j_aidhh==1)~1,
                         j_aidxhh==2 & j_aidhh==2 ~2),
  ##care_hours, 1: <20 hours 2: >= 20 hours 3=no care
         care_hours= case_when(carer==1 & j_aidhrs %in% as.character(c(4:7,9))~ 2,
                               carer==1 & j_aidhrs %notin% as.character(c(4:7,9,NA))~ 1,
                               carer==2 ~ 3))%>%
  rename(diff_household=j_aidxhh,same_household=j_aidhh) 
  
t1<-pre %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week",
                                  care_hours==2~ ">= 20 hours per week",
                                  care_hours== 3 ~ "No caring responsibilities")) %>% 
  select(-c(pidp, j_aidhrs,care_hours)) %>% 
  tbl_summary(label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                                                      same_household~"Caring for someone in the same household?",
                          carehrs_label~ "Caring Intensity (by hours per week)"),
                                                     percent="column") %>% 
  bold_labels() %>% 
  as_flex_table()


t2<-pre %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week",
                                  care_hours==2~ ">= 20 hours per week",
                                  care_hours== 3 ~ "No caring responsibilities")) %>% 
  select(-c(pidp, j_aidhrs, care_hours)) %>% 
  tbl_summary(by=carehrs_label, missing= "ifany", label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                                                           same_household~"Caring for someone in the same household?"),
              percent="row") %>% 
  bold_labels() %>% 
  add_overall() %>% 
  as_flex_table()

t2<- width(t2, width = 1.5)
##Saving the tables in one doc

tf1 <-here::here('outputs','caring_pre.docx')

save_as_docx('Caring Pre pandemic, Overall'= t1, 'Care Type, Pre Pandemic'=t2, 
             path =tf1,pr_section = sect_properties)


# During pandemic (wave 6 and 7 of covid sub study) -----------------------
var_label(during)<- NULL
val_labels(during)<-NULL

during<-during %>% 
  ##unpaid carer, 1=yes, 2=no
  mutate(carer=case_when((aidhh== 1|caring==1)~1,
                         caring==2 & aidhh==2 ~2),
         ##care_hours, 1: <20 hours/Non personal tasks 2: >= 20 hours/personal tasks 3=no care       
  care_hours= case_when(aidhh==1 & aidhrs %in% as.character(c(4:7,9)) ~ 2,
                        caring==1 & (cf_carehow3==1 | cf_carehow4==1 | cf_carehow5==1 | cf_carehow6==1 )~ 2,
                        aidhh==1 %notin% as.character(c(4:7,9,NA))~ 1,
                        caring==1 & (cf_carehow1==1 | cf_carehow2==1 | cf_carehow7==1 | cf_carehow8==1 | cf_carehow9==1 | cf_carehow10==1 )~ 1,
                        carer==2 ~ 3)) %>% 
  rename(same_household=aidhh, diff_household=caring)

t3<-during %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week/Non Personal Tasks",
                                  care_hours==2~ ">= 20 hours per week/ Personal Tasks",
                                  care_hours== 3 ~ "No caring responsibilities")) %>% 
  select(carehrs_label, carer, same_household, diff_household) %>% 
  tbl_summary(label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                          same_household~"Caring for someone in the same household?",
                          carehrs_label~ "Caring Intensity (by hours per week/type of care tasks)"),
              percent="column") %>% 
  bold_labels() %>% 
  as_flex_table()


t4<-during %>% 
  mutate(carehrs_label= case_when(care_hours==1~ "<20 hours per week/Non Personal Tasks",
                                  care_hours==2~ ">= 20 hours per week/ Personal Tasks",
                                  care_hours== 3 ~ "No caring responsibilities")) %>% 
  select(carehrs_label, carer, same_household, diff_household) %>% 
  tbl_summary(by=carehrs_label, missing= "ifany", label= list(carer~ "If unpaid carer?", diff_household~"Caring for someone in a different household?",
                                                              same_household~"Caring for someone in the same household?"),
              percent="row") %>% 
  bold_labels() %>% 
  add_overall() %>% 
  as_flex_table()

t4<- width(t2, width = 1.5)


# Combining the two time points---------------------------------------------

all<-pre %>% 
  select(pidp, carer, care_hours, same_household, diff_household) %>% 
  rename(same_household_1=same_household, diff_household_1=diff_household, carer_1=carer, care_hours_1=care_hours) %>% 
  full_join(during %>% 
              select(pidp, carer, care_hours, same_household, diff_household) %>% 
              rename(same_household_2=same_household, diff_household_2=diff_household, carer_2=carer, care_hours_2=care_hours),by="pidp") %>% 
  # 1=yes 2=no
  mutate(carer_all= case_when((carer_1==1|carer_2==1)~1,
                              carer_1==2 & carer_2==2~2,
                              is.na(carer_1)& carer_2==2~2,
                              is.na(carer_2)&carer_1==2~2),
         caring_cont=case_when(carer_1==1&carer_2==1~ 1,  
                               carer_1==1&(is.na(carer_2)|carer_2==2)~2,
                               (is.na(carer_1)| carer_1==2) &carer_2==1~2),
         caring_new=case_when((is.na(carer_1)|carer_1==2)& carer_2==1~ 1,  
                              carer_1==1~2),
         caring_stopped=case_when(carer_2==2& carer_1==1~ 1,  
                                  carer_2==1~2),
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
                                         
all %>% 
  filter(carer_all==1) %>% 
  select(carer_all,caring_cont,caring_new, caring_stopped) %>% 
  tbl_summary(by=carer_all)
  
t5<-all %>% 
  filter(carer_all==1) %>% 
    mutate(carer_all=ifelse(carer_all==1,"Carer"," ")) %>% 
    select(carer_all, caring_cont, caring_new, caring_stopped) %>% 
    tbl_summary(by=carer_all, label= list(caring_cont~ "Caring Pre and during COVID-19", caring_new~"Caring during COVID-19 only (new carers)",
                                          caring_stopped~"Caring Pre COVID-19 only (stopped caring)")) %>% 
    bold_labels() %>% 
     as_flex_table()

  
t6<-all %>% 
    filter(carer_all==1) %>% 
    mutate(carer_all=ifelse(carer_all==1,"Carer"," ")) %>% 
    select(carer_all, high_care_int_cont, low_care_int_cont, change_care_intensity) %>% 
    tbl_summary(by=carer_all, label=list(high_care_int_cont ~ ">=20 hours of care/Personal care, Pre and during COVID-19",
                                         low_care_int_cont ~ "<20 hours of care per week/Non personal tasks only, Pre and during COVID-19",
                                         change_care_intensity ~ "Changed care intensity between Pre and during COVID-19")) %>% 
  bold_labels() %>% 
  as_flex_table()
  
  
t7<-all %>% 
    filter(carer_all==1) %>% 
      mutate(carer_all=ifelse(carer_all==1,"Carer"," ")) %>% 
    select(carer_all, same_household_cont, diff_household_cont, change_household) %>% 
    tbl_summary(by=carer_all, label= list(same_household_cont~ "Caring within household, Pre and during COVID-19",
                                          diff_household_cont~ "Caring outside household, Pre and during COVID-19",
                                          change_household~ "Change care proximity between Pre and during COVID-19")) %>% 
  bold_labels() %>% 
  as_flex_table()

  
##Saving the tables in one doc
  
tf <-here::here('outputs','caring.docx')
  
save_as_docx('Caring Pre COVID-19, Overall'= t1, 'Care Type, Pre COVID-19'=t2,
             'Caring during COVID-19, Overall'= t3, 'Care Type, During COVID-19'=t4, 
             'Caring Pre and during COVID 19, Overall'=t5, 'Care Intensity, Pre and during COVID-19'=t6,
             'Caring Proximity, Pre and during COVID-19'=t7,
              path =tf1,pr_section = sect_properties)
  
  
  