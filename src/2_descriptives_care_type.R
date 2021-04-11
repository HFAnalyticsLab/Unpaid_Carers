
# =======================================================
# Project: Unpaid Carers with high care level during the pandemic
# Purpose: Descriptive statistics for care type variables only
# Author: Anne Alarilla
# Date: 11/04/2021
# =======================================================

#Load library
library(tidyverse)
library(gtsummary)
library(haven)


#Loading functions
replace_with_na_all_2 <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}


#Load data
all_clean <- readRDS(here::here('data','care_type','caring_pandemic.rds'))

all_lab<-all_clean %>% 
  select(c("carer_2","care_hours_2","care_loc_2","carer_1","care_loc_change", "care_type_change","care_hours_1","care_loc_1", "care_status")) %>% 
  replace_with_na_all_2(df=.,formule = ~.x <0)



##Setting labels for the variables

all_lab$carer_2<-ordered(as.factor(all_lab$carer_2), levels=c(1,2), 
                         labels=c("Yes", "No"))
all_lab$care_hours_2<-ordered(as.factor(all_lab$care_hours_2), levels=c(1,2,3), 
                              labels=c("Low Level Caring", "High Level Caring","No caring"))
all_lab$care_loc_2<-ordered(as.factor(all_lab$care_loc_2), levels=c(1,2,3), 
                            labels=c("Within household only", "Outside of household only","Within and outside household"))

all_lab$carer_1<-ordered(as.factor(all_lab$carer_1), levels=c(1,2), 
                         labels=c("Yes", "No"))
all_lab$care_hours_1<-ordered(as.factor(all_lab$care_hours_1), levels=c(1,2,3), 
                              labels=c("Low Level Caring", "High Level Caring","No caring"))
all_lab$care_loc_1<-ordered(as.factor(all_lab$care_loc_1), levels=c(1,2,3), 
                            labels=c("Within household only", "Outside of household only","Within and outside household"))

all_lab$care_loc_change<-ordered(as.factor(all_lab$care_loc_change), levels=c(1,2), 
                                 labels=c("No", "Yes"))
all_lab$care_type_change<-ordered(as.factor(all_lab$care_type_change), levels=c(1,2,3,4), 
                                   labels=c("No", "Yes", "No caring during pandemic","No caring pre pandemic"))
all_lab$care_status<-ordered(as.factor(all_lab$care_status), levels=c(1,2,3), 
                             labels=c("Caring pre and during", "New carers during pandemic", "Stopped caring during pandemic"))



summary(all_lab[,c("carer_2","care_hours_2","care_loc_2","carer_1","care_loc_change", "care_type_change","care_hours_1","care_loc_1","care_status")])


##New/Stopped caring
t1<-all_lab %>% 
  select(carer_1, care_hours_2) %>% 
  tbl_summary(by=care_hours_2, type=everything()~"categorical",label= list(carer_1~"If unpaid carer pre pandemic?")) %>% 
  add_p() %>% 
  bold_labels() %>% 
  as.tibble()%>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


##Care type and proximity
t2<-all_lab %>% 
  filter(carer_2=="Yes") %>% 
  select(care_hours_1,care_loc_2,care_loc_change,care_hours_2,care_loc_1) %>% 
  tbl_summary(by=care_hours_2, type=everything()~"categorical",label= list(care_loc_2~"Proximity of caring during pandemic",
                                                                           care_hours_1~"Type of unpaid carer pre pandemic",
                                                                           care_loc_change~"If care proximity changed during pandemic",
                                                                           care_loc_1~"Proximity of caring pre pandemic")) %>% 
  add_p() %>% 
  add_overall() %>% 
  bold_labels() %>% 
  as_tibble() %>% 
  select(- "**No caring**, N = 0") %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


##Saving in one doc
wb = createWorkbook()

sheet = createSheet(wb, "Care Status")

addDataFrame(as.data.frame(t1), sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Care type and proximity")

addDataFrame(as.data.frame(t2), sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, here::here('outputs', 'CareVariables.xlsx'))
         