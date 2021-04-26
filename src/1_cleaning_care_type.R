
# 
# Project: Unpaid Carers with high care level during the pandemic
# Purpose: Cleaning the care type variables for COVID-19 sub study and joining Wave 10
# Author: Anne Alarilla
# Date: 19/04/2021
# 

#Load libraries
library(tidyverse)
library(haven)


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
                                j_aidhh==1& j_aidxhh==1~3),
         care_loc_cv=case_when(aidhh==1& (caring==2|caring<0)~1,
                               caring==1& (aidhh==2|aidhh<0)~2,
                               aidhh==1&caring==1~3),
    ##Check this
         care_status=case_when(carer_pre==1 & carer==1~1,
                               carer_pre==2& carer==1~2,
                               carer_pre==1&carer==2~3),
         care_loc_change=case_when(care_loc_pre==1&care_loc_cv==1~1, 
                                   care_loc_pre==2&care_loc_cv==2~1,
                                   care_loc_pre==3&care_loc_cv==3~1,
                                   care_loc_pre==1&(care_loc_cv==2|care_loc_cv==3)~2,
                                   care_loc_pre==2&(care_loc_cv==1|care_loc_cv==3)~2,
                                   care_loc_pre==3&(care_loc_cv==1|care_loc_cv==2)~2),
         care_type_change=case_when(care_hours_pre==1&care_hours==1~1,
                                    care_hours_pre==2&care_hours==2~1,
                                    care_hours_pre==1&care_hours==2~2,
                                    care_hours_pre==2&care_hours==1~2))




all_lab<-all%>% 
  replace_with_na_all_2(df=.,formule = ~.x <0) %>% 
  mutate(betaindin_xw=ifelse(is.na(betaindin_xw), betaindin_xw_t,betaindin_xw))


##Setting labels for the variables

all_lab$carer<-ordered(as.factor(all_lab$carer), levels=c(1,2), 
                       labels=c("Yes", "No"))
all_lab$care_hours<-ordered(as.factor(all_lab$care_hours), levels=c(1,2,3), 
                            labels=c("Low Level Caring", "High Level Caring","No caring"))
all_lab$care_loc_cv<-ordered(as.factor(all_lab$care_loc_cv), levels=c(1,2,3), 
                             labels=c("Within household only", "Outside of household only","Within and outside household"))

all_lab$carer_pre<-ordered(as.factor(all_lab$carer_pre), levels=c(1,2), 
                           labels=c("Yes", "No"))
all_lab$care_hours_pre<-ordered(as.factor(all_lab$care_hours_pre), levels=c(1,2,3), 
                                labels=c("Low Level Caring", "High Level Caring","No caring"))
all_lab$care_loc_pre<-ordered(as.factor(all_lab$care_loc_pre), levels=c(1,2,3), 
                              labels=c("Within household only", "Outside of household only","Within and outside household"))

all_lab$care_loc_change<-ordered(as.factor(all_lab$care_loc_change), levels=c(1,2), 
                                 labels=c("No", "Yes"))
all_lab$care_type_change<-ordered(as.factor(all_lab$care_type_change), levels=c(1,2), 
                                  labels=c("No", "Yes"))
all_lab$care_status<-ordered(as.factor(all_lab$care_status), levels=c(1,2,3), 
                             labels=c("Caring pre and during", "New carers during pandemic", "Stopped caring during pandemic"))


summary(all_lab[,c("carer","care_hours","care_loc_cv","carer_pre",
                   "care_loc_change", "care_type_change","care_hours_pre","care_loc_pre","care_status")])


saveRDS(all_lab, here::here('data', 'care_type', 'caring_pandemic_care_type.rds'))


# Descriptive ------------------------------------------------------------

##New/Stopped caring
t1<-all_lab %>% 
  select(carer_pre, care_hours) %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical",label= list(carer_pre~"If unpaid carer pre pandemic?")) %>% 
  add_p() %>% 
  bold_labels() %>% 
  as.tibble()%>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


##Care type and proximity
t2<-all_lab %>% 
  filter(carer=="Yes") %>% 
  select(care_hours_pre,care_loc_cv,care_hours,care_loc_change,care_loc_pre,care_type_change, care_status, care_hours_pre) %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical",label= list(care_loc_cv~"Proximity of caring during pandemic",
                                                                         care_hours_pre~"Type of unpaid carer pre pandemic",
                                                                         care_loc_change~"If care proximity changed during pandemic",
                                                                         care_loc_pre~"Proximity of caring pre pandemic",
                                                                         care_type_change~"If care type changed during the pandemic?")) %>% 
  add_p() %>% 
  add_overall() %>% 
  bold_labels() %>% 
  as_tibble() %>% 
  select(- "**No caring**, N = 0") %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))



all %>% 
  filter(carer=="Yes") %>% 
  select(care_hours_pre,care_hours) %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical") %>% 
  add_p() %>% 
  add_overall() %>% 
  bold_labels()

##Saving in one doc
wb = createWorkbook()

sheet = createSheet(wb, "Care Status")

addDataFrame(as.data.frame(t1), sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Care type and proximity")

addDataFrame(as.data.frame(t2), sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, here::here('outputs', 'CareVariables.xlsx'))




##Saving Graph
df<-all_dem %>% 
  select(carer_pre,care_hours_pre,care_loc_cv,care_loc_change,care_hours)



vars<-list() # create empty list to add to
for (j in seq_along(df)) {
  vars[[j]] <- as.name(colnames(df)[j])
}


g<-lapply(vars[1:length(vars)-1],d_graph,data=df)


##Saving graph
somePDFPath = here::here('outputs', 'Health.pdf')
pdf(file=somePDFPath)  


i=1
for (i in seq_along(g))   
{   
  plot(g[[i]])
} 
dev.off() 


