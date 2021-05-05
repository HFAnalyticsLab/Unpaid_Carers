# 
# Project: Unpaid Carers with high care level during the pandemic
# Purpose: Cleaning the caring variables and descriptive analysis
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

replace_with_na_all_2 <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}


d_graph <-  function(data,var.x=sex_lab){
  aaa <- ggplot2::enquo(var.x)
  #bbb <- ggplot2::enquo(var.y)
  plot <-  data %>%
    group_by(care_hours,!!aaa) %>%
    summarise(N=n()) %>% 
    filter(!is.na(!!aaa)) %>% 
    mutate(freq = N/sum(N)) %>% 
    drop_na() %>% 
    ggplot(., mapping=aes(x=!!aaa, y=freq, label=percent(freq)))+
    geom_col(aes(fill=!!aaa),show.legend=FALSE)+
    geom_bar_text()+
    coord_flip()+
    scale_x_discrete(labels = label_wrap(10))+
    facet_grid(cols=vars(care_hours))+
    theme(legend.position="none")+
    theme_bw()
  plot
  
}

# Loading data and Variable list ------------------------------------------------------------

all<- readRDS(here::here('data','care_type','caring_pandemic_care_type.rds'))

all<-all%>% 
  replace_with_na_all_2(df=.,formule = ~.x <0) 


variables <- read_excel(here::here("Variables.xlsx"))

variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) %>% 
  filter(variable_type=="demographics")
  
dem<-variables$Name

#Selecting Demographic variables

all_dem<-all %>% 
  select(carer, carer_pre, care_hours,contains(dem),betaindin_xw,betaindin_lw, psu, strata) %>% 
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
                                   racel_dv %in% c(3,4)~2, #Any other white
                                   racel_dv %in% c(5,8)~3, #mixed
                                   racel_dv== 9~4,
                                   racel_dv==10~5,
                                   racel_dv==11~6,
                                   racel_dv==12~7,
                                   racel_dv==13~8,
                                   racel_dv==14~9,
                                   racel_dv==15~10, 
                                   racel_dv==16~11,
                                   racel_dv %in% c(17,97)~ 12),
                                   levels=c(1,2,4,5,6,7,8,9,10,11,3,12), 
                    labels=c("White","Any other White", "Indian", "Pakistani","Bangladeshi","Chinese", "Any other Asian or Asian British",
                             "Caribbean", "African", "Any other Black or Black British", "Mixed", "Any other ethnic group")),
        urban_label=factor(j_urban_dv, levels=c(1,2), labels=c("Urban area", "Rural Area")))

saveRDS(all_dem, here::here('data', 'care_type', 'demographics_all.rds'))

  
df<-all_dem %>% 
  select(sex_lab, race, gor_dv2, age_band, employment_pre, nssec_pre, 
         marital_status, child_u15,howlong_lab,keyworkerstatus_lab,keyworksec_lab, race_plus, care_hours)

saveRDS(df, here::here('data', 'care_type', 'demographics.rds'))


vars<-list() # create empty list to add to
for (j in seq_along(df)) {
    vars[[j]] <- as.name(colnames(df)[j])
 }


g<-lapply(vars[1:length(vars)-1],d_graph,data=df)

cat<-df%>% 
  tbl_summary(by=care_hours, type=everything()~"categorical", percent=("column"),
              label= list(child_u15~"How many children under 15 are you responsible for?",
                          howlong_lab~"How long do you spend on housework per week?")) %>% 
  bold_labels() %>% 
  add_p() %>% 
  as_tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))
 


df%>% 
  filter(sex_lab=="Female") %>% 
  select(care_hours, age_band, child_u15) %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical", percent=("column")) %>% 
  bold_labels() %>% 
  add_p()

df%>% 
  filter(sex_lab=="Male") %>% 
  select(care_hours, age_band, child_u15) %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical", percent=("column")) %>% 
  bold_labels() %>% 
  add_p()

i<-all_dem %>% 
   mutate(Monthly_Income=as.numeric(j_fihhmngrs_dv)) %>% 
  select(Monthly_Income, care_hours) %>%
   tbl_summary(by=care_hours) %>% 
 bold_labels() %>% 
   add_p() %>% 
   as_tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

all_tab<-rbind(cat,i)



##Saving in one doc

wb <-loadWorkbook( here::here('outputs', 'CareVariables.xlsx'))

sheet = createSheet(wb, "Demographics")

addDataFrame(as.data.frame(all_tab), sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, here::here('outputs', 'CareVariables.xlsx'))



##Saving graph
somePDFPath = here::here('outputs', 'Demographics.pdf')
pdf(file=somePDFPath)  


i=1
for (i in seq_along(g))   
{   
  plot(g[[i]])
} 
dev.off() 
