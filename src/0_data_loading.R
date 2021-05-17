
##
# Project: Unpaid Carers with high care level during the pandemic
# Purpose: Derive the care type variables 
# Author: Anne Alarilla
# Date: 11/04/2021
##

# Load library ------------------------------------------------------------

library(haven)
library(tidyr)
library(tidyverse)
library(stringr)
library(here)
library(janitor)
library(readxl)
library(labelled)


# Selected Variable list --------------------------------------------------
variables <- read_excel(here::here("Variables.xlsx"))
variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) 

t<-variables$Name


# Download Understanding society data set --------

#Create lists of the _indresp files in the main data set
listind <- list.files(here::here('data','stata'), pattern = "*_indresp")


#Set working directory 
setwd(here::here('data','stata'))

#Loading the data sets
ind <- lapply(listind, read_stata)

##Extracting the data sets with only the variables that we're interested in
ind<-lapply(ind, function(x) x%>% select(contains(t),-ends_with(c("st","finishtime",
                                                                  "starttime","duration","end",
                                                                  "outcome", "hidp","useragentstring")),j_hidp))

##Naming the data based on the waves
names(ind) <- gsub("\\.dta$", "", listind)


##saving each data set as separate files in the data environment
list2env(ind, .GlobalEnv)


##Loading household for income
j_hhresp <- read_dta(file=here::here('data','stata','j_hhresp.dta'))

income<-j_hhresp %>% 
  select(j_hidp,j_fihhmngrs_dv)

# Combining the waves  -------------------------------------------

#Combining the telephone and web waves
cg_indresp_w$wave<-"w7_cv"
cf_indresp_w$wave<-"w6_cv"
cf_indresp_t$wave<-"w6_cv"

wave10<- j_indresp %>% 
  left_join(income, by.x= "j_hidp", by.y="hidp")

cg_indresp_w <- cg_indresp_w %>% 
  rename_at(vars(starts_with("cg_")),
            ~str_replace(.,"cg_", "")) %>% 
  arrange(pidp,wave)

wave6<-cf_indresp_w %>% 
  bind_rows(cf_indresp_t) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), -6)) %>% 
  rename_at(vars(starts_with("cf_")),
            ~str_replace(.,"cf_", ""))

covid_long<-cg_indresp_w %>% 
  bind_rows(wave6) %>% 
  relocate(wave, .after=pidp) %>% 
  pivot_longer(!(c(pidp, wave)), names_to="var", values_to="val") %>% 
  pivot_wider(names_from ="wave", values_from ="val") %>% 
  mutate(covid=ifelse(is.na(w7_cv), w6_cv, w7_cv)) 
 

#combining pre and covid waves
all_wide<-covid_long %>%   
  #Replacing the NAs to -6, NA because did not take part in both surveys (just one) or did not take part in telephone only variables
  mutate(covid=ifelse(is.na(covid),-6,covid)) %>% 
  select(-c(w7_cv, w6_cv)) %>% 
  pivot_wider(names_from ="var", values_from ="covid") %>% 
  left_join(wave10, by=c("pidp")) %>% 
  ##Na if did not take part in wave 10  
  mutate_if(is.numeric, ~replace(., is.na(.), -10))
  


#Adding the new weightings

weights <- read_dta(file=here::here('data','stata','Anne_analysis_weight.dta'))

all_wide<-all_wide %>% 
  full_join(weights, by="pidp")


saveRDS(all_wide, here::here('data', 'care_type', 'caring_pandemic.rds'))
