
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


# Selected Variable list --------------------------------------------------
variables <- read_excel(here::here("Variables.xlsx"))
variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) 

t<-variables$Name


# Download Understanding society data set ---------

#Create lists of the _indresp files-The main data set
listind <- list.files(here::here('data'), pattern = "*_indresp")

#Loading the data sets
ind <- lapply(listind, read_sav)

##Extracting the data sets with only the variables that we're interested in
ind<-lapply(ind, function(x) x%>% select(contains(t),-ends_with(c("st","finishtime",
                                                                  "starttime","duration","end",
                                                                  "outcome", "hidp","useragentstring"))))

##Naming the data based on the waves
names(ind) <- gsub("\\.sav$", "", listind)

##saving each data set as separate files in the data environment
list2env(ind, .GlobalEnv)


# Saving data -------------------------------------------

# Pre Pandemic ------------------------------------------------------------


saveRDS(j_indresp, here::here('data', 'care_type', 'wave10.rds'))


# COVID-19 Data -----------------------------------------------------------


#Combining the telephone and web waves
#Removing data labels
val_labels(cg_indresp_w) <- NULL
var_label(cg_indresp_w) <- NULL

#Turning into long format
cg_long<-cg_indresp_w %>% 
  select_if(~!is.character(.)) %>%  
  pivot_longer(!c(pidp,psu, strata,birthy, racel_dv), names_to="var", values_to="cg") %>% 
  separate(var, into=c("wave","var"), sep="_", extra="merge") 

cf_long<-cf_indresp_w %>% 
  bind_rows(cf_indresp_t) %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), -6)) %>% 
  select_if(~!is.character(.)) %>% 
  pivot_longer(!c(pidp,psu, strata,birthy, racel_dv), names_to="var", values_to="cf") %>% 
  separate(var, into=c("wave","var"), sep="_", extra="merge") 

#Joining wave 6 and 7, taking data from wave 7 if available, if not then using wave 6 
during<-cg_long %>%
  select(-wave) %>% 
  full_join(cf_long %>% 
            select(-wave), by=c("pidp", "psu", "strata", "birthy", "racel_dv", "var")) %>% 
  mutate(value=ifelse(is.na(cg),cf,cg)) %>% 
  select(-c(cf, cg)) %>% 
  pivot_wider(names_from ="var", values_from ="value") %>% 
  #Replacing the NAs to -6, NA because did not take part in both surveys (just one) or did not take part in telephone only variables
  mutate_if(is.numeric, ~replace(., is.na(.), -6))

saveRDS(during, here::here('data', 'care_type', 'wave6n7_covid.rds'))