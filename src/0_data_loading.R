
# =======================================================
# Project: Unpaid Carers with high care level during the pandemic
# Purpose: Derive the care type variables 
# Author: Anne Alarilla
# Date: 11/04/2021
# =======================================================

#Load libraries
library(haven)
library(tidyr)
library(tidyverse)
library(stringr)
library(here)
library(janitor)

##Setting the working directory for data
setwd(here::here('data'))

#Create lists of the _indresp files-The main data set
listind <- list.files(here::here('data'), pattern = "*_indresp")

#Loading the data set
ind <- lapply(listind, read_sav)

##Extracting the data set with only the caring variables that we're interested in
ind <- lapply(ind, function(x) x%>% select(contains("aid"),
                                            ends_with("caring"),
                                            ends_with("pidp"),
                                               contains("carehow")))
##Naming the data based on the waves
names(ind) <- gsub("\\.sav$", "", listind)

##saving each data set as separate files in the data environment
list2env(ind, .GlobalEnv)

##Selecting the care_type variables only 
pre<-j_indresp %>%
  select(pidp,j_aidxhh,j_aidhrs,j_aidhh)

saveRDS(pre, here::here('data', 'care_type', 'wave10.rds'))

#Combining the telephone and web waves
post<-cf_indresp_w %>% 
  bind_rows(cf_indresp_t) %>% 
  full_join(cg_indresp_w, by= "pidp") %>% 
  ##NAs= not taken part in both surveys, so taking the most recent data available
  mutate(caring=ifelse(is.na(cf_caring),-10,as.numeric(cf_caring)), 
         aidhh=ifelse(is.na(cg_aidhh), cf_aidhh, cg_aidhh), 
         aidhrs=ifelse(is.na(cg_aidhrs_cv),cf_aidhrs_cv,cg_aidhrs_cv))
    #-10 means didn't take part in wave 6 but took part in wave 7
 
saveRDS(post, here::here('data', 'care_type', 'wave6n7_covid.rds'))

