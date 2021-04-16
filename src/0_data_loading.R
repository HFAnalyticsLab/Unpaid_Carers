
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
library(readxl)
library(data.table)


##Reading in the variables I want
variables <- read_excel(here::here("Variables.xlsx"))
variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) 

t<-variables$Name

##Setting the working directory for data
setwd(here::here('data'))

#Create lists of the _indresp files-The main data set
listind <- list.files(here::here('data'), pattern = "*_indresp")

#Loading the data set
ind <- lapply(listind, read_sav)

##Extracting the data set with only the variables that we're interested in
ind<-lapply(ind, function(x) x%>% select(contains(t),-ends_with(c("st","finishtime","starttime","duration","end"))))

##Naming the data based on the waves
names(ind) <- gsub("\\.sav$", "", listind)

##saving each data set as separate files in the data environment
list2env(ind, .GlobalEnv)

##saving selected variables

saveRDS(j_indresp, here::here('data', 'care_type', 'wave10.rds'))

#Combining the telephone and web waves

post<-cf_indresp_w %>% 
  bind_rows(cf_indresp_t) %>% 
  full_join(cg_indresp_w, by= "pidp")

 
saveRDS(post, here::here('data', 'care_type', 'wave6n7_covid.rds'))

