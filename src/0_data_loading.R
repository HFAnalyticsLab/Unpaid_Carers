##Loading the Understanding Society data

library(haven)
library(tidyr)
library(tidyverse)
library(stringr)
library(here)
library(janitor)
library(naniar)

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
  select(pidp,j_aidxhh,j_aidhrs,j_aidhh) %>%   
  naniar::replace_with_na_all(condition = ~.x <0)

saveRDS(pre, here::here('data', 'care_type', 'wave10.rds'))

#Combining the telephone and web waves
post<-cf_indresp_w %>% 
  bind_rows(cf_indresp_t) %>% 
  naniar::replace_with_na_all(condition = ~.x <0) 


saveRDS(post, here::here('data', 'care_type', 'wave6_covid.rds'))

