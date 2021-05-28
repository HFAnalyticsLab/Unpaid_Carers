# 
# Project: Unpaid Carers during the pandemic
# Purpose: Sensitivity Analysis
# Author: Anne Alarilla
# Date: 17/05/2021
# Only including those providing care within household during the pandemic (so yes for aidhh)



# Set up ------------------------------------------------------------------

library(gtsummary)
library(survey)
library(dplyr)

#Functions
replace_with_na_all_2 <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}

`%notin%` <- Negate(`%in%`)

# Load data ---------------------------------------------------------------

all<- readRDS(here::here('data','care_type','caring_pandemic.rds'))


# Data cleaning -----------------------------------------------------------

all$carer<-factor(all$aidhh, levels=c(1,2), labels=c("Yes", "No"))

all<-all %>% 
  mutate(care_hours= factor(case_when(aidhh==1 & aidhrs_cv %in% as.character(c(4:7,9)) ~ 2,
                        aidhh==1 & aidhrs_cv %notin% as.character(c(4:7,9))~ 1,
                        aidhh==2 ~ 3), 
         levels=c(2,1,3), labels=c("Providing 20+ hours of care", "Providing <20 hours of care", "Not providing care")),
         carer_pre=case_when((j_aidxhh==1|j_aidhh==1)~1,
                             j_aidxhh==2 & j_aidhh==2 ~2,
                             j_aidxhh==2 & j_aidhh<0 ~2,
                             j_aidxhh<0 & j_aidhh==2~2),
         care_hours_pre= factor(case_when(carer_pre==1 & j_aidhrs %in% as.character(c(4:7,9))~ 2,
                                   carer_pre==1 & j_aidhrs %notin% as.character(c(4:7,9))~ 1,
                                   carer_pre==2 ~ 3),
         levels=c(2,1,3), labels=c("Providing 20+ hours of care", "Providing <20 hours of care", "Not providing care")))

all$carer_pre<-factor(all$carer_pre, levels=c(1,2), labels=c("Yes", "No"))


# Survey design ------------------------------------------------------------



uos_design<-svydesign(id= ~psu, strata= ~strata, survey.lonely.psu="adjust",
                      weights= ~probit_lasso_wgt_t25, data=all)

options(survey.lonely.psu="adjust")



# Descriptive ------------------------------------------------------------


uos_design %>% 
  tbl_svysummary(by="carer" ,include = c(carer,carer_pre),
                 type=everything()~"categorical") %>% 
  bold_labels() 


