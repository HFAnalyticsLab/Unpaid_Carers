#Load libraries
library(tidyverse)
library(haven)
library(gtsummary)
library(survey)
library(janitor)
library(readxl)
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
    mutate(freq = N/sum(N)) %>% 
    drop_na() %>% 
    ggplot(., mapping=aes(x=!!aaa, y=freq))+
    geom_col(aes(fill=!!aaa),show.legend=FALSE)+
    # coord_flip()+
    scale_x_discrete(labels = label_wrap(4))+
    facet_grid(cols=vars(care_hours))+
    theme(legend.position="none")+
    theme_bw()
  plot
}

# Load data ---------------------------------------------------------------

all<- readRDS(here::here('data','care_type','caring_pandemic_care_type.rds'))

all<-all%>% 
  replace_with_na_all_2(df=.,formule = ~.x <0) 


variables <- read_excel(here::here("Variables.xlsx"))

variables <- variables %>% 
  remove_empty( c("rows", "cols")) %>% 
  mutate(Name=gsub("XXX","",Name)) %>% 
  mutate(Name=gsub("cf_","",Name)) %>% 
  filter(variable_type=="coronavirus")

c<-variables$Name

all_c<-all %>% 
  select(carer, carer_pre, care_hours,contains(c)) %>% 
  mutate(clinvuln_lab=factor(clinvuln_dv, levels=c(0,1,2), 
                             labels=c("No risk", "Clinically Vulnerable/Moderate Risk", "Clinically extremely vulnerable/High risk")),
         CV_symp=factor(factor(case_when(ff_hadsymp==1 | hadsymp==1| hassymp==1~1,
                                         ff_hadsymp==0 | hadsymp==2| hassymp==2~2),levels=c(1,2), labels=c("Yes", "No"))),
         longcovid_lab=factor(longcovid, levels=c(1,2), labels=c("Yes", "No")),
         testresult_lab= factor(testresult, levels=c(1,2,3,4), labels=c("Positive", "Negative", "Inconclusive", "Waiting for results")),
         fin_help=factor(ifelse(transfin>0,1,0), levels=c(1,0), labels=c("Yes", "No")),
         cvinvite_lab=factor(cvinvite, levels=c(1,2), labels=c("Yes", "No")),
         hadvac_lab=factor(case_when(hadcvvac %in% c(1,2) ~ 1,
                                     hadcvvac== 3 ~ 2,
                                     hadcvvac== 4 ~ 3), levels=c(1,2,3), labels=c("Atleast one dose", "Have an appointment", "No")),
         wah_lab= factor(case_when(wah %in% c(1,2,3)~1,
                                   wah==4~2), levels=c(1,2), labels=c("Yes","No")),
         prod_lab=factor(case_when(prodch %in% c(1,2) ~1,
                                   prodch==3 ~  2,
                                   prodch %in% c(4,5) ~3), levels=c(1,2,3),labels = c("More done", "The same", "Less done")))

df <- all_c %>% 
  select(clinvuln_lab,CV_symp,longcovid_lab, testresult_lab,cvinvite_lab,hadvac_lab,wah_lab,prod_lab,fin_help,care_hours)


tab<-df %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical", percent=("column"), 
              label=list(clinvuln_lab~"At risk of serious illness from COVID-19",
                         CV_symp~"Ever had or has COVID-19 symptoms",
                         longcovid_lab~"If still experiencing COVID-19 symptoms/ not returned to previous health?",
                         testresult_lab~"Result of most recent COVID-19 test?",
                         cvinvite_lab~"If invited for COVID-19 vaccine?",
                         hadvac_lab~"If had the vaccine?",
                         wah_lab~"If currently working at home?",
                         prod_lab~"Has productivity changed since the start of the pandemic?",
                         fin_help~"If received financial help from family or friends since start of the pandemic?")) %>% 
  bold_labels() %>% 
  add_p() %>% 
   as_tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


##Saving in one doc

wb <-xlsx::loadWorkbook( here::here('outputs', 'CareVariables.xlsx'))

sheet = xlsx::createSheet(wb, "Covid")

xlsx::addDataFrame(as.data.frame(tab), sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, here::here('outputs', 'CareVariables.xlsx'))


vars<-list() # create empty list to add to
for (j in seq_along(df)) {
  
  vars[[j]] <- as.name(colnames(df)[j])
}

g<-lapply(vars[1:length(vars)-1],d_graph,data=df)


somePDFPath = here::here('outputs', 'Covid.pdf')
pdf(file=somePDFPath)  


i=1
for (i in seq_along(g))   
{   
  plot(g[[i]])
} 
dev.off() 


