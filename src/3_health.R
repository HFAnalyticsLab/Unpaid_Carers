#Load libraries
library(tidyverse)
library(haven)
library(gtsummary)
library(survey)
library(janitor)
library(readxl)
library(xlsx)

# Functions ---------------------------------------------------------------

`%notin%` <- Negate(`%in%`)

replace_with_na_all_2 <- function(df, formule) {
  df[rlang::as_function(formule)(df)] <- NA
  df
}

fix_no <- function(x){
  x <- as.character(x)
  x[x == "0"] <- "No"
  x
}

fix_yes <- function(x){
  x <- as.character(x)
  x[x == "1"] <- "Yes"
  x
}



fix_NA<-function(x){
  x <- as.character(x)
  x[x == "2"] <- NA
  x
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
    #coord_flip()+
    scale_x_discrete(labels = label_wrap(4))+
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
  filter(variable_type=="health")

h<-variables$Name


all_h<-all %>% 
  select(carer, carer_pre, care_hours,contains(h),betaindin_xw,betaindin_lw, psu, strata) %>% 
  rename(Asthma=ff_hcond1, 
         Arthritis=ff_hcond2, 
         Congestive_heart_failure=ff_hcond3,
         Coronary_heart_disease=ff_hcond4,
         Angina=ff_hcond5,
         Heart_attack=ff_hcond6,
         Stroke=ff_hcond7,
         Emphysema=ff_hcond8,
         Hypothyroidism=ff_hcond10,
         bronchitis=ff_hcond11,
         liver=ff_hcond12,
         Cancer_or_malignancy=ff_hcond13,
         Diabetes=ff_hcond14,
         Epilepsy=ff_hcond15,
         hypertension=ff_hcond16,
         Other=ff_hcond18,
         Multiple_Sclerosis=ff_hcond19,
         COPD=ff_hcond21,
         emotional_nervous_or_psychiatric=ff_hcond22,
         kidney=ff_hcond23,
         Cond_brain_nerves=ff_hcond24,
         Overweight=ff_hcond27
         ) 

all_h<-all_h %>% 
  mutate(GHQ_cv=ifelse(scghq2_dv>=6, "DS", "Non-DS"),
         GHQ_pre=ifelse(j_scghq2_dv>=6, "DS", "Non-DS"),
         GHQ_diff=as.numeric(scghq2_dv)-as.numeric(j_scghq2_dv),
         sum_cond_all=rowSums(all_h[ ,c(6:50)], na.rm=TRUE),
         mltc=factor(case_when(sum_cond_all==0~0,
                               sum_cond_all==1~1,
                               sum_cond_all>=2~2),
                     levels=c(0,1,2), labels=c("None", "One", "2+")))

saveRDS(all_h, here::here('data', 'care_type', 'health_all.rds'))


all_h %>% 
  select(GHQ_diff, care_hours) %>% 
  # mutate(GHQ_diff=as.factor(GHQ_diff)) %>%
  tbl_summary(by=care_hours, statistic = list(all_continuous() ~ "{mean} ({sd})")) %>% 
  add_p()

try <- all_h %>% 
  select(GHQ_diff, care_hours) %>%  
  anova_test(GHQ_diff ~ care_hours)
try

# Compute the analysis of variance
try2 <- aov(GHQ_diff ~ care_hours, data = all_h)
# Summary of the analysis
summary(try2)


##regression, with the care hours, GHQ pre and GHQ during as dependent variable 

top_conds<-all_h %>% 
  select(Asthma:hcondnew_cv96,care_hours) %>% 
  mutate(Asthma2=ifelse(Asthma==1|hcondnew_cv1==1,1,0),
         Arthritis2=ifelse(Arthritis==1|hcondnew_cv2==1,1,0), 
         Congestive_heart_failure2=ifelse(Congestive_heart_failure==1|hcondnew_cv2==1,1,0),
         Coronary_heart_disease2=ifelse(Coronary_heart_disease==1|hcondnew_cv4==1,1,0),
         Angina2=ifelse(Angina==1|hcondnew_cv5==1,1,0),
         Heart_attack2=ifelse(Heart_attack==1|hcondnew_cv6==1,1,0),
         Stroke2=ifelse(Stroke==1|hcondnew_cv7==1,1,0),
         Emphysema2=ifelse(Emphysema==1|hcondnew_cv8==1,1,0),
         Hypothyroidism2=ifelse(Hypothyroidism==1|hcondnew_cv10==1,1,0),
         bronchitis2=ifelse(bronchitis==1|hcondnew_cv11==1,1,0),
         liver2=ifelse(liver==1|hcondnew_cv12==1,1,0),
         Cancer_or_malignancy2=ifelse(Cancer_or_malignancy==1|hcondnew_cv13==1,1,0),
         Diabetes2=ifelse(Diabetes==1|hcondnew_cv14==1,1,0),
         Epilepsy2=ifelse(Epilepsy==1|hcondnew_cv15==1,1,0),
         hypertension2=ifelse(hypertension==1|hcondnew_cv16==1,1,0),
         Other2=ifelse(Other==1|hcondnew_cv18==1,1,0),
         Multiple_Sclerosis2=ifelse(Multiple_Sclerosis==1|hcondnew_cv19==1,1,0),
         COPD2=ifelse(COPD==1|hcondnew_cv21==1,1,0),
         emotional_nervous_or_psychiatric2=ifelse(emotional_nervous_or_psychiatric==1|hcondnew_cv22==1,1,0),
         kidney2=ifelse(kidney==1| hcondnew_cv23==1,1,0),
         Cond_brain_nerves2=ifelse(Cond_brain_nerves==1|hcondnew_cv24==1,1,0),
         Overweight2=ifelse(Overweight==1|hcondnew_cv27==1,1,0)) %>% 
  select(care_hours:Cond_brain_nerves2) %>% 
  pivot_longer(!care_hours,names_to="conds", values_to="val") %>% 
  group_by(care_hours, conds,val) %>% 
  summarise(N=n()) %>% 
  filter(val==1) %>% 
  ungroup() %>% 
  select(-val) %>% 
  pivot_wider(names_from ="care_hours", values_from ="N") 
##Save as top health conditions 


all_h[,6:50] <- lapply(all_h[,6:50], fix_yes)
all_h[,6:50] <- lapply(all_h[,6:50], fix_no)
all_h[,6:50] <- lapply(all_h[,6:50], fix_NA)


# Graphs ------------------------------------------------------------------

df<-all_h %>% 
  select(mltc, GHQ_cv, GHQ_pre, Overweight, care_hours) 
  

vars<-list() # create empty list to add to
for (j in seq_along(df)) {
  
  vars[[j]] <- as.name(colnames(df)[j])
}

g<-lapply(vars[1:length(vars)-1],d_graph,data=df)


tab<-df %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical", percent=("column")) %>% 
  bold_labels() %>% 
  add_p() %>% 
  as_tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))



##Saving in one doc

wb <-xlsx::loadWorkbook( here::here('outputs', 'CareVariables.xlsx'))

sheet = xlsx::createSheet(wb, "Health")

xlsx::addDataFrame(as.data.frame(tab), sheet=sheet, startColumn=1, row.names=FALSE)

sheet = xlsx::createSheet(wb, "Health Conditions")

xlsx::addDataFrame(as.data.frame(top_conds), sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, here::here('outputs', 'CareVariables.xlsx'))


##Saving graph
somePDFPath = here::here('outputs', 'Health.pdf')
pdf(file=somePDFPath)  


i=1
for (i in seq_along(g))   
{   
  plot(g[[i]])
} 
dev.off() 
