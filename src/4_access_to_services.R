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
    filter(!is.na(!!aaa)) %>% 
    mutate(freq = N/sum(N)) %>% 
    drop_na() %>% 
    ggplot(., mapping=aes(x=!!aaa, y=freq, label=percent(freq)))+
    geom_col(aes(fill=!!aaa),show.legend=FALSE)+
    geom_bar_text()+
    coord_flip()+
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
  filter(variable_type=="services")

s<-variables$Name

all_s<-all %>% 
  select(carer, carer_pre, care_hours,contains(s),betaindin_xw,betaindin_lw, psu, strata) %>% 
  rename(treat_OT=hcond_treat1, 
         med_immune=hcond_treat2, 
         chemo_cancer=hcond_treat3,
         radio_cancer=hcond_treat4,
         Other_treat_med=hcond_treat5,
         None_of_above=hcond_treat6,
         NHS_treat_con_plan_prog=treatment1,
         NHS_Op_proc_plan=treatment2,
         NHS_chemo_radio_plan=treatment3,
         NHS_other_treat_plan=treatment4,
         NHS_no_treat_plan=treatment5)


all_s<- all_s %>% ##Check that these are for those who needs it##
  mutate(sum_treat_immune=rowSums(all_s[ ,c(4:8)], na.rm=TRUE),
         treat_immune=ifelse(sum_treat_immune>0,"Yes","No"),
         sum_NHS_treat = rowSums(all_s[ ,c(10:13)], na.rm=TRUE), 
         wait_for_NHS_treat= ifelse(sum_NHS_treat>0, "Yes", "No"),
         NHS_treat_con_plan_prog = factor(NHS_treat_con_plan_prog, levels=c(1,0), labels=c("Yes","No")),
         NHS_Op_proc_plan= factor(NHS_Op_proc_plan, levels=c(1,0), labels=c("Yes","No")),                                
         NHS_chemo_radio_plan= factor(NHS_chemo_radio_plan, levels=c(1,0), labels=c("Yes","No")),                         
         NHS_other_treat_plan= factor(NHS_other_treat_plan, levels=c(1,0), labels=c("Yes","No")),                         
                               #   ==1~1,
                               #           NHS_Op_proc_plan==1~2,
                               #           NHS_chemo_radio_plan==1~3,
                               #           NHS_other_treat_plan==1~4),
                               # levels=c(1,2,3,4), labels=c("Tests/consultations planned or in progress",
                               #                             "Operation / procedure planned",
                               #                             "Targeted, chemotherapy or radiotherapy planned or in progress",
                               #                             "Other treatment planned")),
         NHS_reason_canceltreat=factor(canceltreat, levels=c(1,2,3,4),
                            labels=c("Yes, cancelled/postponed by NHS", "Yes, alternative provided", "Yes, I cancelled or postponed", "No, treatment continued as planned")),
         nhs_access=factor(case_when(nhsnowgp2 %in% c(1,2)~ 1,
                              nhsnowgp2 %in% c(3,4)~2,
                              nhsnow1112 ==1 ~ 1,
                              nhsnow1112 %in% c(2,3)~2,
                              nhsnowip2 %in% c(1,4)~1,
                              nhsnowip2 %in% c(2,3)~2,
                              nhsnowop2 %in% c(1,2,5)~1,
                              nhsnowop2 %in% c(3,4)~2),levels=c(1,2), labels=c("Yes","No")),
         nhs_presciption_acess=factor(nhsnowpm2, levels=c(1,2,3), labels=c("Yes", "No", "Not Required")),
         nhs_access_own_cancel=factor(case_when(nhsnowgp2==4~1,
                                                nhsnowgp2==3~2,
                                                nhsnow1112==3~1,
                                                nhsnow1112==2~2,
                                                nhsnowip2==3~1,
                                                nhsnowip2==4~3,
                                                nhsnowop2==4~1,
                                                nhsnowop2==3~2,
                                                nhsnowop2==5~3),levels=c(1,2,3), 
         labels=c("I postponed, cancelled or decided not to seek help this time","I was not able to access", "Different treatment provided")),
         respite=factor(ifelse(respitenow>0,1,2), levels=c(1,2), labels=c("Yes","No")),
         respite_hours=factor(case_when(respitenow<=17 ~ 1,
                                        respitenow>17 & respitenow<35~2,
                                        respitenow>=35~3),levels=c(1,2,3), 
                              labels=c("Less than or equal to 17 hours per week",
                                       "More than 17 and Less than 35 hours per week", 
                                       "More than 35 hours per week")),
         chsc_access_fc_psy=factor(case_when(chscnowcarer2 %in% c(1,2,3)~1,
                                      chscnowcarer2==4~2,
                                      chscnowpsy2 %in% c(1,2,3)~1,
                                      chscnowpsy2==4~2), levels=c(1,2),
                            labels=c("Yes","No")),
         nowcarer_lab=factor(case_when(chscnowcarer2 %in% c(1:3)~1,
                                       chscnowcarer2==4~2),levels=c(1,2),
                              labels=c("Yes", "No")),
         psy_lab=factor(case_when(chscnowpsy2 %in% c(1:3)~1,
                                  chscnowpsy2==4 ~2),levels=c(1,2), labels=c("Yes","No")),
         nowcarer_pre_lab=factor(j_hlsv2, levels=c(1,0), labels=c("Yes","No")),
         psy_pre_lab=factor(j_hlsv7, levels=c(1,0), labels=c("Yes","No")),
         nhs_access_pre=factor(case_when(j_hl2gp<0 ~ 1,
                                         j_hl2gp==0 ~2,
                                         j_hl2hop<0 ~ 1,
                                         j_hl2hop==0 ~2,
                                         j_hosp==1 ~ 1,
                                         j_hosp==2 ~ 1),levels=c(1,2), labels=c("Yes","No")))
 
saveRDS(all_s, here::here('data', 'care_type', 'services_all.rds'))        


df <- all_s %>% 
  select(treat_immune,wait_for_NHS_treat,NHS_reason_canceltreat,nhs_access, NHS_treat_con_plan_prog,NHS_Op_proc_plan,
         NHS_chemo_radio_plan,NHS_other_treat_plan,
         nhs_presciption_acess,nhs_access_own_cancel,respite,
         respite_hours,chsc_access_fc_psy,nowcarer_lab,psy_lab,
         nowcarer_pre_lab,psy_pre_lab,nhs_access_pre,care_hours)



tab<-df %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical", percent=("column"), 
              label=list(wait_for_NHS_treat~"Since 1st Jan 2020, have you been waiting for NHS treatment?",
                         NHS_reason_canceltreat~"If waiting for NHS treatment, has treatment been changed in any way?",
                         nhs_access~"Were you able to access services such as the gp, 111, inpatient and/or outpatients?",
                         respite~"Can you access respite care now?",
                         nhs_access_own_cancel~"Reason for not being able to access NHS services?",
                         chsc_access_fc_psy~"Were you able to access community and social care services such as formal carers and or psychotherapist in the last 4 weeks?",
                         nowcarer_lab~"In the last 4 weeks were supported by a formal carer?",
                         psy_lab~"In the last 4 weeks were you able to access counselling or talking therapy?")) %>% 
  bold_labels() %>% 
  add_p() %>% 
  as_tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


tab2<-df %>% 
  select(care_hours, respite) %>% 
  tbl_summary(by=care_hours, type=everything()~"categorical", percent=("column"), 
              label=list(respite~"Can you access respite care now?")) %>% 
  bold_labels() %>% 
  add_p()

##Tying to see which services they couldn't access
# type_services<-df %>% 
#   select(NHS_treat_con_plan_prog, care_hours) %>% 
#   group_by(care_hours,NHS_treat_con_plan_prog) %>% 
#   summarise(N=n()) %>% 
#   filter(NHS_treat_con_plan_prog=="Yes") %>% 
#   rename(NHS_treat_con_plan_prog_N=N) %>% 
#   select(-NHS_treat_con_plan_prog) %>% 
#   left_join(df %>% 
#               select(NHS_Op_proc_plan, care_hours) %>% 
#               group_by(care_hours,NHS_Op_proc_plan) %>% 
#               summarise(N=n()) %>% 
#               filter(NHS_Op_proc_plan=="Yes") %>% 
#               rename(NHS_Op_proc_plan_N=N) %>% 
#               select(-NHS_Op_proc_plan), by="care_hours") %>% 
#   left_join(df %>% 
#               select(NHS_chemo_radio_plan, care_hours) %>% 
#               group_by(care_hours,NHS_chemo_radio_plan) %>% 
#               summarise(N=n()) %>% 
#               filter(NHS_chemo_radio_plan=="Yes") %>% 
#               rename(NHS_chemo_radio_plan_N=N) %>% 
#               select(-NHS_chemo_radio_plan), by="care_hours") %>% 
#   left_join(df %>% 
#               select(NHS_other_treat_plan, care_hours) %>% 
#               group_by(care_hours,NHS_other_treat_plan) %>% 
#               summarise(N=n()) %>% 
#               filter(NHS_other_treat_plan=="Yes") %>% 
#               rename(NHS_other_treat_plan_N=N) %>% 
#               select(-NHS_other_treat_plan), by="care_hours") %>% 
#   pivot_longer(!(care_hours),names_to='type_treatment', values_to='val')




##Saving in one doc

wb <-xlsx::loadWorkbook( here::here('outputs', 'CareVariables.xlsx'))

sheet = xlsx::createSheet(wb, "Access to services")

xlsx::addDataFrame(as.data.frame(tab), sheet=sheet, startColumn=1, row.names=FALSE)

saveWorkbook(wb, here::here('outputs', 'CareVariables.xlsx'))



vars<-list() # create empty list to add to
for (j in seq_along(df)) {
  
  vars[[j]] <- as.name(colnames(df)[j])
}

g<-lapply(vars[1:length(vars)-1],d_graph,data=df)


somePDFPath = here::here('outputs', 'Access_to_services.pdf')
pdf(file=somePDFPath)  


i=1
for (i in seq_along(g))   
{   
  plot(g[[i]])
} 
dev.off() 




