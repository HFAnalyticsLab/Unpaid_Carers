# 
# Project: Unpaid Carers during the pandemic
# Purpose: Preliminary Analysis
# Author: Anne Alarilla
# Date: 17/05/2021
# 

# Set up ------------------------------------------------------------------

##Load library 
library(tidyverse)
library(THFstyle)
library(ggplot2)
library(ggfittext)
library(scales)
library(janitor)
library(networkD3)
library(stringr)
library(survey)
library(ggtext)
library(plotly)
library(xlsx)
library(gtsummary)
library(sankey)


##Load Data 
all_h<- readRDS(here::here('data','care_type','health_all.rds'))
all_dem<- readRDS(here::here('data','care_type','demographics_all.rds'))
all<- readRDS(here::here('data','care_type','caring_pandemic_care_type.rds'))
all_s<- readRDS(here::here('data','care_type','services_all.rds'))

##Functions

clean_names<-function(df) { 
w.env <- new.env()
w.env$df <- df
t2_names<-sub("[[:punct:]]{2}","",colnames(w.env$df))
t3_names<-sub("[[:punct:]]{2}","",t2_names)
names(w.env$df)<-t3_names
assign(deparse(substitute(df)),w.env$df, envir=.GlobalEnv)
}

# Contents ----------------------------------------------------------------

#Set up for excel output
#Combining the data set 
#Survey design set up
#Sankey plot: Change in unpaid caring
#Women providing unpaid care and childcare
#Ethnicity and caring
#Caring within or outside household
#Caring outcomes 


# Set up for excel output -------------------------------------------------

wb = createWorkbook()

# Final data and design variable ------------------------------------------------------

##Adding variables 

##Demographics
all_dem <- all_dem %>% 
  mutate(resp_child=factor(case_when(child_u15 %in% c("One", "Two+")  ~ 1,
                                     child_u15== "None" ~ 2),levels=c(1,2), 
                           labels=c("1+ child under 15 responsible for","No child under 15 responsible for")),
         hh_child=factor(case_when(hh_child_u16 =="Atleast One" ~ 1,
                                   hh_child_u16 == "None" ~ 2),levels=c(1,2), 
                         labels=c("1+ child under 16 in the household","No child under 16 in the household")))

##Health and access to services 
all_h_s<- all_h %>% 
  left_join(all_s) %>% 
  mutate(mltc_short=case_when(mltc %in% c("None", "One")~ "None/One",
                              mltc=="2+"~ "2 or more")) 

#Combining the data 

final<-all %>% 
  select(pidp, carer, carer_pre,care_hours_pre,care_loc_cv,care_loc_change,care_hours, probit_lasso_wgt_t25, psu, strata) %>% 
  left_join(all_dem %>%  
    select(race_plus, sex_lab,age,hh_child_u16, hh_child,resp_child,child_u15,pidp)) %>% 
  left_join(all_h_s %>% 
    select(GHQ_cv,mltc,wait_for_NHS_treat, mltc_short, pidp)) 


# Survey design set up ----------------------------------------------------

uos_design<-svydesign(id= ~psu, strata= ~strata, survey.lonely.psu="adjust",
                      weights= ~probit_lasso_wgt_t25, data=final)

options(survey.lonely.psu="adjust")


#Sankey plot: Change in unpaid caring -------------------------------------------------------------

##Descriptives

uos_design %>% 
  tbl_svysummary(include = c(care_hours_pre,care_hours),
                 type=everything()~"categorical") %>% 
  bold_labels() 

uos_design %>% 
  tbl_svysummary(by="care_hours",include = c(care_hours,carer_pre),
                 type=everything()~"categorical") %>% 
  bold_labels() 


##Sankey plot 
t1<-svytable(~care_hours_pre+care_hours, design=uos_design) %>% 
  as.data.frame() %>% 
  mutate(care_hours_pre=factor(care_hours_pre, levels=c("High Level Caring", "Low Level Caring", "No caring"), 
                               labels=c("Providing 20+ hours of care pw","Providing <20 hours of care pw","Not providing care")),
          care_hours=factor(care_hours, levels=c("High Level Caring", "Low Level Caring", "No caring"), 
                             labels=c("Providing 20+ hours of care pw","Providing <20 hours of care pw","Not providing care")))

##For data labels 
lab_1<-svytable(~care_hours_pre, design=uos_design) %>% 
  as.data.frame() %>% 
  mutate(care_hours_pre=factor(care_hours_pre, levels=c("High Level Caring", "Low Level Caring", "No caring"), 
                               labels=c("Providing 20+ hours of care pw","Providing <20 hours of care pw","Not providing care")),
         sum=sum(Freq), prop=paste0(round(Freq/sum*100,1),"%"),
         In=case_when(care_hours_pre != "" ~  paste0(care_hours_pre, " (",prop,")",sep='_1'))) %>% 
  select(care_hours_pre, In)

lab_2<-svytable(~care_hours, design=uos_design) %>% 
  as.data.frame() %>% 
  mutate(care_hours=factor(care_hours, levels=c("High Level Caring", "Low Level Caring", "No caring"), 
                           labels=c("Providing 20+ hours of care pw","Providing <20 hours of care pw","Not providing care")),
         sum=sum(Freq), prop=paste0(round(Freq/sum*100,1),"%"),
         Out=case_when(care_hours != "" ~  paste0(care_hours, " (",prop,")",sep='_2'))) %>% 
  select(care_hours, Out)


sankey_plot_data<-t1 %>% 
  left_join(lab_1) %>% 
  left_join(lab_2) %>% 
  drop_na() %>% 
  select(In, Out, Freq) %>% 
  arrange(desc(In)) %>% 
  arrange(desc(Out))

# sankey_plot_data<-t1 %>% 
#   mutate(In=case_when(care_hours_pre != "" ~ paste0(care_hours_pre, sep='_1')),
#          Out=case_when(care_hours != "" ~ paste0(care_hours, sep='_2'))) %>% 
#   drop_na() %>% 
#   select(In, Out, Freq) %>% 
#   arrange(desc(In)) %>% 
#   arrange(desc(Out))

nodes<- sankey_plot_data %>% 
  select(In, Out) %>% 
  pivot_longer(c(In,Out), names_to="col_names", values_to= "name_match") %>% 
  select(-1) %>% 
  distinct() %>% 
  mutate(name=str_sub(name_match, end=-3)) 


nodes<-data.frame(nodes)

sankey_plot_id<- sankey_plot_data %>% 
  mutate(IDin= match(In, nodes$name_match)-1,
         IDout=match(Out, nodes$name_match)-1)

sankey_plot_id<-data.frame(sankey_plot_id)

# nodes<- nodes %>% 
#   mutate(ColourGroup=case_when(name== "Providing 20+ hours of care pw" ~ "#dd0031",
#                                name== "Providing <20 hours of care pw" ~ "#53a9cd",
#                                name=="Not providing care" ~ "#744284",
#                                ))

node_colour<-'d3.scaleOrdinal() .domain(["THF_red","THF_50pct_light_blue","THF_1_purple"])
              .range(["#dd0031","#53a9cd","#744284"])'


nodes<- nodes %>% 
  mutate(ColourGroup=case_when(name %in% c("Providing 20+ hours of care pw (4.5%)","Providing 20+ hours of care pw (9%)")  ~ "THF_red",
                               name %in% c("Providing <20 hours of care pw (12.8%)", "Providing <20 hours of care pw (22.8%)") ~ "THF_50pct_light_blue",
                               name %in% c("Not providing care (82.7%)", "Not providing care (68.2%)")~ "THF_1_purple"))

sankeyNetwork(Links = sankey_plot_id, Nodes = nodes, 
              Source = "IDin", Target = "IDout", 
              Value = "Freq", NodeID = "name", sinksRight = FALSE, fontSize = 16, colourScale = node_colour, 
              NodeGroup = "ColourGroup", iterations=0, units="respondents", nodeWidth=25, fontFamily="Arial", height=750, width=780)

# 
# fig <- plot_ly(
#   type = "sankey",
#   arrangement = "snap",
#   orientation = "h",
# 
#   node = list(
#     label = nodes$name,
#     color = nodes$ColourGroup,
#     pad = 15,
#     thickness = 20,
#     line = list(
#       color = "black",
#       width = 0.5
#     )
#   ),
# 
#   link = list(
#     source = sankey_plot_id$IDin,
#     target = sankey_plot_id$IDout,
#     value =  sankey_plot_id$Freq
#   )
# )
# fig <- fig %>% layout(
#   title = "",
#   font = list(
#     size = 12
#   )
# )
# 
# fig


#Saving data to excel sheet

sheet = createSheet(wb, "Care Status")

addDataFrame(as.data.frame(t1), sheet=sheet, startColumn=1, row.names=FALSE)


# Women providing unpaid care and childcare ---------------------------------------------------------

##Breakdown of Sex, Caring and Childcare

##Descriptive 

t2<-uos_design %>% 
  tbl_svysummary(by="sex_lab",include = c(carer, care_hours, sex_lab, hh_child, resp_child),
                 type=everything()~"categorical", 
                 label= list(carer~"If unpaid carer (Nov 2020/Jan 2021)?",
                             care_hours~"Type of carer?",
                             hh_child~"Number of children in household?",
                             resp_child~"Number of children responsible for?")) %>% 
  add_p() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))
      
clean_names(t2)

uos_design %>% 
  tbl_svysummary(include = c(carer, care_hours, sex_lab, hh_child, resp_child))

uos_design %>% 
  tbl_svysummary(by = "care_hours", include = c(care_hours, sex_lab, hh_child,resp_child),
                 label=list(sex_lab~"Gender",
                            hh_child~"Number of children in household?",
                            resp_child~"Number of children responsible for?"))%>% 
  add_p() %>% 
  bold_labels()


fem<-subset(uos_design, sex_lab=="Female") %>% 
tbl_svysummary(by="care_hours",include = c(care_hours, hh_child,resp_child),
                 type=everything()~"categorical", 
                 label= list(care_hours~"Type of carer?",
                             hh_child~"Number of children in household?",
                             resp_child~"Number of children responsible for?")) %>% 
  add_p() %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  # modify_spanning_header(c("stat_1", "stat_2", "stat_3")~"**Female**") %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))


male<-subset(uos_design, sex_lab=="Male") %>% 
  tbl_svysummary(by="care_hours",include = c(care_hours, hh_child,resp_child),
                 type=everything()~"categorical", 
                 label= list(care_hours~"Type of carer?",
                             hh_child~"Number of children in household?",
                             resp_child~"Number of children responsible for?")) %>% 
  add_p() %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  # modify_spanning_header(c("stat_1", "stat_2", "stat_3")~"**Female**") %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

t3<-cbind(fem,male)

clean_names(t3)

#Graph of sex and childcare by caring status 
# tab_s<-svytable(~sex_lab+ hh_child +care_hours, design=uos_design) %>% 
#   as.data.frame() %>% 
#   # group_by(care_hours) %>% 
#   # mutate(sum_care=sum(Freq), prop_care=Freq/sum_care,) %>% 
#   # ungroup() %>% 
#   group_by(care_hours,sex_lab) %>% 
#   mutate(sum_sex=sum(Freq),prop_sex=Freq/sum_sex) %>% 
#   ungroup()
# 
# tab_s %>% 
#   mutate(lab=case_when(care_hours=="Low Level Caring"~"Providing <20 hours of care",
#                        care_hours=="High Level Caring"~"Providing 20+ hours of care",
#                        care_hours=="No caring"~"Not providing care")) %>% 
#   ggplot(., aes(x = sex_lab, y = prop_sex*100)) +
#   # geom_col(aes(color = hh_child, fill = hh_child), position = position_dodge(0.8), width = 0.7)+
#   geom_bar(aes(color = hh_child, fill = hh_child), position ="stack", stat="identity")+
#   theme_THF()+
#   # scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20))+
#   facet_wrap(~lab)+
#   scale_fill_THF()+
#   scale_colour_THF()+
#   labs(x= "", y="", title="")+
#   theme(plot.title = element_text(size=14),
#         legend.text=element_text(size=14), legend.position="bottom", 
#         axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14),
#         strip.text=element_text(size=14))
# 
# #Saving graph
# ggsave(here::here('outputs','care.png'),dpi=300,
#        width = 10, height = 6.5) 

# tab_sx<-svytable(~sex_lab+ resp_child +care_hours, design=uos_design) %>% 
#   as.data.frame() %>% 
#   group_by(care_hours) %>%
#   mutate(sum_care=sum(Freq)) %>%
#   ungroup() %>%
#   group_by(care_hours,sex_lab) %>% 
#   mutate(sum_sex=sum(Freq),prop_sex=sum_sex/sum_care, prop_child=Freq/sum_sex) %>% 
#   ungroup() 
# 
# tab_sx %>% 
#   mutate(lab=case_when(care_hours=="Low Level Caring"~"Providing <20 hours of care",
#                        care_hours=="High Level Caring"~"Providing 20+ hours of care",
#                        care_hours=="No caring"~"Not providing care")) %>% 
#   ggplot(., aes(x = sex_lab, y = prop_sex))+
#   # geom_col(aes(color = resp_child, fill = resp_child), position = position_dodge(0.8), width = 0.7)+
#   geom_bar(aes(color = resp_child, fill = resp_child), position="stack", stat="identity")+
#   theme_THF()+
#   # scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20))+
#   facet_wrap(~lab)+
#   scale_fill_THF()+
#   scale_colour_THF()+
#   labs(x= "", y="", title="")+
#   theme(plot.title = element_text(size=14),
#         legend.text=element_text(size=14), legend.position="bottom", 
#         axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14),
#         strip.text=element_text(size=14))
# 

##Caring status and childcare responsibility by sex 

tab_sex_care<-svytable(~sex_lab+care_hours+resp_child, design=uos_design) %>% 
  as.data.frame() %>% 
  group_by(sex_lab,care_hours) %>% 
  mutate(sum_sex_care=sum(Freq)) %>% 
  ungroup() %>% 
  group_by(sex_lab) %>% 
  mutate(sum_sex=sum(Freq)) %>% 
  group_by(resp_child,sex_lab) %>% 
  mutate(sum_child_sex=sum(Freq)) %>% 
  ungroup() %>% 
  mutate(prop_care_sex=sum_sex_care/sum_sex, prop_child_sex_care=Freq/sum_sex) %>% 
  mutate(lab_prop=ifelse(prop_child_sex_care<0.01, "",percent(prop_child_sex_care,1)))

tab_sex_care


tab_sex_care %>% 
  mutate(lab=case_when(care_hours=="Low Level Caring"~"Providing <20 hours of care pw",
                       care_hours=="High Level Caring"~"Providing 20+ hours of care pw",
                       care_hours=="No caring"~"Not providing care")) %>% 
  ggplot(., aes(x = sex_lab, y = prop_child_sex_care*100, label=lab_prop))+
  # geom_col(aes(color = resp_child, fill = resp_child), position = position_dodge(0.8), width = 0.7)+
  geom_bar(aes(color = resp_child, fill = resp_child),position="stack", stat="identity")+
  geom_text(aes(fill=resp_child, label=lab_prop), colour="White",position = position_stack(vjust = 0.5))+
  theme_THF()+
  scale_y_continuous(limits = c(0,80), breaks = seq(0, 80, by = 10))+
  facet_wrap(~lab)+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "", y="% respondents", title="")+
  theme(plot.title = element_text(size=14),
        legend.text=element_text(size=14), legend.position="bottom", 
        axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14),
        strip.text=element_text(size=14))

##Saving graph
ggsave(here::here('outputs','care_childcare_by_sex.png'),dpi=300,
       width = 10, height = 6.5) 


##Sex and childcare responsibility by caring status 
tab_care<-svytable(~sex_lab+care_hours+resp_child, design=uos_design) %>% 
  as.data.frame() %>% 
  group_by(care_hours) %>% 
  mutate(sum_care=sum(Freq)) %>% 
  ungroup() %>% 
  mutate(prop_care=Freq/sum_care, lab_prop=ifelse(prop_care<0.01, "",percent(prop_care,1)))


tab_care %>% 
  mutate(lab=case_when(care_hours=="Low Level Caring"~"Providing <20 hours of care pw",
                       care_hours=="High Level Caring"~"Providing 20+ hours of care pw",
                       care_hours=="No caring"~"Not providing care")) %>% 
  ggplot(., aes(x = sex_lab, y = prop_care*100, label=lab_prop))+
  # geom_col(aes(color = resp_child, fill = resp_child), position = position_dodge(0.8), width = 0.7)+
  geom_bar(aes(color = resp_child, fill = resp_child),position="stack", stat="identity")+
  geom_text(aes(fill=resp_child, label=lab_prop), colour="White",position = position_stack(vjust = 0.5))+
  theme_THF()+
  scale_y_continuous(limits = c(0,80), breaks = seq(0, 80, by = 10))+
  facet_wrap(~lab)+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "", y="% respondents", title="")+
  theme(plot.title = element_text(size=14),
        legend.text=element_text(size=14), legend.position="bottom", 
        axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14),
       strip.text=element_text(size=14))

#Saving graph
ggsave(here::here('outputs','sex_childcare_by_care.png'),dpi=300,
       width = 10, height = 6.5) 



#Saving data to excel sheet
sheet = createSheet(wb, "Sex")

addDataFrame(as.data.frame(t2), sheet=sheet, startColumn=1, row.names=FALSE)

sheet = createSheet(wb, "Sex and Childcare")

addDataFrame(as.data.frame(t3), sheet=sheet, startColumn=1, row.names=FALSE)


# Ethnicity and caring -----------------------------------------------------------------

##Descriptive 
t4<-uos_design %>% 
  tbl_svysummary(by="race_plus", include = c(race_plus, care_hours),
                 type=everything()~"categorical") %>% 
  add_p() %>%
  bold_labels() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

clean_names(t4)

# tbl<-svytable(~race_plus+care_hours, design=uos_design)  
# c<-svychisq(~race_plus+care_hours, uos_design_xw, statistic="adjWald")
# summary(tbl, statistic="adjWald")

tab_s<-svytable(~race_plus+care_hours, design=uos_design) %>% 
  as.data.frame() %>% 
  group_by(race_plus) %>% 
  mutate(sum_care_hours=sum(Freq)) %>% 
  ungroup() %>% 
  mutate(prop_race=Freq/sum_care_hours, lab_prop=percent(prop_race,1))

##Graph

tab_s %>% 
  mutate(lab=case_when(care_hours=="Low Level Caring"~"Providing <20 hours of care pw",
                       care_hours=="High Level Caring"~"Providing 20+ hours of care pw",
                       care_hours=="No caring"~"Not providing care")) %>% 
  ggplot(., aes(x = race_plus, y = prop_race*100,label=lab_prop)) +
  # geom_col(aes(color = race, fill = race), position = position_dodge(0.8), width = 0.7)+
  geom_bar(aes(color = lab, fill = lab), position ="stack", stat="identity")+
  geom_text(aes(fill=lab, label=lab_prop), colour="White",position = position_stack(vjust = 0.5))+
  theme_THF()+
  # scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20))+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "", y="% of respondents", title="")+
  theme(plot.title = element_text(size=14),
        legend.text=element_text(size=14), legend.position="bottom", 
        axis.text.x=element_text(size=14, angle = 0, hjust=0.45),axis.text.y=element_text(size=14))

##Saving Graph
ggsave(here::here('outputs','ethnicity.png'),dpi=300,
       width = 10, height = 6.5) 

##Table for excel sheet

sheet = createSheet(wb, "Ethnicity and Care status")

addDataFrame(as.data.frame(t4), sheet=sheet, startColumn=1, row.names=FALSE)


# Caring within or outside household -----------------------------------------------------------------

##Removing non carers in the sample
w_all_2<-final %>% 
  filter(carer=="Yes") %>% 
  mutate(care_hours=factor(care_hours, levels=c("Low Level Caring", "High Level Caring")))

##Needs a new survey design
uos_design_xw<-svydesign(id= ~psu, strata= ~strata,
                         weights= ~probit_lasso_wgt_t25, data=w_all_2) 

options(survey.lonely.psu="adjust")

##Descriptives 
t5<- uos_design_xw %>% 
  tbl_svysummary(by="care_hours", include = c(care_loc_cv, care_hours),
                 type=everything()~"categorical",label= list(care_loc_cv~"Where they are caring?")) %>% 
  add_p() %>% 
  add_overall() %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

clean_names(t5)

tab_s<-svytable(~care_loc_cv+care_hours, design=uos_design_xw) %>% 
  as.data.frame() %>% 
  group_by(care_hours) %>% 
  mutate(sum_care=sum(Freq)) %>% 
  ungroup() %>% 
  mutate(prop_all=Freq/sum_care)

##Graph
tab_s %>% 
  mutate(lab=ifelse(care_hours=="Low Level Caring", "Providing <20 hours of care", "Providing 20+ hours of care")) %>% 
  ggplot(., aes(x = lab, y = prop_all*100)) +
  geom_bar(aes(color = care_loc_cv, fill = care_loc_cv), position ="stack", stat="identity")+
  theme_THF()+
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20))+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "", y="% of respondents", title="")+
  theme(legend.text=element_text(size=14), legend.position="bottom", legend.direction = "vertical", 
        axis.text.x=element_text(size=14, angle = 0, hjust=0.5),axis.text.y=element_text(size=14))

##Saving graph 
ggsave(here::here('outputs','care_proximity.png'),dpi=300,
       width = 10, height = 6.5) 

# 
# tbl<-svytable(~care_loc_cv+care_hours, design=uos_design_xw)
# c<-svychisq(~care_loc_cv+care_hours, uos_design_xw)
# 
# svychisq(~care_loc_cv+care_hours, uos_design_xw, statistic="adjWald")
# summary(tbl, statistic="adjWald")

##Saving excel sheet

sheet = createSheet(wb, "Care location and Care status")

addDataFrame(as.data.frame(t5), sheet=sheet, startColumn=1, row.names=FALSE)


# Caring outcomes  -----------------------------------------------------------------

#Descriptive
t6<-uos_design %>% 
  tbl_svysummary(by="care_hours", include = c(GHQ_cv, mltc_short, wait_for_NHS_treat, care_hours),
                 type=everything()~"categorical",label= list(GHQ_cv~"If experiencing Depressive Symptoms (DS) in Nov 2020/Jan 2021?",
                                                             mltc_short~"Number of long term health conditions in Nov 2020/Jan 2021?",
                                                             wait_for_NHS_treat~"Since 1st Jan 2020, have you been wating for NHS treatment?")) %>% 
  add_p() %>% 
  add_overall() %>% 
  bold_labels() %>% 
  as.tibble() %>% 
  mutate_if(is.character, ~replace(., is.na(.), ""))

clean_names(t6)

df<-svytable(~GHQ_cv+care_hours, design=uos_design) %>% 
  as.data.frame() %>% 
  rename(Metric=GHQ_cv) %>%
  group_by(care_hours) %>% 
  mutate(sum_care=sum(Freq)) %>% 
  ungroup() %>% 
  mutate(prop_all=Freq/sum_care) %>% 
  bind_rows(svytable(~mltc_short+care_hours, design=uos_design) %>% 
              as.data.frame() %>% 
              rename(Metric=mltc_short) %>%
              group_by(care_hours) %>% 
              mutate(sum_care=sum(Freq)) %>% 
              ungroup() %>% 
              mutate(prop_all=Freq/sum_care)) %>% 
  bind_rows(svytable(~wait_for_NHS_treat+care_hours, design=uos_design) %>% 
              as.data.frame() %>% 
              rename(Metric=wait_for_NHS_treat) %>%
              group_by(care_hours) %>% 
              mutate(sum_care=sum(Freq)) %>% 
              ungroup() %>% 
              mutate(prop_all=Freq/sum_care)) %>% 
  filter(Metric %in% c("DS","2 or more", "Yes")) %>% 
  mutate(Metric_lab= case_when(Metric=="DS"~ "Depressive symptoms Nov 2020/Jan 2021",
                               Metric=="2 or more"~ "Two or more long term health conditions Nov 2020/Jan 2021",
                               Metric=="Yes"~ "Since 1st Jan 2020, been wating for NHS treatment"),
         care_lab=factor(case_when(care_hours=="Low Level Caring"~ "Providing <20 hrs of care",
                                   care_hours=="High Level Caring"~ "Providing 20+ hrs of care",
                                   care_hours=="No caring"~ "Not providing care"),
                         levels=c("Not providing care","Providing <20 hrs of care","Providing 20+ hrs of care")),
         lab_prop=percent(prop_all,1))

#Graph
df %>% 
  ggplot(., aes(x = care_lab, y = prop_all*100, label=lab_prop)) +
  geom_col(aes(color = Metric_lab, fill = Metric_lab), position = position_dodge(0.8), width = 0.7)+
  geom_text(aes(fill=Metric_lab, label=lab_prop), colour="White", position = position_dodge(width=0.8), vjust=1.5)+
  # geom_text(position=position_dodge(width=0.8))+
  theme_THF()+
  scale_y_continuous(limits = c(0,100), breaks = seq(0, 100, by = 20))+
  scale_fill_THF()+
  scale_colour_THF()+
  labs(x= "", y="% of respondents", title="")+
  theme(legend.text=element_text(size=14), legend.position="bottom", legend.direction = "vertical",
        axis.text.x=element_text(size=14, angle = 0, hjust=0.5),axis.text.y=element_text(size=14))

##Saving graph 
ggsave(here::here('outputs','care_outcomes.png'),dpi=300,
       width = 10, height = 6.5) 


##Saving excel sheet

sheet = createSheet(wb, "Care status and outcomes")

addDataFrame(as.data.frame(t6), sheet=sheet, startColumn=1, row.names=FALSE)



# Closing excel sheet -----------------------------------------------------


saveWorkbook(wb, here::here('outputs', 'Unpaid_Carer.xlsx'))

