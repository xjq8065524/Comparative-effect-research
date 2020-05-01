#### Set up library ----------------------------------------------------------
install.packages("dplyr")
install.packages("zoo")
install.packages("lubridate")#deal with dates and times
install.packages("naniar")#deal with missing data: https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
install.packages("ggplot2")
install.packages("remotes")
install.packages("survminer")
install.packages("cowplot")
install.packages("mefa4") # %notin% function
install.packages("pryr")
install.packages("gmodels")
install.packages("xlsx")
install.packages("labeling")
install.packages("gridExtra ")
install.packages("readr")
install.packages("stringr")
install.packages("gridExtra")
install.packages("naniar")
install.packages("visdat") 
install.packages("finalfit")
install.packages("rms")
install.packages("installr") #Updating R/RStudio: https://uvastatlab.github.io/phdplus/installR.html#updateR

# memory.limit()
# memory.limit(62468)
library(dplyr)
# library(zoo)
# library(lubridate)
library(readxl)
library(ggplot2)
library(remotes)
library(survival)
library(survminer)
# library(cowplot)
# library(scales)
library(ggsci)
# library(mefa4)
# library(pryr)
# library(gmodels)
library(extrafont) #font
# library(labeling)
# library(readr)
library(tidyr)
# library(stringr)
# library(xlsx)
# library(naniar)
# library(visdat)
# library(finalfit)
# library(gridExtra)  #masking  'package:dplyr'  combine
library(rms)

getwd()
setwd("D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research")
# load raw and label datasets -------------------------------------------------------
load("D:/DPhil/Project_Opioid_use/Data/billing.RData")
load("D:/DPhil/Project_Opioid_use/Data/diagnosis.RData")
load("D:/DPhil/Project_Opioid_use/Data/demography.RData")
load("D:/DPhil/Project_Opioid_use/Data/social_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/clinical_variables.RData")

load("R_datasets/tramadol_codeine01.RData")
load("R_datasets/tramadol_diclofenac01.RData")
load("R_datasets/tramadol_ibuprofen01.RData")
load("R_datasets/tramadol_celecoxib01.RData")
load("R_datasets/tramadol_Paracetamol01.RData")

Denominator_data <- read_delim("D:/DPhil/Data_raw/OPIODU/OPIOIDES_entregable_poblacio_denominadors_20191219_143606.txt", 
                               delim = "|",
                               col_names = TRUE)

#dictionary datasets
Dic_analgesics <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_analgesics.xlsx")
Dic_CER_adverse_events <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_CER_adverse_events.xlsx")
Dic_history_medication <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_history_medication.xlsx")
Dic_commorbidity <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_commorbidity.xlsx")

# Little explore with raw dta ---------------------------------------------
head(billing)
str(billing)
glimpse(billing)
glimpse(diagnosis)
# check missing of each variable (no missing)
sapply(diagnosis, function(x)sum(is.na(x)))
sapply(social_variables, function(x)sum(is.na(x)))
sapply(clinical_variables, function(x)sum(is.na(x)))

#make sure there is no duplicate 
check_dup_billing <- distinct(billing, idp, billing_cod, bill_date, .keep_all = TRUE) #(confirm no duplicate row)
check_dup_diagnosis <- distinct(diagnosis, idp, dia_cod, dia_start_date, .keep_all = TRUE) #(confirm with duplicate row)
check_dup_social_variables <- distinct(social_variables, idp, economic_level, rural, .keep_all = TRUE) #(confirm no duplicate row)
check_dup_clinical_variables <- distinct(clinical_variables,idp,clinical_cod, clinical_date, val, .keep_all = TRUE) #(confirm no duplicate row)

#data preparation
BMI_dataset <- 
  clinical_variables %>% 
  filter(clinical_agr == "IMC") %>% 
  select(-clinical_cod)


# stage_one:select opioid users (mainly based on billing dataset) -------------------------------------------------------
set.seed(1)
sub_Denominator <- sample_frac(Denominator_data, 0.1)

#select all users , generate some auxiliary variables
generate_func <- function(comparator){
  pattern_cod_tra_stage1 <- 
    Denominator_data %>% 
    select(idp) %>% 
    left_join(billing, by = "idp") %>% 
    left_join(select(Dic_analgesics,ATC_code, Specific_drug), by = c("billing_cod" = "ATC_code")) %>% 
    filter(!is.na(Specific_drug), Specific_drug %in% c("tramadol", comparator)) %>% 
    group_by(idp) %>% 
    arrange(bill_date) %>% 
    mutate(seq = row_number(), total_seq = n()) %>% # create two index
    ungroup() 
  
  #generate index varible for users with both codeine and tramadol dispensed at the first initiation
  pattern_cod_tra_stage2 <- 
    pattern_cod_tra_stage1 %>% 
    group_by(idp) %>% # quite important function in R: any()
    arrange(bill_date) %>% 
    #generate one variable to indicate first dispensation date, and drug name
    mutate(first_time = bill_date[1], first_drug = Specific_drug[1]) %>%  # make sure the first position means ealiest date
    #generate one variable to indicate whether double initiators. 0: no, 1:yes
    mutate(index_double_user = case_when(any(seq >= 2 & first_time == bill_date & Specific_drug != first_drug) ~ 1,
                                         TRUE ~ 0)) %>% 
    ungroup()
  
  pattern_cod_tra_stage2 %>% 
    filter(index_double_user == 1) %>% 
    summarise(n = n_distinct(idp))
  
  #generate switchers and no_switchers index
  pattern_cod_tra_stage3 <- 
    pattern_cod_tra_stage2 %>% 
    filter(index_double_user == 0) %>% #only for non double users
    mutate(index = case_when(Specific_drug == "tramadol" ~ 1,
                             Specific_drug == comparator~ -1)) %>% 
    group_by(idp) %>% 
    mutate(switchs = case_when((total_seq == sum(index) | total_seq == -sum(index)) ~ "No_switchers",
                               TRUE ~ "Switchers")) %>% 
    ungroup()
  
  
  #generate index variable for calculating number of dispensation before switch
  pattern_cod_tra_stage4 <- 
    pattern_cod_tra_stage3 %>% 
    group_by(idp) %>% 
    # there are warning message, try to fix it later
    mutate(total_before_switch = case_when(switchs == "No_switchers" ~ as.numeric(total_seq),
                                           switchs == "Switchers" ~ as.numeric(min((which(first_drug != Specific_drug)))))) %>% 
    ungroup()
  
  return(pattern_cod_tra_stage4)
}


tramadol_codeine <- generate_func(comparator = "codeine")
tramadol_ibuprofen <- generate_func(comparator = "ibuprofen")
tramadol_diclofenac <- generate_func(comparator = "diclofenac")
tramadol_celecoxib <- generate_func(comparator = "celecoxib")
tramadol_Paracetamol <- generate_func(comparator = "Paracetamol")


save(tramadol_codeine, file="R_datasets/tramadol_codeine.RData")
save(tramadol_ibuprofen, file="R_datasets/tramadol_ibuprofen.RData")
save(tramadol_diclofenac, file="R_datasets/tramadol_diclofenac.RData")
save(tramadol_celecoxib, file="R_datasets/tramadol_celecoxib.RData")
save(tramadol_Paracetamol, file="R_datasets/tramadol_Paracetamol.RData")


tramadol_NSAID <- generate_func(comparator = "Oral NSAID")
tramadol_non_NSAID <- generate_func(comparator = "Non anti-inflammatory drugs")
# 
# save(tramadol_NSAID, file="R_datasets/tramadol_NSAID.RData")
# save(tramadol_non_NSAID, file="R_datasets/tramadol_non_NSAID.RData")
   



# stage_two:select eligible cohorts (test codes) ------------------------------------------------------
# create auxiliary variables
cohort_all_new <- 
  tramadol_celecoxib %>% 
  filter(seq == 1) %>% 
  # filter (select one billing record for each persom)
  left_join( demography, by = "idp") %>% 
  left_join( check_dup_diagnosis, by = "idp") %>% 
  left_join( Dic_CER_adverse_events, by = "dia_cod") %>% 
  #important: check if there are missing values for demographics variables
  # sapply(function(x)sum(is.na(x)))
  #index for intiation age and continuous register duration
  mutate( initiation_age = as.numeric( difftime(first_time, date_of_birth, units = "days")/365 ),
          initiation_gap = as.numeric( difftime(first_time, entry_date, units = "days")/365) ) %>% 
  group_by(idp) %>% 
  #index for previous adverse events 
  mutate( history_adverse_events = case_when( any( !is.na(Disease_Group) & dia_start_date <= first_time ) ~ 1,
                                            TRUE ~ 0),
         #index for new adverse events
          any_adverse_events = case_when( any( !is.na(Disease_Group) & dia_start_date > first_time ) ~ 1,
                                        TRUE ~ 0)) %>%
  ungroup() %>% 
  # filter (select age>=18 and continuous data >=1 year)
  filter( history_adverse_events == 0, initiation_age >=18, initiation_gap >=1) %>% 
  # # filter (exclude some usefulless disease records)
  filter( !(any_adverse_events == 1 & is.na(Disease_Group))) %>% 
  group_by(idp) %>% 
  #index for follow-up period
  mutate(follow_up_time = case_when( any_adverse_events == 0 ~ as.numeric( difftime( departure_date, first_time, units = "days")),
                                     any_adverse_events == 1 ~ as.numeric( min( difftime( dia_start_date, first_time, units = "days"))))) %>% 
  ungroup() %>% 
  distinct(idp, sex, first_time, first_drug, initiation_age, any_adverse_events, follow_up_time)
  

bb <- 
  cohort_all_multiple_row_new %>% 
  left_join( social_variables, by = "idp") %>% 
  group_by( first_drug) %>% 
  summarise(n = n(), 
            mean_age = mean(initiation_age), 
            events = sum(any_adverse_events), 
            mean_follow = as.numeric(mean(follow_up_time)/365)) %>% 
  ungroup() %>% 
  mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
  mutate( ratio = rate[2] / rate[1])


# stage_two:select eligible cohorts (wrap codes) ------------------------------------------------------
# create auxiliary variables
stage_two_function <- function( data){
  new_dateset <- 
    data %>% 
    filter(seq == 1) %>% 
    # filter (select one billing record for each persom)
    left_join( demography, by = "idp") %>% 
    left_join( check_dup_diagnosis, by = "idp") %>% 
    left_join( Dic_CER_adverse_events, by = "dia_cod") %>% 
    #index for intiation age and continuous register duration
    mutate( initiation_age = as.numeric( difftime(first_time, date_of_birth, units = "days")/365 ),
            initiation_gap = as.numeric( difftime(first_time, entry_date, units = "days")/365) ) %>% 
    group_by(idp) %>% 
    #index for previous adverse events 
    mutate( history_adverse_events = case_when( any( !is.na(Disease_Group) & dia_start_date <= first_time ) ~ 1,
                                                TRUE ~ 0),
            #index for new adverse events
            any_adverse_events = case_when( any( !is.na(Disease_Group) & dia_start_date > first_time ) ~ 1,
                                            TRUE ~ 0)) %>%
    ungroup() %>% 
    # filter (select age>=18 and continuous data >=1 year)
    filter( history_adverse_events == 0, initiation_age >=18, initiation_gap >=1) %>% 
    # # filter (exclude some usefulless disease records)
    filter( !(any_adverse_events == 1 & is.na(Disease_Group))) %>% 
    group_by(idp) %>% 
    #index for follow-up period
    mutate(follow_up_time = case_when( any_adverse_events == 0 ~ as.numeric( difftime( departure_date, first_time, units = "days")),
                                       any_adverse_events == 1 ~ as.numeric( min( difftime( dia_start_date, first_time, units = "days"))))) %>% 
    ungroup() %>% 
    distinct(idp, sex, first_time, first_drug, initiation_age, any_adverse_events, follow_up_time)
  return(new_dateset)
}


data_raw <- list(tramadol_codeine, tramadol_ibuprofen, tramadol_diclofenac,tramadol_celecoxib, tramadol_Paracetamol)
stage_two_saved_data <- lapply(data_raw, stage_two_function)
names(stage_two_saved_data) <- paste( "stage_two", c("tramadol_codeine", "tramadol_ibuprofen", "tramadol_diclofenac","tramadol_celecoxib", "tramadol_Paracetamol"), sep = "_")
save(stage_two_saved_data, file="R_datasets/stage_two_saved_data.RData")




# stage_three: extract potential confounders (test codes)-------------------------------------------------------------


stage_three_saved_data <- 
  cohort_all_new %>% 
  left_join( social_variables, by = "idp") %>% 
  left_join( BMI_dataset, by = "idp") %>% 
  #deal with missing variables: economic_level, rural
  mutate(economic_level = case_when(economic_level == "" ~ NA_character_,
                                    TRUE ~ economic_level),
         rural = case_when(rural == "" ~ NA_character_,
                           TRUE ~ rural)) %>% 
  mutate(BMI_record_gap = as.numeric( difftime(first_time, clinical_date, units = "days"))) %>% 
  # index for eligiable records of BMI (6 months before index date)
  mutate( BMI_index_records = case_when( BMI_record_gap >= 0 & BMI_record_gap <= 730 ~ 1,
                     TRUE ~ 0)) %>% 
  group_by(idp) %>% 
  arrange( desc(BMI_index_records), BMI_record_gap) %>% 
  mutate( BMI_value = case_when(any(BMI_index_records == 1) ~ val[1],
          TRUE ~ NA_real_)) %>% 
  ungroup() %>% 
  distinct(idp, sex, first_time, first_drug, initiation_age, any_adverse_events, follow_up_time, 
           economic_level, rural,
           BMI_value) %>% 
  
  # use check_dup_diagnosis rather than diagnosis
  left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>% 
  left_join( Dic_commorbidity, by = "dia_cod") %>% 
  # index for eligiable records of history of other disease
  mutate( commorbidities_index_records = case_when( dia_start_date <= first_time & !is.na(commorbidity_label) ~ 1,
                                        TRUE ~ 0)) %>% 
  # replace unnecessary disease label with NA
  mutate( commorbidity_label = case_when(commorbidities_index_records == 1 ~ commorbidity_label,
                                         TRUE ~ NA_character_)) %>% 
  # index for eligiable subjects of history of other disease
  group_by(idp) %>% 
  mutate( commorbidities_index_subjects = case_when(any(commorbidities_index_records == 1) ~ 1,
                                                   TRUE ~ 0)) %>% 
  ungroup() %>% 
  # filter unnecessary records
  filter( !(commorbidities_index_subjects == 1 & commorbidities_index_records == 0) ) %>% 
  # remove duplicate diagnosis
  distinct(idp, sex, first_time, first_drug, initiation_age, any_adverse_events, follow_up_time, 
           economic_level, rural,
           BMI_value,
           commorbidity_label, commorbidities_index_records) %>% 
  # transform from long to wide data
  spread(commorbidity_label, commorbidities_index_records)
  

save(stage_three_saved_data, file="R_datasets/stage_three_saved_data.RData")




  




# Cox-model ---------------------------------------------------------------
bb <- 
  totol_data[[1]] %>% 
  distinct(idp, first_time, first_drug, sex, initiation_age,  any_adverse_events, follow_up_time) %>% 
  mutate(first_drug = as.factor(first_drug), 
         follow_up_time = as.numeric(follow_up_time)) %>% 
  as.data.frame()

cox_model <- coxph(Surv(follow_up_time, any_adverse_events) ~ first_drug + sex + initiation_age , data =  bb)
summary(cox_model)
cox_model$xlevels

set.seed(1)
cc <- sample_n(bb, 1000, replace = FALSE)

plot_list <- ggadjustedcurves(cox_model, data = cc, method = "average", variable = "first_drug")  
plot_data <- plot_list$data
plot_data$surv <- (1 - (plot_data$surv))


# Plot COX model (test codes) ----------------------------------------------------
T_codeine_cox_plot <- 
  ggplot(data = plot_data, aes(x = time , y = surv, color = variable)) +
  geom_line(size = 0.4) + 
  scale_x_continuous(limits = c(0, max(plot_data$time)),
                     breaks = seq(0,max(plot_data$time)+365, 365),
                     labels = c(0:11))+ 
  scale_y_continuous(limits = c(0, 0.18), breaks = seq(0,18,0.02))+
  
  labs(x = "\nFollow up (year)",
       y = "Cumulative risk\nof adverse events",
       fill = "Types",
       subtitle = "Tramadol vs Codeine")+
  
  scale_color_lancet()+
  theme(
    text = element_text(family = "Candara", colour = "black"),
    # panel.border = element_rect(fill=NA),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    # panel.grid.minor.x = element_blank(),
    # panel.grid.minor.y = element_line(color = "grey"),
    # panel.spacing = unit(0.5, "lines"),
    # plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    strip.background = element_blank(),
    # strip.text = element_blank(),
    
    axis.line = element_line(),
    # axis.text.x  = element_text( angle = 45, vjust = 0.5),
    axis.title.y  = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
    
    legend.position = c(0.05, 0.99),
    legend.justification=c(0,1),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(margin = margin(l = -15)),
    legend.text.align = 0,
    # legend.margin=margin(t = 10, r = 0, b = 10, l = -5),
    legend.box.margin=margin(0,0,0,0),
    legend.spacing.x = unit(25, 'pt'),
    legend.spacing.y = unit(-5, 'pt'),
    aspect.ratio = 0.66) 

T_codeine_cox_plot




# Plot COX model (wrap up)  -----------------------------------------------



# KM_curve ----------------------------------------------------------------

KM_mode <- npsurv(Surv(follow_up_time, any_adverse_events) ~ first_drug, data = bb)

plot(KM_mode)


survplot(KM_mode,
         label.curves = FALSE,
         levels.only  = TRUE,  
         
         conf = "none",
         # xlim(0,1000),
         fun = function(x) {1 - x},
         time.inc = 100)




# Save_plot ---------------------------------------------------------------
ggsave(filename = "T_codeine_cox_plot.png",
       path = "D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research/Figures",
       plot = T_codeine_cox_plot,
       width = 6,
       height = 4,
       dpi = 300,
       type = "cairo")




