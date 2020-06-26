#### Set up library ----------------------------------------------------------
# memory.limit()
# memory.limit(62468)
# library(dplyr)
# library(zoo)
# library(lubridate)#
library( tidyverse)
library(readxl)
# library(ggplot2)
library(remotes)
library(survival)
library(survminer)
# library(cowplot)
# library(scales)
library(ggsci)
# library(mefa4)
# library(pryr)
# library(gmodels)
# library(extrafont) #font
# library(labeling)
library(readr)
# library(tidyr)
# library(stringr)
library(xlsx)
# library(naniar)
# library(visdat)
# library(finalfit)
# library(gridExtra)  #masking  'package:dplyr'  combine
library(rms)
library(hrbrthemes)
library(tableone)
library(MatchIt)
library(optmatch)
# library(ckbplotr)
library(mice)
library(lattice)
library(cobalt)
library(survivalAnalysis)
library( comorbidity)
library(fmsb)
library( grid)
library( devEMF)
library( condSURV)
library( stddiff)
# library(fastDummies)
# load raw and label datasets -------------------------------------------------------
# attention: which biliing dataset is used
load("D:/DPhil/Project_Opioid_use/Data/billing_delay.RData")
# attention: which diagnosis dataset is used
load("D:/DPhil/Project_Opioid_use/Data/diagnosis_complete.RData")
load("D:/DPhil/Project_Opioid_use/Data/demography.RData")
load("D:/DPhil/Project_Opioid_use/Data/social_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/clinical_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/admission.RData")
Denominator_data <- read_delim("D:/DPhil/Data_raw/OPIODU/OPIOIDES_entregable_poblacio_denominadors_20191219_143606.txt",
                               delim = "|",
                               col_names = TRUE)
#dictionary datasets
catalog_clear <- read_excel("D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research/Excel/catalog_clear_codeine_tramadol.xlsx") %>% 
                 filter( cod_type != "farmacs_prescrits") #important filter, otherwise some row would be duplicated when linked

#prepared datasets
# load("R_datasets/Final_cohort_codeine_100.RData")
# load("R_datasets/baseline_cohort_codeine_add_outcome_100.RData")
# load("R_datasets/baseline_cohort_codeine_add_outcome_100_imputation.RData")
# load("R_datasets/PS_model_codeine_add_outcome_100.RData")
# 
# load("R_datasets/On_treatment_codeine_add_outcome_100.RData")
# load("R_datasets/ATT_combined_add_outcome_codeine_dataframe.RData")
# load("R_datasets/Intention_combined_add_outcome_codeine_dataframe.RData")

# Little explore with raw dta ---------------------------------------------
head(billing)
str(billing)
glimpse(billing)
glimpse(diagnosis)
# check missing of each variable (no missing)
map_dbl(diagnosis_complete, ~ sum(is.na(.)))

#make sure there is no duplicate 
check_dup_billing <- distinct(billing_delay, idp, billing_cod, bill_date, .keep_all = TRUE) #(confirm no duplicate row)
check_dup_diagnosis <- distinct(diagnosis_complete, idp, dia_cod, dia_start_date, .keep_all = TRUE) #(confirm with duplicate row)
check_dup_social_variables <- distinct(social_variables, idp, economic_level, rural, .keep_all = TRUE) #(confirm no duplicate row)
check_dup_clinical_variables <- distinct(clinical_variables,idp,clinical_cod, clinical_date, val, .keep_all = TRUE) #(confirm no duplicate row)


# source population(Indicated by MSK) -----------------------------------------------------
set.seed(1)
sub_Denominator <- sample_frac(Denominator_data, 0.1)
#=======================================================#
# all registered subjects with billing data after 2007
#=======================================================#
MSK_population_codeine <- 
  Denominator_data %>% 
  select( idp) %>% 
  left_join( select( billing_delay, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  # filter studied drugs and observation period
  filter(  Drug_name %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
  arrange( bill_date) %>% 
  group_by( idp) %>% 
  mutate( seq = row_number(), 
          total_seq = n(), 
          first_bill_time = bill_date[1], 
          first_bill_drug = Drug_name[1]) %>% # create two index
  ungroup() %>% 
  # filter (select one billing record for each person)
  filter( seq == 1) %>% 
  select( idp, first_bill_time) %>% 
  
  #==================================================================#
  # MSK indications
  #==================================================================#
  
  left_join( select(check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp")  %>% 
  left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  rename( disease_name = English_label, disease_group = variable_group) %>% 
  group_by( idp) %>% 

  mutate( MSK_indication = case_when( any( disease_name %in%  c("back_pain", "neck_pain", 
                                                                "fybromialgia", "oa", "rheumatoid_arthritis", "osteoporosis", "other_musculskeletal_disorders") 
                                           & dia_start_date <= first_bill_time & 
                                             dia_start_date > first_bill_time -365*3) ~ 1, TRUE ~ 0)) %>% 
  distinct( idp, MSK_indication) %>% 
  ungroup()


MSK_new_user_idp <- 
  database_population_codeine %>% 
  left_join( MSK_population_codeine, by = "idp") %>% 
  filter( index_double_user == 0,
          index_age == 0, # Continuous enrolment in the database < 1 year before the entry date 
          index_look_back == 0,# withou dispensation one year before index date
          index_continuous_enrolment == 0,# Dispensed both tramadol and codeine on the entry date
          index_history_outcomes == 0,  # No outcomes of interest previous or at the time of the entry date
          MSK_indication == 1)

#==================save=============================================#
# save(MSK_population_codeine, file="R_datasets/MSK_population_codeine.RData")
#==================save=============================================#

# Link to imputed covariates ----------------------------------------
MSK_new_user_cohort <- 
  MSK_new_user_idp %>% 
  select( idp) %>% 
  left_join( Base_new_user_cohort_imputed, by = "idp") 

map_dbl( MSK_new_user_cohort, ~ sum(is.na(.)))

# Propensity score modelling and matching ----------------------------------------------
## Get variables names
dput( names( MSK_new_user_cohort))

impute_covariate <- 
  c( "sex", "economic_level", "rural", "initiation_age", 
     "BMI_value",
     "alzheimer_disease", "angina", "back_pain", "burn_injuries", "cancer", "cardiac_arrhythmia", 
     "chronic_kidney_disease", "chronic_liver_disease", "copd", "cough", 
     "diabetes", "diarrhoea", "dyspnea", "fybromialgia", "malabsorption_disorder", 
     "neck_pain", "neurologic_pathologies", "oa", "osteoporosis", 
     "other_musculskeletal_disorders", "parkinson_disease", "peripheral_vascular_disease", 
     "pulmonary_oedema", "rheumatoid_arthritis", "surgery", "tia", 
     "traffic", 
     "cci_group", 
     "anticonvulsant", "benzodiazepines", "Celecoxib", 
     "Diclofenaco", "fentanyl", "hypnotics", "Ibuprofeno", "Metamizole", 
     "morphine", "Naproxeno", "NSAID", "Paracetamol", "SSIR", 
     "GP_visits", "HP_admissions")


## Fit model
psModel <- glm(reformulate(termlabels = impute_covariate, response = "first_bill_drug"),
               family  = binomial(link = "logit"),
               data    = MSK_new_user_cohort)

MSK_new_user_cohort_probability <- 
  MSK_new_user_cohort %>% 
  mutate( P_tramadol = psModel$fitted.values,
          P_codeine = 1 - psModel$fitted.values)

listMatch <- Matching::Match(Tr       = MSK_new_user_cohort_probability$first_bill_drug,      # Need to be in 0,1
                             ## logit of PS,i.e., log(PS/(1-PS)) as matching scale
                             X        = log(MSK_new_user_cohort_probability$P_tramadol / MSK_new_user_cohort_probability$P_codeine),
                             ## 1:1 matching
                             M        = 1,
                             ## caliper = 0.2 * SD(logit(PS))
                             caliper  = 0.2,
                             replace  = FALSE,
                             ties     = FALSE,
                             version  = "fast")

Mathced_new_MSK_user_cohort <- MSK_new_user_cohort_probability[unlist(listMatch[c("index.treated","index.control")]), ]
table(Mathced_new_MSK_user_cohort$first_bill_drug)


#==================save=============================================#
load("R_datasets/Mathced_new_MSK_user_cohort.RData")
# save(Mathced_new_MSK_user_cohort, file="R_datasets/Mathced_new_MSK_user_cohort.RData")
#==================save=============================================#



# Cox-model outcome stratification (ITT) ------------------------------------------------
cox_dataset_ITT <- 
  Mathced_new_MSK_user_cohort %>% 
  select( idp, first_bill_drug) %>% 
  left_join( ITT_outcomes_dataframe, by = "idp") %>% 
  left_join( select(First_billing_dataset, idp, first_bill_time), by = "idp") %>%  
  # filter( outcome_label != "opioid_abuse") %>% 
  mutate( calendar_year = substring( first_bill_time,1,4)) 

res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$group_label)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug + calendar_year,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = cox_dataset_ITT)

survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
    dt <- summary(x)
    # p.value<-signif(dt$wald["pvalue"], digits=2)
    # wald.test<-signif(x$wald["test"], digits=3)
    # beta<-signif(x$coef[1], digits=3);#coeficient beta
    P_value <- dt$coefficients["first_bill_drug", "Pr(>|z|)"];
    HR <-dt$conf.int["first_bill_drug","exp(coef)"]
    HR.confint.lower <-dt$conf.int["first_bill_drug","lower .95"]
    HR.confint.upper <- dt$conf.int["first_bill_drug","upper .95"]
    # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c( HR , P_value, HR.confint.lower, HR.confint.upper)
    names(res)<-c( "estimate", "P_value","lower", "upper")
    return(res)
    #return(exp(cbind(coef(x),confint(x)))) 
  }) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), " to ",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")", sep = ""),
            P_value_format = case_when( P_value <0.001 ~ "<0.001",
                                        TRUE ~ paste( substring( P_value, 2, str_locate( as.character(P_value), "[.]")[,1] + 3))))}

ITT_survial_hazard <- survial_hazard_func(input = res.separate)

# #==================================================================#
# # Calculate addtional statistics ( mean follow up, rate)
# #==================================================================#
summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( outcome_label, group_label, first_bill_drug) %>% 
    summarise(n = n(), 
              # mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days)/365), 
              total_follow = as.numeric( sum(follow_up_days)/365)) %>% 
    group_by(outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    mutate( dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$estimate * 1000) %>% 
    mutate( low_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[1] * 1000) %>% 
    mutate( high_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[2] * 1000) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>% 
    mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2),
            dif_CI = paste(substring(dif, 1,str_locate(as.character(dif), "[.]")[,1] + 2), " (",
                           substring(low_dif, 1,str_locate(as.character(low_dif), "[.]")[,1] + 2), " to ",
                           substring(high_dif, 1,str_locate(as.character(high_dif), "[.]")[,1] + 2), ")", sep = ""))
  
  return(summary_table)
}
ITT_survial_event <- summary_table_func( inputdata = cox_dataset_ITT)

# #==================================================================#
# # combine all statistics 
# #==================================================================#
ITT_survial_summary <- 
  filter( ITT_survial_event, first_bill_drug ==0) %>% 
  left_join( filter( ITT_survial_event, first_bill_drug ==1), by = c("outcome_label", "group_label")) %>% 
  mutate( variable = paste( outcome_label,group_label, sep = ".")) %>% 
  left_join( ITT_survial_hazard, by = "variable") %>% 
  ungroup() %>%  
  mutate( outcome_label = factor( outcome_label, levels = c("composite_CVD", "fracturas",  "all_cause_mortality", "falls", "constipation", "sleep_disorders", "delirium", "opioid_abuse"))) %>% 
  arrange( outcome_label)


# Cox-model outcome stratification CVD and fractures (ITT) ------------------------------------------------
cox_dataset_CVD_fracture <- 
  Mathced_new_MSK_user_cohort %>% 
  select( idp, first_bill_drug) %>% 
  left_join( ITT_outcomes_dataframe, by = "idp") %>% 
  left_join( select(First_billing_dataset, idp, first_bill_time), by = "idp") %>%  
  filter( outcome_label %in%  c("composite_CVD", "fracturas") ) %>% 
  mutate( calendar_year = substring( first_bill_time,1,4)) %>% 
  select( -group_label)


CVD_fracture <-   list(stroke = filter(cox_dataset_CVD_fracture, one_year_outcome_name %in% c("stroke", "No outcome ouccured")),
                       MI = filter(cox_dataset_CVD_fracture, one_year_outcome_name %in% c("myocardial_infarction", "No outcome ouccured")),
                       heart_failure = filter(cox_dataset_CVD_fracture, one_year_outcome_name %in% c("cardiac_insufficiency_heart_failure", "No outcome ouccured"))
                                   ) %>% 
  bind_rows( .id = "group_label")  



res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$group_label)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug + calendar_year,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = CVD_fracture)

survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
    dt <- summary(x)
    # p.value<-signif(dt$wald["pvalue"], digits=2)
    # wald.test<-signif(x$wald["test"], digits=3)
    # beta<-signif(x$coef[1], digits=3);#coeficient beta
    P_value <- dt$coefficients["first_bill_drug", "Pr(>|z|)"];
    HR <-dt$conf.int["first_bill_drug","exp(coef)"]
    HR.confint.lower <-dt$conf.int["first_bill_drug","lower .95"]
    HR.confint.upper <- dt$conf.int["first_bill_drug","upper .95"]
    # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c( HR , P_value, HR.confint.lower, HR.confint.upper)
    names(res)<-c( "estimate", "P_value","lower", "upper")
    return(res)
    #return(exp(cbind(coef(x),confint(x)))) 
  }) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), " to ",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")", sep = ""),
            P_value_format = case_when( P_value <0.001 ~ "<0.001",
                                        TRUE ~ paste( substring( P_value, 2, str_locate( as.character(P_value), "[.]")[,1] + 3))))}

CVD_fracture_survial_hazard <- survial_hazard_func(input = res.separate)

# #==================================================================#
# # Calculate addtional statistics ( mean follow up, rate)
# #==================================================================#
summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( outcome_label, group_label, first_bill_drug) %>% 
    summarise(n = n(), 
              # mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days)/365), 
              total_follow = as.numeric( sum(follow_up_days)/365)) %>% 
    group_by(outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    mutate( dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$estimate * 1000) %>% 
    mutate( low_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[1] * 1000) %>% 
    mutate( high_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[2] * 1000) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>% 
    mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2),
            dif_CI = paste(substring(dif, 1,str_locate(as.character(dif), "[.]")[,1] + 2), " (",
                           substring(low_dif, 1,str_locate(as.character(low_dif), "[.]")[,1] + 2), " to ",
                           substring(high_dif, 1,str_locate(as.character(high_dif), "[.]")[,1] + 2), ")", sep = ""))
  
  return(summary_table)
}
CVD_fracture_survial_event <- summary_table_func( inputdata = CVD_fracture)

# #==================================================================#
# # combine all statistics 
# #==================================================================#
CVD_fracture_survial_summary <- 
  filter( CVD_fracture_survial_event, first_bill_drug ==0) %>% 
  left_join( filter( CVD_fracture_survial_event, first_bill_drug ==1), by = c("outcome_label", "group_label")) %>% 
  mutate( variable = paste( outcome_label,group_label, sep = ".")) %>% 
  left_join( ITT_survial_hazard, by = "variable") %>% 
  ungroup() %>% 
  mutate( outcome_label = factor( outcome_label, levels = c("composite_CVD", "fracturas",  "all_cause_mortality", "falls", "constipation", "sleep_disorders", "delirium"))) %>% 
  arrange( outcome_label)


# Cox-model outcome phase stratification (ITT) ------------------------------------------------
cox_dataset_phase_ITT <- 
  Mathced_new_user_cohort %>% 
  select( idp, first_bill_drug) %>% 
  left_join( ITT_outcomes_phase_dataframe, by = "idp") %>% 
  left_join( select(First_billing_dataset, idp, first_bill_time, env), by = "idp") %>% 
  filter( outcome_label %in% c("composite_CVD", "fracturas", "all_cause_mortality")) %>% 
  mutate( calendar_year = substring( first_bill_time,1,4)) 



res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$group_label)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug + calendar_year,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = cox_dataset_phase_ITT)

survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
    dt <- summary(x)
    # p.value<-signif(dt$wald["pvalue"], digits=2)
    # wald.test<-signif(x$wald["test"], digits=3)
    # beta<-signif(x$coef[1], digits=3);#coeficient beta
    P_value <- dt$coefficients["first_bill_drug", "Pr(>|z|)"];
    HR <-dt$conf.int["first_bill_drug","exp(coef)"]
    HR.confint.lower <-dt$conf.int["first_bill_drug","lower .95"]
    HR.confint.upper <- dt$conf.int["first_bill_drug","upper .95"]
    # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c( HR , P_value, HR.confint.lower, HR.confint.upper)
    names(res)<-c( "estimate", "P_value","lower", "upper")
    return(res)
    #return(exp(cbind(coef(x),confint(x)))) 
  }) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), " to ",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")", sep = ""),
            P_value_format = case_when( P_value <0.001 ~ "<0.001",
                                        TRUE ~ paste( substring( P_value, 2, str_locate( as.character(P_value), "[.]")[,1] + 3))))}

ITT_survial_phrase_hazard <- survial_hazard_func(input = res.separate)

# #==================================================================#
# # Calculate addtional statistics ( mean follow up, rate)
# #==================================================================#
summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( outcome_label, group_label, first_bill_drug) %>% 
    summarise(n = n(), 
              # mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days)/365), 
              total_follow = as.numeric( sum(follow_up_days)/365)) %>% 
    group_by(outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    mutate( dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$estimate * 1000) %>% 
    mutate( low_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[1] * 1000) %>% 
    mutate( high_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[2] * 1000) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>% 
    mutate( total_follow = substring(total_follow, 1,str_locate(as.character(total_follow), "[.]")[,1]-1)) %>% 
    mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2),
            dif_CI = paste(substring(dif, 1,str_locate(as.character(dif), "[.]")[,1] + 2), " (",
                           substring(low_dif, 1,str_locate(as.character(low_dif), "[.]")[,1] + 2), " to ",
                           substring(high_dif, 1,str_locate(as.character(high_dif), "[.]")[,1] + 2), ")", sep = ""))
  
  return(summary_table)
}
ITT_survial_phrase_event <- summary_table_func( inputdata = cox_dataset_phase_ITT)

# #==================================================================#
# # combine all statistics 
# #==================================================================#
ITT_survial_phrase_summary <- 
  filter( ITT_survial_phrase_event, first_bill_drug ==0) %>% 
  left_join( filter( ITT_survial_phrase_event, first_bill_drug ==1), by = c("outcome_label", "group_label")) %>% 
  mutate( variable = paste( outcome_label,group_label, sep = ".")) %>% 
  left_join( ITT_survial_phrase_hazard, by = "variable") %>% 
  ungroup()




# Cox-model interaction stratification(ITT) ------------------------------------------------
cox_interraction_ITT <- 
  Mathced_new_user_cohort %>% 
  left_join( ITT_outcomes_dataframe, by = "idp") %>% 
  left_join( select(First_billing_dataset, idp, first_bill_time), by = "idp") %>% 
  left_join( select(Baseline_covariate_complete, idp, any_MSK, any_pain, any_NSAIDs, any_Psychotropic), by = "idp") %>% 
  filter( outcome_label %in% c("composite_CVD", "fracturas", "all_cause_mortality"), group_label == "one_year_outcome_dataset") %>% 
  mutate( calendar_year = substring( first_bill_time,1,4)) %>% 
  mutate( separate_label = outcome_label) %>% 
  select( -outcome_label, -group_label) %>% 
  mutate( age_group = case_when( initiation_age >=18 & initiation_age < 40 ~ "18-39",
                                 initiation_age >=40 & initiation_age < 60 ~ "40-59",
                                 initiation_age >=60  ~ ">=60"))




cox_dataset_interaction <-   list( overall = cox_interraction_ITT,
                                   sex = cox_interraction_ITT,
                                   initiation_age = cox_interraction_ITT,
                                   cancer = cox_interraction_ITT,
                                   any_MSK = cox_interraction_ITT,
                                   cough = cox_interraction_ITT, 
                                   any_pain = cox_interraction_ITT,
                                   cci_group = cox_interraction_ITT,
                                   any_Psychotropic = cox_interraction_ITT,
                                   any_NSAIDs = cox_interraction_ITT
                                   ) %>% 
  bind_rows( .id = "outcome_label") %>% 
  mutate( group_label = case_when( outcome_label == "overall" ~ "overall", 
                                   outcome_label == "sex" ~ as.character(sex), 
                                   outcome_label == "initiation_age" ~ age_group,
                                   outcome_label == "cancer" ~ as.character(cancer),
                                   outcome_label == "any_MSK" ~ as.character(any_MSK),
                                   outcome_label == "any_pain" ~ as.character(any_pain),
                                   outcome_label == "cough" ~ as.character(cough),
                                   outcome_label == "cci_group" ~ paste(as.character(cci_group), "dis", sep = ""),
                                   outcome_label == "any_Psychotropic" ~ as.character(any_Psychotropic),
                                   outcome_label == "any_NSAIDs" ~ as.character(any_NSAIDs)
                                   )) %>% 
  mutate( variable = paste(outcome_label, group_label, sep = "."))


res.separate_func <- function(input){
  lapply( split(input, list(input$separate_label, input$variable)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug + calendar_year,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = cox_dataset_interaction)

survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
    dt <- summary(x)
    # p.value<-signif(dt$wald["pvalue"], digits=2)
    # wald.test<-signif(x$wald["test"], digits=3)
    # beta<-signif(x$coef[1], digits=3);#coeficient beta
    P_value <- dt$coefficients["first_bill_drug", "Pr(>|z|)"];
    HR <-dt$conf.int["first_bill_drug","exp(coef)"]
    HR.confint.lower <-dt$conf.int["first_bill_drug","lower .95"]
    HR.confint.upper <- dt$conf.int["first_bill_drug","upper .95"]
    # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c( HR , P_value, HR.confint.lower, HR.confint.upper)
    names(res)<-c( "estimate", "P_value","lower", "upper")
    return(res)
    #return(exp(cbind(coef(x),confint(x)))) 
  }) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), " to ",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")", sep = ""),
            P_value_format = case_when( P_value <0.001 ~ "<0.001",
                                        TRUE ~ paste( substring( P_value, 2, str_locate( as.character(P_value), "[.]")[,1] + 3))))}

survial_hazard <- survial_hazard_func(input = res.separate)



# #==================================================================#
# # Calculate addtional statistics ( mean follow up, rate)
# #==================================================================#
summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( separate_label, outcome_label,group_label, first_bill_drug) %>% 
    summarise(n = n(), 
              mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days)/365),
              total_follow = as.numeric( sum(follow_up_days)/365)) %>% 
    group_by(separate_label, outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    mutate( dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$estimate * 1000) %>% 
    mutate( low_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[1] * 1000) %>% 
    mutate( high_dif = ratedifference( events[2], events[1], total_follow[2], total_follow[1])$conf.int[2] * 1000) %>% 
    mutate( NNH = 1/dif*100) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>%
    mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2))
  
  
  return(summary_table)
}
survial_event <- summary_table_func( inputdata = cox_dataset_interaction)

# #==================================================================#
# # combine all statistics 
# #==================================================================#
survial_summary <- 
  filter( survial_event, first_bill_drug ==0) %>% 
  left_join( filter( survial_event, first_bill_drug ==1), by = c("separate_label", "outcome_label", "group_label")) %>% 
  mutate( variable = paste( separate_label, outcome_label, group_label, sep = ".")) %>% 
  left_join( survial_hazard, by = "variable") %>% 
  mutate( RD_estimate = as.numeric(rate.x) * (estimate - 1  ),
          RD_lower = as.numeric(rate.x) * (lower - 1  ),
          RD_upper = as.numeric(rate.x) * (upper - 1  ),) %>% 
  mutate( RD_CI = paste(substring(RD_estimate, 1,str_locate(as.character(RD_estimate), "[.]")[,1] + 2), " (",
                 substring(RD_lower, 1,str_locate(as.character(RD_lower), "[.]")[,1] + 2), " to ",
                 substring(RD_upper, 1,str_locate(as.character(RD_upper), "[.]")[,1] + 2), ")", sep = ""))




# Kaplan-Meier survival estimate ------------------------------------------
cox_dataset_ITT <- 
  Mathced_new_user_cohort %>% 
  select( idp, first_bill_drug) %>% 
  left_join( ITT_outcomes_dataframe, by = "idp") %>% 
  left_join( select(First_billing_dataset, idp, first_bill_time, env), by = "idp") %>% 
  filter( outcome_label %in% c( "all_cause_mortality")) 

c("composite_CVD", "fracturas", "all_cause_mortality")

Fit_data <- npsurv(Surv(follow_up_days, outcome_occur_subject) ~ first_bill_drug, 
                   data = cox_dataset_ITT) %>% 
  ggsurvplot(fun = function(x) {-log(x)}, risk.table = TRUE, break.time.by = 90)

plot_data <- Fit_data$plot$data



# res.cox <- coxph(Surv(follow_up_days, outcome_occur_subject) ~ first_bill_drug + calendar_year, data =  cox_dataset_ITT)
# test.ph <- cox.zph(res.cox)
# test.ph
# 
# ggcoxzph(test.ph)


main_table_func<- function( target){
  cox_dataset_ITT <- 
    Mathced_new_user_cohort %>% 
    select( idp, first_bill_drug) %>% 
    left_join(ITT_outcomes_dataframe, by = "idp") %>% 
    filter( outcome_label == target)
  
  
  Fit_data <- npsurv(Surv(follow_up_days, outcome_occur_subject) ~ first_bill_drug, 
                     data = cox_dataset_ITT) %>% 
    ggsurvplot(fun = function(x) {-log(x)}, risk.table = TRUE, break.time.by = 90)
  
  table_data <- Fit_data$table$data
  return(table_data)
  
}
main_table_CVD <- main_table_func( target = "composite_CVD")
main_table_Fracture <- main_table_func( target = "fracturas") 
main_table_mortality <- main_table_func( target = "all_cause_mortality") 



main_plot_func_CVD <- function( target){
  cox_dataset_ITT <- 
    Mathced_new_user_cohort %>% 
    select( idp, first_bill_drug) %>% 
    left_join(ITT_outcomes_dataframe, by = "idp") %>% 
    filter( outcome_label == target)
  
  
  Fit_data <- npsurv(Surv(follow_up_days, outcome_occur_subject) ~ first_bill_drug, 
                     data = cox_dataset_ITT) %>% ggsurvplot(fun = function(x) {-log(x)}, risk.table = TRUE, break.time.by = 90)
  
  plot_data <- Fit_data$plot$data
  table_data <- Fit_data$table$data
  
  ggplot( data = plot_data, aes( x = time, y = surv * 1000, color = first_bill_drug)) +
    # geom_point( size = 0.001) +
    # geom_line( ) +
    geom_step( size = 0.35) +
    annotate(geom="text", x= c(300, 300), y=c(3,8), label= c("Codeine", "Tramadol"), size  = 2, family = "sans", hjust = 0)+
    annotate(geom="text", x= c(20), y=c(22.5), label= paste("HR ", 
                                                            substring( ITT_survial_summary[1,"estimate"], 1,str_locate(as.character( ITT_survial_summary[1,"estimate"]), "[.]")[,1] + 2), 
                                                            " (95%, ",
                                                            substring( ITT_survial_summary[1,"lower"], 1,str_locate(as.character( ITT_survial_summary[1,"lower"]), "[.]")[,1] + 2), 
                                                            "-",
                                                            substring( ITT_survial_summary[1,"upper"], 1,str_locate(as.character( ITT_survial_summary[1,"upper"]), "[.]")[,1] + 2), 
                                                            ")",
                                                            sep = ""), 
             
             size  = 2, family = "sans", hjust = 0)+
    scale_x_continuous( expand = c(0, 0),limits = c( 0, 370), breaks = seq( 0, 360, 90), labels = seq( 0, 12, 3))+
    scale_y_continuous( expand = c(0, 0), limits = c( 0, 25))+
    labs(x = "Months of Follow-up ",
         y = "Cumulative\nIncidence Rate, ‰")+
    scale_color_jama() +
    
    theme(
      text = element_text(family = "sans", colour = "black", size  = 6),
      axis.text = element_text(colour = "black"),
      line = element_line( size = 0.25),
      # panel.border = element_rect(fill=NA),
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey"),
      # panel.spacing = unit(0.5, "lines"),
      # plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      strip.background = element_blank(),
      # strip.text = element_blank(),
      axis.line = element_line(),
      # axis.text.x  = element_text( angle = 45, vjust = 0.5),
      # axis.title.y  = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
      legend.position = "none",
      aspect.ratio = 0.55) 
}
main_plot_func_fracture <- function( target){
  cox_dataset_ITT <- 
    Mathced_new_user_cohort %>% 
    select( idp, first_bill_drug) %>% 
    left_join(ITT_outcomes_dataframe, by = "idp") %>% 
    filter( outcome_label == target)
  
  
  Fit_data <- npsurv(Surv(follow_up_days, outcome_occur_subject) ~ first_bill_drug, 
                     data = cox_dataset_ITT) %>% ggsurvplot(fun = function(x) {-log(x)}, risk.table = TRUE, break.time.by = 90)
  
  plot_data <- Fit_data$plot$data
  table_data <- Fit_data$table$data
  
  ggplot( data = plot_data, aes( x = time, y = surv * 1000, color = first_bill_drug)) +
    # geom_point( size = 0.001) +
    # geom_line( ) +
    geom_step( size = 0.35) +
    annotate(geom="text", x= c(300, 300), y=c(6,13.5), label= c("Codeine", "Tramadol"), size  = 2, family = "sans", hjust = 0)+
    annotate(geom="text", x= c(20), y=c(22.5), label= paste("HR ", 
                                                            substring( ITT_survial_summary[2,"estimate"], 1,str_locate(as.character( ITT_survial_summary[2,"estimate"]), "[.]")[,1] + 2), 
                                                            " (95%, ",
                                                            substring( ITT_survial_summary[2,"lower"], 1,str_locate(as.character( ITT_survial_summary[2,"lower"]), "[.]")[,1] + 2), 
                                                            "-",
                                                            substring( ITT_survial_summary[2,"upper"], 1,str_locate(as.character( ITT_survial_summary[2,"upper"]), "[.]")[,1] + 2), 
                                                            ")",
                                                            sep = ""), 
             
             size  = 2, family = "sans", hjust = 0)+
    scale_x_continuous( expand = c(0, 0),limits = c( 0, 370), breaks = seq( 0, 360, 90), labels = seq( 0, 12, 3))+
    scale_y_continuous( expand = c(0, 0), limits = c( 0, 25))+
    labs(x = "Months of Follow-up ",
         y = "Cumulative\nIncidence Rate, ‰")+
    scale_color_jama() +
    
    theme(
      text = element_text(family = "sans", colour = "black", size  = 6),
      axis.text = element_text(colour = "black"),
      line = element_line( size = 0.25),
      # panel.border = element_rect(fill=NA),
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey"),
      # panel.spacing = unit(0.5, "lines"),
      # plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      strip.background = element_blank(),
      # strip.text = element_blank(),
      axis.line = element_line(),
      # axis.text.x  = element_text( angle = 45, vjust = 0.5),
      # axis.title.y  = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
      legend.position = "none",
      aspect.ratio = 0.55) 
}
main_plot_func_mortality  <- function( target){
  cox_dataset_ITT <- 
    Mathced_new_user_cohort %>% 
    select( idp, first_bill_drug) %>% 
    left_join(ITT_outcomes_dataframe, by = "idp") %>% 
    filter( outcome_label == target)
  
  
  Fit_data <- npsurv(Surv(follow_up_days, outcome_occur_subject) ~ first_bill_drug, 
                     data = cox_dataset_ITT) %>% ggsurvplot(fun = function(x) {-log(x)}, risk.table = TRUE, break.time.by = 90)
  
  plot_data <- Fit_data$plot$data
  table_data <- Fit_data$table$data
  
  ggplot( data = plot_data, aes( x = time, y = surv * 1000, color = first_bill_drug)) +
    # geom_point( size = 0.001) +
    # geom_line( ) +
    geom_step( size = 0.35) +
    annotate(geom="text", x= c(300, 300), y=c(4.5,15), label= c("Codeine", "Tramadol"), size  = 2, family = "sans", hjust = 0)+
    annotate(geom="text", x= c(20), y=c(22.5), label= paste("HR ", 
                                                            substring( ITT_survial_summary[3,"estimate"], 1,str_locate(as.character( ITT_survial_summary[3,"estimate"]), "[.]")[,1] + 2), 
                                                            " (95%, ",
                                                            substring( ITT_survial_summary[3,"lower"], 1,str_locate(as.character( ITT_survial_summary[3,"lower"]), "[.]")[,1] + 2), 
                                                            "-",
                                                            substring( ITT_survial_summary[3,"upper"], 1,str_locate(as.character( ITT_survial_summary[3,"upper"]), "[.]")[,1] + 2), 
                                                            ")",
                                                            sep = ""), 
             
             size  = 2, family = "sans", hjust = 0)+
    scale_x_continuous( expand = c(0, 0),limits = c( 0, 370), breaks = seq( 0, 360, 90), labels = seq( 0, 12, 3))+
    scale_y_continuous( expand = c(0, 0), limits = c( 0, 25))+
    labs(x = "Months of Follow-up ",
         y = "Cumulative\nIncidence Rate, ‰")+
    scale_color_jama() +
    
    theme(
      text = element_text(family = "sans", colour = "black", size  = 6),
      axis.text = element_text(colour = "black"),
      line = element_line( size = 0.25),
      # panel.border = element_rect(fill=NA),
      panel.background = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey"),
      # panel.spacing = unit(0.5, "lines"),
      # plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
      strip.background = element_blank(),
      # strip.text = element_blank(),
      axis.line = element_line(),
      # axis.text.x  = element_text( angle = 45, vjust = 0.5),
      # axis.title.y  = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
      legend.position = "none",
      aspect.ratio = 0.55) 
  
}


main_plot_composite_CVD <- main_plot_func_CVD( target = "composite_CVD") %>% ggplotGrob()
main_plot_Fracture <- main_plot_func_fracture( target = "fracturas") %>% ggplotGrob()
main_plot_mortality <- main_plot_func_mortality( target = "all_cause_mortality") %>% ggplotGrob()


plot_func <- function(){

  # g <- ggplotGrob(main_plot)
  
  grid.newpage()
  
  widths <- unit(c(7.5, 1, 7.5), c("cm", "cm", "cm"))
  heights <- unit(c(1, 4.5, 0.8, 1.5, 4.5, 0.8), c("cm", "cm", "cm", "cm", "cm", "cm"))
  lay <- grid.layout(nrow = 6, 
                     ncol = 3, 
                     widths=widths,
                     heights = heights)
  
  vp <- viewport( width = unit(16, units = "cm"),
                  height = unit( 18, units = "cm"),
                  layout = lay)
  pushViewport(vp)
  # grid.rect(gp=gpar(col=NA, fill="grey80"))
  
  CVD_part <- function(){
    pushViewport(viewport(layout.pos.row=1,
                          layout.pos.col=1))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,1,0,.5)))
    grid.rect(x=unit(0, "cm"), y=unit(0.35, "cm"),
              width=unit(0.25, "cm"), height=unit(0.25, "cm"),
              just=c("left", "bottom"),
              gp=gpar(col= "black"))
    
    grid.text(" A   Composite CVD events",
              x=unit(0, "cm"), y=unit(0.4, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    # upViewport()
    # pushViewport(viewport(layout.pos.row=3,
    #                       layout.pos.col=2))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))
    
    upViewport()
    pushViewport(viewport(layout.pos.row=2,
                          layout.pos.col=1))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))
    grid.draw(main_plot_composite_CVD)
    upViewport()
    pushViewport(viewport(layout.pos.row=3,
                          layout.pos.col=1))
    
    grid.text("No. at risk",
              x=unit(0, "cm"), y=unit(0.6, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    grid.text("Tramadol",
              x=unit(0, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    grid.text("Codeine",
              x=unit(0, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    #codeine 
    grid.text(main_table_CVD$n.risk[1],
              x=unit(1, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[2],
              x=unit(2.3, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[3],
              x=unit(3.8, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[4],
              x=unit(5.3, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[5],
              x=unit(6.8, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    #tramadol 
    grid.text(main_table_CVD$n.risk[6],
              x=unit(1, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[7],
              x=unit(2.3, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[8],
              x=unit(3.8, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[9],
              x=unit(5.3, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    
    grid.text(main_table_CVD$n.risk[10],
              x=unit(6.8, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    upViewport()
  }
  CVD_part()
  
  Fracture_part <- function(){
    pushViewport(viewport(layout.pos.row=1,
                          layout.pos.col=3))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,1,0,.5)))
    grid.rect(x=unit(0, "cm"), y=unit(0.35, "cm"),
              width=unit(0.25, "cm"), height=unit(0.25, "cm"),
              just=c("left", "bottom"),
              gp=gpar(col= "black"))

    grid.text(" B   Fracture",
              x=unit(0, "cm"), y=unit(0.4, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    # upViewport()
    # pushViewport(viewport(layout.pos.row=3,
    #                       layout.pos.col=2))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))

    upViewport()
    pushViewport(viewport(layout.pos.row=2,
                          layout.pos.col=3))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))
    grid.draw(main_plot_Fracture)
    upViewport()
    pushViewport(viewport(layout.pos.row=3,
                          layout.pos.col=3))

    grid.text("No. at risk",
              x=unit(0, "cm"), y=unit(0.6, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    grid.text("Tramadol",
              x=unit(0, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    grid.text("Codeine",
              x=unit(0, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    #codeine
    grid.text(main_table_Fracture$n.risk[1],
              x=unit(1, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[2],
              x=unit(2.3, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[3],
              x=unit(3.8, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[4],
              x=unit(5.3, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[5],
              x=unit(6.8, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    #tramadol
    grid.text(main_table_Fracture$n.risk[6],
              x=unit(1, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[7],
              x=unit(2.3, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[8],
              x=unit(3.8, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[9],
              x=unit(5.3, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_Fracture$n.risk[10],
              x=unit(6.8, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    upViewport()
  }
  Fracture_part()

  mortality_part <- function(){
    pushViewport(viewport(layout.pos.row=4,
                          layout.pos.col=1))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,1,0,.5)))
    grid.rect(x=unit(0, "cm"), y=unit(0.35, "cm"),
              width=unit(0.25, "cm"), height=unit(0.25, "cm"),
              just=c("left", "bottom"),
              gp=gpar(col= "black"))

    grid.text(" C   All-cause Mortality",
              x=unit(0, "cm"), y=unit(0.4, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    # upViewport()
    # pushViewport(viewport(layout.pos.row=6,
    #                       layout.pos.col=2))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))

    upViewport()
    pushViewport(viewport(layout.pos.row=5,
                          layout.pos.col=1))
    # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))
    grid.draw(main_plot_mortality)
    upViewport()
    pushViewport(viewport(layout.pos.row=6,
                          layout.pos.col=1))

    grid.text("No. at risk",
              x=unit(0, "cm"), y=unit(0.6, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    grid.text("Tramadol",
              x=unit(0, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    grid.text("Codeine",
              x=unit(0, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    #codeine
    grid.text(main_table_mortality$n.risk[1],
              x=unit(1, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[2],
              x=unit(2.3, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[3],
              x=unit(3.8, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[4],
              x=unit(5.3, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[5],
              x=unit(6.8, "cm"), y=unit(0, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    #tramadol
    grid.text(main_table_mortality$n.risk[6],
              x=unit(1, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[7],
              x=unit(2.3, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[8],
              x=unit(3.8, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[9],
              x=unit(5.3, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))

    grid.text(main_table_mortality$n.risk[10],
              x=unit(6.8, "cm"), y=unit(0.3, "cm"),
              just=c("left", "bottom"),
              gp = gpar( fontsize = 6))
    upViewport()
  }
  mortality_part()
  # 
  # mortality <- function(){
  #   pushViewport(viewport(layout.pos.row=4,
  #                         layout.pos.col=3))
  #   # grid.rect(gp=gpar(col=NA, fill=rgb(1,1,0,.5)))
  #   grid.rect(x=unit(0, "cm"), y=unit(0.35, "cm"),
  #             width=unit(0.25, "cm"), height=unit(0.25, "cm"),
  #             just=c("left", "bottom"),
  #             gp=gpar(col= "black"))
  #   
  #   grid.text(" D   All-cause Mortality",
  #             x=unit(0, "cm"), y=unit(0.4, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   # upViewport()
  #   # pushViewport(viewport(layout.pos.row=6,
  #   #                       layout.pos.col=2))
  #   # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))
  #   
  #   upViewport()
  #   pushViewport(viewport(layout.pos.row=5,
  #                         layout.pos.col=3))
  #   # grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))
  #   grid.draw(main_plot_mortality)
  #   upViewport()
  #   pushViewport(viewport(layout.pos.row=6,
  #                         layout.pos.col=3))
  #   
  #   grid.text("No. at risk",
  #             x=unit(0, "cm"), y=unit(0.6, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   grid.text("Tramadol",
  #             x=unit(0, "cm"), y=unit(0.3, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   grid.text("Codeine",
  #             x=unit(0, "cm"), y=unit(0, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   #codeine 
  #   grid.text(main_table_mortality$n.risk[1],
  #             x=unit(1, "cm"), y=unit(0, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[2],
  #             x=unit(2.3, "cm"), y=unit(0, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[3],
  #             x=unit(3.8, "cm"), y=unit(0, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[4],
  #             x=unit(5.3, "cm"), y=unit(0, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[5],
  #             x=unit(6.8, "cm"), y=unit(0, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   #tramadol 
  #   grid.text(main_table_mortality$n.risk[6],
  #             x=unit(1, "cm"), y=unit(0.3, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[7],
  #             x=unit(2.3, "cm"), y=unit(0.3, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[8],
  #             x=unit(3.8, "cm"), y=unit(0.3, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[9],
  #             x=unit(5.3, "cm"), y=unit(0.3, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   
  #   grid.text(main_table_mortality$n.risk[10],
  #             x=unit(6.8, "cm"), y=unit(0.3, "cm"),
  #             just=c("left", "bottom"),
  #             gp = gpar( fontsize = 6))
  #   upViewport()
  # }
  # mortality()
  
  
  
  
  
}

png(file = 'Figures/survival_curve.png',
     units = "cm",
     width = 16,
     height = 14,
     res = 300)

# pdf(file = 'Figures/survival_curve.pdf',
#      width = 6.4,
#      height = 6.4)

plot_func()

dev.off()








# table_plot <- 
#   ggplot( data = table_data, aes( x= time, y= first_bill_drug, label = n.risk))+
#   geom_text( size  = 8*5/15, hjust = 0) +
#   scale_x_continuous(expand = c(0,0),limits = c( 0, 395))+
#   scale_y_discrete(expand = c(0,0.1), labels = c( "Codeine", "Tramadol"))+
# 
#   theme(text = element_text(family = "sans", colour = "black", size  = 8),
#         axis.text = element_text(colour = "black", size  = 8),
#         axis.ticks = element_blank())


  

#   g2 <- ggplotGrob(main_plot)
#   g3 <- ggplotGrob(table_plot)
#   g <- rbind(g2, g3, size = "first")
#   g$widths <- unit.pmax(g2$widths, g3$widths)
#   grid.newpage()
#   grid.draw(g)
#   
  
# Numbers needed to harm --------------------------------------------------
cox_absolute_risk_ITT <- 
  Mathced_new_user_cohort %>% 
  left_join(ITT_outcomes_dataframe, by = "idp") %>% 
  filter( outcome_label %in% c( "myocardial_infarction", "stroke", "fracturas", "all_cause_mortality"), group_label == "one_year_outcome_dataset") %>% 
  mutate( age_group = case_when( initiation_age >=18 & initiation_age < 40 ~ "18-39",
                                 initiation_age >=40 & initiation_age < 60 ~ "40-59",
                                 initiation_age >=60 & initiation_age < 80 ~ "60-79",
                                 initiation_age >=80 ~ ">=80")) %>% 
  mutate( group_label = paste( sex, age_group, sep = "_"))


table(cox_interraction_ITT$group_label)

summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( outcome_label, group_label, first_bill_drug) %>% 
    summarise(n = n(), 
              # mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days))) %>% 
    group_by(outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow /365) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    mutate( diff = rate[2] - rate[1]) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>% 
    mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2),
            diff = substring(diff, 1,str_locate(as.character(diff), "[.]")[,1] + 2))
  
  return(summary_table)
}
ITT_RD_event <- summary_table_func( inputdata = cox_absolute_risk_ITT)



aa <- 
  ITT_RD_event %>% 
  ungroup() %>% 
  filter( outcome_label == "stroke", first_bill_drug ==0) 
  mutate( group_label = factor( group_label, levels = c("18-39", "40-59", "60-79", ">=80")))

ggplot( data = aa, aes( x = group_label, y = dif))+
  geom_point() +
  # scale_y_sqrt( expand= c(0, 0),breaks = c( 0, 10, 100, 250, 500, 1000, 2000), limits = c(0, 2000)) +
  # scale_y_continuous( )+
  
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
    legend.position = "none",
    aspect.ratio = 0.8) 


# Prepare tables for the manuscripts --------------------------------------
#==================================================================#
# Table xx. Primary and exploratory outcomes among matched tramadol and codeine initiators over the next yeara. 
#==================================================================#
Table2 <- 
  ITT_survial_summary %>% 
  select( outcome_label,  events.y,  rate.y, events.x,rate.x, dif_CI.y, CI,   P_value_format) %>% 
  mutate( outcome_label = factor( outcome_label, levels = c("composite_CVD", "fracturas",  "all_cause_mortality",
                                                              "falls", "constipation",  "sleep_disorders", "delirium", "opioid_abuse"))) %>% 
  arrange( outcome_label)


Table2 <- 
  CVD_fracture_survial_summary %>% 
  select( group_label,  events.y,  rate.y, events.x,rate.x, dif_CI.y, CI, P_value_format) %>% 
  mutate( group_label = factor( group_label, levels = c("stroke" , "MI",  "heart_failure"))) %>% 
  arrange( group_label)



Table2 <- 
  MPR_survial_summary %>% 
  select( outcome_label, group_label, n.y, events.y,  rate.y, n.x, events.x,rate.x, dif_CI.y, CI, P_value_format) %>% 
  mutate( group_label = factor( group_label, levels = c("env_1" , "env_2",  "env_3"))) %>% 
  arrange( outcome_label, group_label)






# Forest plot -------------------------------------------------------------

  ggplot(data=aa, aes(x=variable, y=estimate, ymin=lower, ymax=upper)) +
  geom_pointrange(shape=20, size = 0.5)+
  geom_hline(yintercept = 1, color = "red", linetype="longdash") +
  coord_flip() +
  labs( x = "Outcome",
        y = "Hazard ratio (95% CI)")+
  
  theme(
    text = element_text(family = "Candara", colour = "black"),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_blank(),
    # strip.text = element_blank(),
    
    axis.line = element_line(),
    axis.text.y  = element_text( face = "bold"),
    axis.title.y  = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
    
    legend.position = "none") 

ggsave(filename = "Forest.png",
       path = "Figures",
       plot = Forest,
       width = 5.5,
       height = 5.5,
       dpi = 300,
       type = "cairo")



# Standard difference before and after plot -------------------------------
plot_level <- 
    ## Vector of variables to summarize
    c(
      # "Demographics",
      "initiation_age",  "sex", "economic_level", "rural", 
      # lifestyle factors
      # "Lifestyle_factors",
      "BMI_value", 
      # medical conditions
      # "Medical_conditions",
      "cancer",
      "back_pain","neck_pain", 
      "peripheral_vascular_disease", "cardiac_arrhythmia",  "angina", "tia", 
      
      "oa", "osteoporosis", "fybromialgia","rheumatoid_arthritis","other_musculskeletal_disorders",

      "diabetes","chronic_liver_disease","chronic_kidney_disease", "cough", "dyspnea","pulmonary_oedema","diarrhoea", 
      "malabsorption_disorder",  "copd", "neurologic_pathologies", "parkinson_disease","alzheimer_disease", "burn_injuries", "surgery", "traffic", 
      #CCI
      "cci_group", 
      #medications
      # "Concomitant_medication",
     
      "hypnotics","benzodiazepines", "SSIR",  "anticonvulsant", 
      
      "Naproxeno","Diclofenaco","Ibuprofeno", "Celecoxib","NSAID","Paracetamol", "Metamizole", "fentanyl", "morphine",
      
      #health utilisation
      # "Health_care_utilization",
      "GP_visits", "HP_admissions")


SMD_calculate <- function( input_data, X) {
  intermediate_data <- 
    input_data %>% 
    select( first_bill_drug, X) %>% 
    as.data.frame() %>% 
    stddiff.numeric( gcol = 1, vcol = 2)
  
  value <- (intermediate_data[ ,"mean.t"] -intermediate_data[ ,"mean.c"]) / intermediate_data[ ,"sd.c"]
  names(value) <- X
  return(value)
}


before_smd <- 
  map( plot_level, SMD_calculate, input_data = MSK_new_user_cohort) %>% 
  unlist() %>% 
  as.tibble( rownames = "covariates")

after_smd <- 
  map( plot_level, SMD_calculate, input_data = Mathced_new_MSK_user_cohort) %>% 
  unlist() %>% 
  as.tibble( rownames = "covariates")

sd_plot_dataset <- 
  before_smd %>% 
  left_join( after_smd, by = "covariates") %>% 
  mutate( covariates = factor( covariates, levels = plot_level))

sd_plot <- 
  ggplot( data = sd_plot_dataset ) +
  geom_point( aes(x = value.x , y = covariates), shape = 1, size = 1.5) +
  geom_point( aes(x = value.y , y = covariates, color = "red"), shape = 17, size = 1.5) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_vline(xintercept = -0.1, color = "grey", linetype="longdash", size = 0.5) +
  geom_vline(xintercept = 0.1, color = "grey", linetype="longdash", size = 0.5) +
  scale_x_continuous(limits = c( -0.6, 0.6),
                     breaks =  round( seq( -0.6, 0.6, 0.1), digits = 1))+
  scale_y_discrete(limits = rev(sd_plot_dataset$covariates)) +
  
  labs(x = "\nStandardised mean difference",
       y = "Covariates")+
  
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
    
    legend.position = "none") 


  
ggsave(filename = "MSK_indication_sd_plot.png",
       path = "Figures",
       plot = sd_plot,
       width = 5.5,
       height = 7,
       dpi = 300,
       type = "cairo")





# Save plots --------------------------------------------------------------
ggsave(filename = "trend_plot.png",
       path = "Figures",
       plot = trend_plot,
       width = 12,
       height = 8,
       units = "cm",
       type = "cairo")



ggsave(filename = "main_plot_Fracture.png",
       path = "Figures",
       plot = main_plot_Fracture,
       width = 8,
       height = 6,
       dpi = 300,
       type = "cairo")

ggsave(filename = "Distribution_after_matching.png",
       path = "Figures",
       plot = Distribution_after_matching,
       width = 8,
       height = 6,
       dpi = 300,
       type = "cairo")

