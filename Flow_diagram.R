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
install.packages("tableone")
install.packages("MatchIt")
install.packages("hrbrthemes") #https://cran.r-project.org/web/packages/hrbrthemes/hrbrthemes.pdf
install.packages("installr") #Updating R/RStudio: https://uvastatlab.github.io/phdplus/installR.html#updateR
install.packages("optmatch")
install.packages('devtools')
install.packages("fastDummies")
install.packages("cobalt")
devtools::install_github('neilstats/ckbplotr')


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
library(readr)
library(tidyr)
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
library( cobalt)
# library(fastDummies)

getwd()
setwd("D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research")
# load raw and label datasets -------------------------------------------------------
load("D:/DPhil/Project_Opioid_use/Data/billing.RData")
load("D:/DPhil/Project_Opioid_use/Data/diagnosis.RData")
load("D:/DPhil/Project_Opioid_use/Data/demography.RData")
load("D:/DPhil/Project_Opioid_use/Data/social_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/clinical_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/admission.RData")
Denominator_data <- read_delim("D:/DPhil/Data_raw/OPIODU/OPIOIDES_entregable_poblacio_denominadors_20191219_143606.txt", 
                               delim = "|",
                               col_names = TRUE)

#dictionary datasets
Dic_analgesics <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_analgesics.xlsx")
Dic_history_cancer <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_history_cancer.xlsx")
Dic_CER_adverse_events <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_CER_adverse_events.xlsx")
Dic_history_medication <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_history_medication.xlsx")
Dic_commorbidity <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_commorbidity.xlsx")

#prepared datasets
# load("R_datasets/baseline_cohort_100.RData")
# load("R_datasets/follow_up_dateset.RData")

# set.seed(1)
# baseline_cohort <- sample_frac(baseline_cohort_100, 0.1)

# Little explore with raw dta ---------------------------------------------
head(billing)
str(billing)
glimpse(billing)
glimpse(diagnosis)
# check missing of each variable (no missing)
sapply(diagnosis, function(x)sum(is.na(x)))
sapply(social_variables, function(x)sum(is.na(x)))
sapply(clinical_variables, function(x)sum(is.na(x)))
sapply(admission, function(x)sum(is.na(x)))

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

# database population -----------------------------------------------------
# set.seed(1)
# sub_Denominator <- sample_frac(Denominator_data, 0.1)
#=======================================================#
# all registered subjects with billing data after 2007
#=======================================================#
database_population_fentanyl <- 
    Denominator_data %>% 
    select( idp) %>% 
    left_join( select( billing, -billing_agr), by = "idp") %>% 
    left_join( select( Dic_analgesics, ATC_code, Specific_drug), by = c("billing_cod" = "ATC_code")) %>% 
    # filter studied drugs and observation period
    filter( !is.na(Specific_drug), Specific_drug %in% c("tramadol", "fentanyl"), bill_date >= as.Date("2007-01-01")) %>% 
    group_by( idp) %>% 
    arrange( bill_date) %>% 
    mutate( seq = row_number(), total_seq = n()) %>% # create two index
    mutate( first_bill_time = bill_date[1], first_bill_drug = Specific_drug[1]) %>% 
    # index for two drugs dispensed on the same same entry day
    mutate( index_double_user = case_when(any(seq >= 2 & first_bill_time == bill_date & Specific_drug != first_bill_drug) ~ 1,
                                         TRUE ~ 0)) %>% 
    ungroup() %>% 
    # filter (select one billing record for each person)
    filter( seq == 1) %>% 
    ungroup()  

nrow(database_population_fentanyl)

#==================================================================#
# generate population with study durg during the look-back period 
#==================================================================#
look_back_population_fentanyl <- 
  database_population_fentanyl %>% 
  select( idp, first_bill_time) %>% 
  left_join( select( billing, -billing_agr), by = "idp") %>% 
  left_join( select( Dic_analgesics, ATC_code, Specific_drug), by = c("billing_cod" = "ATC_code")) %>% 
  filter( !is.na(Specific_drug), Specific_drug %in% c("tramadol", "fentanyl")) %>% 
  mutate( look_back_date = first_bill_time - 365) %>% 
  filter( bill_date >= look_back_date, bill_date < first_bill_time) %>% 
  distinct( idp) %>% 
  mutate( look_back_index = 1)


database_population_fentanyl <- 
  database_population_fentanyl %>% 
  left_join( look_back_population_fentanyl, by = "idp") %>% 
  mutate( look_back_index = case_when( look_back_index == 1 ~ 1,
                                       TRUE ~ 0))


# study population --------------------------------------------------------

#==================================================================#
# Aged >= 18 years old on the date of first dispensation (entry date) of studied drugs 
#==================================================================#
study_population_stage1_fentanyl <- 
  database_population_fentanyl %>% 
  left_join( demography, by = "idp") %>% 
  #important: check if there are missing values for demographics variables
  # sapply(function(x)sum(is.na(x)))
  #index for intiation age and continuous register duration
  mutate( initiation_age = as.numeric( difftime(first_bill_time, date_of_birth, units = "days")/365 ),
          initiation_gap = as.numeric( difftime(first_bill_time, entry_date, units = "days")/365)) %>% 
  # filter age >= 18
  filter( initiation_age >= 18) 
  
#==================================================================#
# No cancer previous or at the time of the entry date
#==================================================================#
study_population_stage2_fentanyl <- 
  study_population_stage1_fentanyl %>% 
  left_join( select(check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>% 
  left_join( Dic_history_cancer, by = "dia_cod") %>% 
  left_join( Dic_CER_adverse_events, by = "dia_cod") %>% 
  group_by( idp) %>% 
  #index for previous cancer event
  mutate( history_cancer = case_when( any( !is.na(cancer_label) & dia_start_date <= first_bill_time ) ~ 1,
                                              TRUE ~ 0)) %>% 
  mutate( history_outcomes = case_when( any( !is.na(Disease_Group) & dia_start_date <= first_bill_time ) ~ 1,
                                              TRUE ~ 0)) %>% 
  ungroup() %>% 
  # filter subjects with cancer
  filter( history_cancer == 0) %>% 
  group_by( idp) %>% 
  filter( row_number() == 1) %>% 
  select( -dia_cod, -dia_start_date, -cancer_label, -Disease_Group, -Disease_Specific ) %>% 
  ungroup()

nrow(study_population_stage2_fentanyl)



# Finial cohort -----------------------------------------------------------
#==================================================================#
# Continuous enrolment in the database < 1 year before the entry date 
#==================================================================#
Final_cohort_stage1_fentanyl <- 
  study_population_stage2_fentanyl %>% 
  filter( initiation_gap >= 1)

nrow(study_population_stage2_fentanyl) - nrow(Final_cohort_stage1_fentanyl) 

#==================================================================#
# withou dispensation one year before index date
#==================================================================#
Final_cohort_stage2_fentanyl <- 
  Final_cohort_stage1_fentanyl %>% 
  filter(  look_back_index == 0) 

nrow(Final_cohort_stage1_fentanyl) - nrow(Final_cohort_stage2_fentanyl) 


#==================================================================#
# Dispensed both tramadol and codeine on the entry date 
#==================================================================#
Final_cohort_stage3_fentanyl <- 
  Final_cohort_stage2_fentanyl %>% 
  filter( index_double_user == 0) 

nrow(Final_cohort_stage2_fentanyl) - nrow(Final_cohort_stage3_fentanyl) 
#==================================================================#
# No outcomes of interest previous or at the time of the entry date
#==================================================================#
Final_cohort_100_fentanyl <- 
  Final_cohort_stage3_fentanyl %>% 
  filter( history_outcomes == 0) 

nrow(Final_cohort_stage3_fentanyl) - nrow(Final_cohort_100_fentanyl) 
nrow(Final_cohort_100_fentanyl) 

table( Final_cohort_100_fentanyl$first_bill_drug)

save(Final_cohort_100_fentanyl, file="R_datasets/Final_cohort_100_fentanyl.RData")



# Link to other baseline variables ----------------------------------------
baseline_cohort_100_fentanyl <- 
  Final_cohort_100_fentanyl %>% 
  left_join( social_variables, by = "idp") %>%
  left_join( BMI_dataset, by = "idp") %>%
  
  #==================================================================#
  # deal with missing variables: economic_level, rural 
  #==================================================================#
  
  mutate(economic_level = case_when(economic_level == "" ~ "Ms",
                                    TRUE ~ economic_level),
         rural = case_when(rural == "" ~ "Ms",
                           TRUE ~ rural)) %>%
  mutate(BMI_record_gap = as.numeric( difftime(first_bill_time, clinical_date, units = "days"))) %>%
  
  #==================================================================#
  # index for eligiable records of BMI (6 months before index date)
  #==================================================================#

  mutate( BMI_index_records = case_when( BMI_record_gap >= 0 & BMI_record_gap <= 365 ~ 1,
                     TRUE ~ 0)) %>%
  group_by(idp) %>%
  arrange( desc(BMI_index_records), BMI_record_gap) %>%
  mutate( BMI_value = case_when(any(BMI_index_records == 1) ~ val[1],
          TRUE ~ NA_real_)) %>%
  ungroup() %>%
  distinct( idp, sex, first_bill_time, first_bill_drug, initiation_age,
            economic_level, rural,
            BMI_value) %>%

  #==================================================================#
  #  GP visits one year before cohort entry
  #==================================================================#
  left_join( select( admission, idp, admission_cod, admission_date), by = "idp") %>% 
  mutate( admission_record_gap = as.numeric( difftime(first_bill_time, admission_date, units = "days"))) %>% 
  mutate( admission_index_records_10999 = case_when( admission_record_gap >= 0 & admission_record_gap <= 365 & admission_cod == 10999 ~ 1,
                                                     TRUE ~ 0)) %>%
  group_by(idp) %>%
  mutate( admission_times_10999 = case_when(any(admission_index_records_10999 == 1) ~ sum(admission_index_records_10999),
          TRUE ~ 0)) %>%
  distinct(idp, sex, first_bill_time, first_bill_drug, initiation_age,
           economic_level, rural,
           BMI_value,
           admission_times_10999) %>%
  ungroup() %>% 
  
  #==================================================================#
  #  hospital  visits one year before cohort entry
  #==================================================================#
  left_join( select( admission, idp, admission_cod, admission_date), by = "idp") %>% 
  mutate( admission_record_gap = as.numeric( difftime(first_bill_time, admission_date, units = "days"))) %>% 
  mutate( admission_index_records_30999 = case_when( admission_record_gap >= 0 & admission_record_gap <= 365 & admission_cod == 30999 ~ 1,
                                                     TRUE ~ 0)) %>%
  group_by(idp) %>% 
  mutate( admission_times_30999 = case_when(any(admission_index_records_30999 == 1) ~ sum(admission_index_records_30999),
                                            TRUE ~ 0)) %>%
  distinct(idp, sex, first_bill_time, first_bill_drug, initiation_age,
           economic_level, rural,
           BMI_value,
           admission_times_10999,
           admission_times_30999) %>%
  ungroup() %>% 

#==================================================================#
# commorbidity history, use check_dup_diagnosis rather than diagnosis
#==================================================================#
  left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  left_join( Dic_commorbidity, by = "dia_cod") %>%
  # index for eligiable records of history of other disease
  mutate( commorbidities_index_records = case_when( dia_start_date <= first_bill_time & !is.na(commorbidity_label) ~ 1,
                                                    TRUE ~ 0)) %>%
  # replace unnecessary disease label with NA
  mutate( commorbidity_label = case_when(commorbidities_index_records == 1 ~ commorbidity_label,
                                         TRUE ~ "Other_or_non_commorbidities")) %>%
  # index for eligiable subjects of history of other disease
  group_by(idp) %>%
  mutate( commorbidities_index_subjects = case_when(any(commorbidities_index_records == 1) ~ 1,
                                                    TRUE ~ 0)) %>%
  ungroup() %>%
  # filter unnecessary records
  filter( !(commorbidities_index_subjects == 1 & commorbidities_index_records == 0) ) %>%
  # remove duplicate diagnosis
  distinct(idp, sex, first_bill_time, first_bill_drug, initiation_age,
           economic_level, rural,
           BMI_value,
           admission_times_10999,
           admission_times_30999,
           commorbidity_label, commorbidities_index_records) %>%
  # transform from long to wide data
  spread(commorbidity_label, commorbidities_index_records, fill = 0)
  

  
save(baseline_cohort_100_fentanyl, file="R_datasets/baseline_cohort_100_fentanyl.RData")




# Follow-up periods -------------------------------------------------------

start.time <- Sys.time()
follow_up_dataset_ATT_100 <- 
  baseline_cohort_100 %>% 
  select( idp, first_bill_time, first_bill_drug) %>%
  left_join( select( billing, -billing_agr), by = "idp") %>% 
  left_join( select( Dic_analgesics, ATC_code, Specific_drug), by = c("billing_cod" = "ATC_code")) %>% 
  # filter studied drugs and observation period
  filter( !is.na(Specific_drug), Specific_drug %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
  group_by( idp) %>% 
  arrange( bill_date) %>% 
  mutate( bill_seq = as.numeric( row_number())) %>% 
  #==================================================================#
  # cencored date for switching
  #==================================================================#
  mutate( switcher = case_when( any(first_bill_drug != Specific_drug) ~ 1,
                                        TRUE ~ 0),
          switcher_seq = case_when( first_bill_drug != Specific_drug ~ bill_seq,
                                    TRUE ~ NA_real_),
          switcher_seq_index = case_when( switcher == 1  ~ min(switcher_seq, na.rm = TRUE) - 1,
                                          TRUE ~ NA_real_),
          switch_date = case_when( switcher == 1 ~ bill_date[switcher_seq_index] + 15,
                                   TRUE ~ bill_date[n()] + 15)) %>% 
  #==================================================================#
  # cencored date for discontinuation 
  #==================================================================# 
  mutate( bill_date_diff = bill_date - lag(bill_date, default = first(bill_date))) %>% 
  mutate( discontinuationer = case_when( any(bill_date_diff > 90 ) ~ 1,
                                         TRUE ~ 0),
          discontinuation_seq = case_when( bill_date_diff > 90 ~ bill_seq,
                                           TRUE ~ NA_real_),
          discontinuation_index = case_when( any( discontinuation_seq > 0) ~ min(discontinuation_seq, na.rm = TRUE) - 1,
                                             TRUE ~ NA_real_),
          discontinuation_date = case_when( any( discontinuation_seq > 0) ~ bill_date[discontinuation_index] + 15,
                                            TRUE ~ bill_date[n()] + 15) ) %>% 
  mutate( cencored_date = pmin( switch_date, discontinuation_date)) %>% 
  ungroup( idp) %>% 
  
  #==================================================================#
  # calculate follow-up days
  #==================================================================#
  left_join( select( demography, idp, departure_date), by = "idp") %>% 
  left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  left_join( Dic_CER_adverse_events, by = "dia_cod") %>% 
  #==================================================================#
  # index for follow-up period (ATT)
  #==================================================================#
  mutate( outcome_occur_record_ATT = case_when( dia_start_date > first_bill_time & cencored_date > dia_start_date & !is.na(Disease_Specific) ~ 1,
                                            TRUE ~ 0)) %>%
  group_by( idp) %>%
  mutate( outcome_occur_subject_ATT = case_when( any(outcome_occur_record_ATT == 1) ~ 1,
                                             TRUE ~ 0)) %>%
  ungroup() %>%
  filter( !(outcome_occur_subject_ATT == 1 & outcome_occur_record_ATT == 0)) %>%
  group_by( idp) %>%
  mutate( outcome_occur_date_ATT = case_when( outcome_occur_subject_ATT == 1 ~ min(dia_start_date),
                                          TRUE ~ as.Date("2017-12-31"))) %>%
  #==================================================================#
  # condense
  #==================================================================#
  distinct( idp, first_bill_time, outcome_occur_subject_ATT, outcome_occur_date_ATT, cencored_date) %>%  
  mutate( combined_date_ATT = pmin(outcome_occur_date_ATT, cencored_date),
          follow_up_days_ATT  = as.numeric( difftime( combined_date_ATT , first_bill_time, units = "days"))) %>%
  ungroup() %>% 
  select( idp, outcome_occur_subject_ATT, follow_up_days_ATT)
end.time <- Sys.time()

end.time - start.time



outcome_list <- Dic_CER_adverse_events %>% distinct( Disease_Group) %>% unlist() %>% unname()
outcome_list


Follow_ITT_func <- function(outcomes){
  start.time <- Sys.time()
  follow_up_dataset <- 
    baseline_cohort_100_fentanyl %>% 
    select( idp, first_bill_time, first_bill_drug) %>%
    left_join( select( demography, idp, departure_date), by = "idp") %>% 
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( Dic_CER_adverse_events, by = "dia_cod") %>% 
    #==================================================================#
    # index for follow-up period (ITT)
    #==================================================================#
    mutate( outcome_occur_record_ITT = case_when( dia_start_date > first_bill_time & Disease_Group %in% outcomes ~ 1,
                                                  TRUE ~ 0)) %>% 
    group_by( idp) %>%
    mutate( outcome_occur_subject_ITT = case_when( any(outcome_occur_record_ITT == 1) ~ 1,
                                                   TRUE ~ 0)) %>% 
    ungroup() %>%
    filter( !(outcome_occur_subject_ITT == 1 & outcome_occur_record_ITT == 0)) %>% 
    group_by( idp) %>% 
    mutate( outcome_occur_date_ITT = case_when( outcome_occur_subject_ITT == 1 ~ min(dia_start_date),
                                                TRUE ~ as.Date("2017-12-31"))) %>% 
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp, first_bill_time, outcome_occur_subject_ITT, outcome_occur_date_ITT, departure_date) %>% 
    mutate( combined_date_ITT = pmin(outcome_occur_date_ITT, departure_date),
            follow_up_days_ITT  = as.numeric( difftime( combined_date_ITT , first_bill_time, units = "days"))) %>% 
    ungroup() %>% 
    select( idp, outcome_occur_subject_ITT, follow_up_days_ITT)
  end.time <- Sys.time()
  print(end.time - start.time)
  return(follow_up_dataset)
}

follow_up_dataset_ITT_100_fentanyl <- Follow_ITT_func( outcomes = outcome_list)
# 
# follow_up_dataset_ITT_100_specific <- lapply( outcome, Follow_ITT_func) 
# names(follow_up_dataset_ITT_100_specific) <- outcome
#   


  
table(follow_up_dataset_ITT_100_fentanyl$outcome_occur_subject_ITT)


# 
# sapply(follow_up_dateset_1, function(x)sum(is.na(x)))
# save(follow_up_dataset_ATT_100, file="R_datasets/follow_up_dataset_ATT_100.RData")
# save(follow_up_dataset_ITT_100, file="R_datasets/follow_up_dataset_ITT_100.RData")
 
# Missing data imputation -------------------------------------------------

summary(baseline_cohort_100_fentanyl)
sapply(baseline_cohort_100_fentanyl, function(x)sum(is.na(x)))

mice_cohort_fentanyl <- 
  baseline_cohort_100_fentanyl %>% 
  select(  -Other_or_non_commorbidities) %>% 
  mice( m = 1, method = 'cart', printFlag = FALSE)

imputation_plot <- densityplot(mice_cohort_fentanyl, ~BMI_value)
imputation_plot
baseline_cohort_imputation_fentanyl <- complete(mice_cohort_fentanyl)

save(baseline_cohort_imputation_fentanyl, file="R_datasets/baseline_cohort_imputation_fentanyl.RData")

# trellis.device(device="png", filename="Figures/imputation_plot.png")
# print(imputation_plot)
# dev.off()


# Propensity score matching (method 1)-----------------------------------------------

#==================================================================#
# # data preparation for PS modelling
#==================================================================#
PS_model_dataset <- 
  baseline_cohort_imputation %>% 
  mutate( first_bill_drug = case_when(first_bill_drug == "tramadol" ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(sex = as.factor(sex),
         economic_level = as.factor(economic_level),
         rural = as.factor(rural),
         Alzheimer_disease = as.factor(Alzheimer_disease),
         Chronic_cough = as.factor( Chronic_cough),
         Chronic_kidney_disease = as.factor( Chronic_kidney_disease),
         Chronic_liver_disease = as.factor( Chronic_liver_disease),
         Chronic_musculoskeletal_pain_disorders = as.factor( Chronic_musculoskeletal_pain_disorders),
         COPD = as.factor( COPD),
         Diabetes = as.factor( Diabetes),
         Parkinson_disease = as.factor( Parkinson_disease),
         Peripheral_vascular_disease = as.factor( Peripheral_vascular_disease)) 

str(PS_model_dataset)

covariates <- setdiff( names(PS_model_dataset), c( "idp", "first_bill_time", "first_bill_drug"))
dependent_variable <- "first_bill_drug"


mylogit <- glm( reformulate(termlabels = covariates, response = dependent_variable), 
                data = PS_model_dataset, 
                family = "binomial")
summary(mylogit)


score_before_matching <- data.frame( first_bill_drug = mylogit$y, 
                                     pscore= mylogit$fitted.values)

plot_distribution <- function(dataset){
  
  plot <- 
    ggplot( ) +
    # scale_x_continuous( limits = c( 0, 1.5), breaks = seq( 0, 1, 0.2)) +
    # scale_y_continuous( limits = c( -20, 20), breaks = seq( -5, 5, 2.5)) +
    # Top
    geom_histogram( data = filter( dataset, first_bill_drug == 1),  aes(x = pscore, y = ..density..),  fill="#69b3a2" ,bins = 100) +
    geom_label( aes(x= 0.6, y= 2.5, label="Tramadol cohort"), color="#69b3a2") +
    # Bottom
    geom_histogram( data = filter( dataset, first_bill_drug == 0),  aes(x = pscore, y = -..density..), fill= "#404080", bins = 100) +
    geom_label( aes(x= 0.6, y= -2.5, label="Fentanyl cohort"), color="#404080") +
    
    labs( x = "Score",
          y = "Frequency") +
    
    theme_ipsum() +
    theme(aspect.ratio = 0.66)
  
  return(plot)
  
  
}
Distribution_before_matching_fentanyl <- plot_distribution(dataset = score_before_matching)
Distribution_after_matching_fentanyl <- plot_distribution(dataset = score_after_matching)
Distribution_after_matching_fentanyl

set.seed(1)
test_data_sub <- sample_frac(PS_model_dataset, 0.2)
str(test_data_sub)

#==================================================================#
# propensity score modelling
#==================================================================#
PS_model_mahalanobis_fentanyl <- matchit( reformulate(termlabels = covariates, response = dependent_variable), 
                              # m.order = "smallest",
                              method = "nearest",
                              distance = "mahalanobis",
                              # caliper = 0.2,
                              ratio=1,
                              data = PS_model_dataset)
PS_model_unweight_fentanyl <- PS_model
save(PS_model_unweight_fentanyl, file="R_datasets/PS_model_unweight_fentanyl.RData")

# summary(PS_model, standardize=TRUE)
sd_data <- bal.tab(PS_model_mahalanobis_fentanyl, binary = "std", un = TRUE)
sd_data
#==================================================================#
# PS score visualisation
#==================================================================#
plot(PS_model, type = 'jitter', interactive = FALSE)


#index for matched rows
index <- 
  data.frame(control =  rownames(PS_model$match.matrix), treat = PS_model$match.matrix[,1] ) %>% 
  filter( !is.na(treat)) %>% 
  mutate( control = as.character(control), treat = as.character(treat)) %>% 
  gather( condition, index, control, treat) %>% 
  mutate( index = as.numeric( index)) %>% 
  select( index) %>% 
  unlist()

  

score_before_matching <- data.frame( first_bill_drug = PS_model$model$y, 
                                         pscore= PS_model$model$fitted.values)

score_after_matching <- data.frame( first_bill_drug = PS_model$model$y[index], 
                                         pscore = PS_model$model$fitted.values[index])



Distribution_after_matching <- plot_distribution(dataset = score_after_matching)
Distribution_after_matching

#==================================================================#
# PS mactched cohort
#==================================================================#
mathched_cohort <- 
  PS_model_dataset%>% 
  filter( row_number() %in% index)

# Calculate statistics ( mean follow up, rate, crude HR) ------------------------------------------
cox_dataset_ITT <- 
  mathched_cohort %>% 
  left_join(follow_up_dataset_ITT_100_fentanyl, by = "idp")


bb <- 
  cox_dataset_ITT %>% 
  group_by( first_bill_drug) %>% 
  summarise(n = n(), 
            mean_age = mean( initiation_age), 
            events = sum( outcome_occur_subject_ITT), 
            mean_follow = as.numeric( mean(follow_up_days_ITT)/365)) %>% 
  ungroup() %>% 
  mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
  mutate( ratio = rate[2] / rate[1])


# Cox-model ---------------------------------------------------------------
cox_model <- coxph( Surv(follow_up_days_ITT, outcome_occur_subject_ITT) ~ first_bill_drug ,
                    data =  cox_dataset_ITT)
summary(cox_model)

set.seed(1)
cc <- sample_n(cox_dataset, 1000, replace = FALSE)

plot_list <- ggadjustedcurves(cox_model, data = cc, method = "average", variable = "first_bill_drug")  
plot_data <- plot_list$data
plot_data$surv <- (1 - (plot_data$surv))


names(cox_dataset)

# Cox-model stratification ------------------------------------------------
cox_dataset <- 
  mathched_cohort %>% 
  left_join(follow_up_dateset, by = "idp") %>% 
  mutate( age_group = case_when( initiation_age >= 18 & initiation_age <49 ~ 1,
                                 initiation_age >= 49 & initiation_age <69 ~ 2,
                                 initiation_age >=69  ~ 3))

table(cox_dataset$age_group)


res.separate <- lapply( split(cox_dataset, cox_dataset$age_group),
                        FUN = function(DF) {
                           coxph( Surv(follow_up_time, outcome_occur_subject) ~ 
                                  first_bill_drug ,
                                  data =  DF)
                       })


res.separate

summary(res.separate$H)
summary(res.separate$D)

summary(res.separate$`1`)
summary(res.separate$`2`)
summary(res.separate$`3`)





# Forest plot -------------------------------------------------------------
set.seed(57911624)
exampleresults <- function(){
  data.frame(variable = c('Male', 'Female', 'Age 18~49', "Age 50~69", "Age >=70"),
             estimate = c(0.3033, 0.36855, 0.4025, 0.29121, 0.42028),
             stderr   = c(0.05501, 0.04269, 0.08122, 0.05509, 0.05025),
             n        = round(runif(5, 100, 2000)),
             nb       = round(runif(5, 100, 2000)))
}
resultsA <- exampleresults()

forestplot <- make_forest_plot(cols         = list(resultsA),
                               exponentiate = TRUE,
                               colnames     = c("Subgroups"),
                               col.key      = "variable",
                               xlim = c(0.99, 2))

forestplot





# Standard difference before and after plot -------------------------------
sd_plot_dataset <- 
  sd_data$Balance %>% 
  select( Type, Diff.Un, Diff.Adj) %>% 
  tibble::rownames_to_column( var = "variable_names") %>% 
  filter( variable_names != "distance") %>% 
  mutate( Diff.Un = Diff.Un * 100, Diff.Adj = Diff.Adj * 100) %>% 
  mutate( variable_names = factor( variable_names,
                                      levels = c( "sex_H",
                                                  "initiation_age",
                                                  "economic_level_Ms",
                                                  "economic_level_U1",
                                                  "economic_level_U2",
                                                  "economic_level_U3",
                                                  "economic_level_U4",
                                                  "economic_level_U5",
                                                  "rural_Ms",
                                                  "rural_R",
                                                  "rural_U",
                                                  "BMI_value",
                                                  "Alzheimer_disease",
                                                  "Chronic_cough",
                                                  "Chronic_kidney_disease",
                                                  "Chronic_liver_disease",
                                                  "Chronic_musculoskeletal_pain_disorders",
                                                  "COPD",
                                                  "Diabetes",
                                                  "Parkinson_disease",
                                                  "Peripheral_vascular_disease",
                                                  "admission_times_10999",
                                                  "admission_times_30999")))



sd_plot <- 
  ggplot( data = sd_plot_dataset ) +
  geom_point( aes(x = Diff.Un , y = variable_names), shape = 1, size = 2) +
  geom_point( aes(x = Diff.Adj , y = variable_names, color = "red"), shape = 17, size = 2.5) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_vline(xintercept = -10, color = "grey", linetype="longdash", size = 1) +
  geom_vline(xintercept = 10, color = "grey", linetype="longdash", size = 1) +
  scale_x_continuous(limits = c( -100, 30),
                     breaks = seq( -100, 30, 10))+
  scale_y_discrete(limits = rev(levels(sd_plot_dataset$variable_names))) +
  
  labs(x = "\nPercentage standardised difference",
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
sd_plot

ggsave(filename = "sd_plot_fentanyl.png",
       path = "Figures",
       plot = sd_plot,
       width = 5.5,
       height = 5.5,
       dpi = 300,
       type = "cairo")





# Save plots --------------------------------------------------------------

ggsave(filename = "Distribution_before_matching_fentanyl.png",
       path = "Figures",
       plot = Distribution_before_matching_fentanyl,
       width = 8,
       height = 6,
       dpi = 300,
       type = "cairo")

ggsave(filename = "Distribution_after_matching_fentanyl.png",
       path = "Figures",
       plot = Distribution_after_matching_fentanyl,
       width = 8,
       height = 6,
       dpi = 300,
       type = "cairo")





#