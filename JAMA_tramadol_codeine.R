#### Set up library ----------------------------------------------------------
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
library(stringr)
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
# library(fastDummies)

getwd()
setwd("D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research")
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
sapply(diagnosis, function(x)sum(is.na(x)))
sapply(social_variables, function(x)sum(is.na(x)))
sapply(clinical_variables, function(x)sum(is.na(x)))
sapply(admission, function(x)sum(is.na(x)))

#make sure there is no duplicate 
check_dup_billing <- distinct(billing, idp, billing_cod, bill_date, .keep_all = TRUE) #(confirm no duplicate row)
check_dup_diagnosis <- distinct(diagnosis_complete, idp, dia_cod, dia_start_date, .keep_all = TRUE) #(confirm with duplicate row)
check_dup_social_variables <- distinct(social_variables, idp, economic_level, rural, .keep_all = TRUE) #(confirm no duplicate row)
check_dup_clinical_variables <- distinct(clinical_variables,idp,clinical_cod, clinical_date, val, .keep_all = TRUE) #(confirm no duplicate row)


# Describe dispensation trend ---------------------------------------------
Total_billing_dataset <- 
  billing_delay %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  filter( Drug_name %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
  left_join( demography, by = "idp") %>% 
  mutate( bill_age = as.numeric( difftime( bill_date, date_of_birth, units = "days")/365 )) %>% 
  filter( bill_age >= 18) %>% 
  mutate( bill_year = substring( bill_date,1,4)) 
  
trend_data <- 
  Total_billing_dataset %>% 
  mutate(Drug_name = factor(Drug_name,
                       levels = c( "tramadol", "codeine"),
                       labels = c( "Tramadol", "Codeine"))) %>% 
  mutate( bill_year = as.numeric( bill_year)) %>% 
  group_by( Drug_name, bill_year) %>% 
  summarise( num_prescription = n()) %>% 
  ungroup()

trend_plot <- 
  ggplot( trend_data, aes( x= bill_year,y= num_prescription, group = Drug_name, fill = Drug_name)) +
  geom_area( )+
  scale_fill_manual(values = c("#AD002AFF", "#FDAF91FF","#42B540FF" ,"#0099B4FF"))+
  scale_y_continuous(expand = c(0,0), limit = c(0, 500000), labels = function(x) format(x, scientific = FALSE))+
  scale_x_continuous(expand = c(0,0), breaks = c(2007:2017))+
  labs(x = "Year",
       y = "Number of presriptions\n")+
  # scale_color_lancet()+
  guides( fill = guide_legend( title = NULL,
                               keywidth = 0.8,
                               keyheight  = 0.8))+
  theme(
    text = element_text(family = "sans" ),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
    
    axis.line = element_line( size = 0.2),
    axis.ticks = element_line( size = 0.2),
    axis.text = element_text(colour = "black", size = 6),
    axis.title = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), size = 8),
    
    legend.position = c(0.15,0.9),
    legend.text = element_text( size = 6),
    aspect.ratio = 0.7) 

trend_plot


# Baseline covariates preparation -----------------------------------------
# set.seed(1)
# sub_Denominator <- sample_frac(Denominator_data, 0.1)

First_billing_dataset <- 
  Denominator_data %>% 
  select( idp) %>% 
  left_join( select( billing_delay, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  # filter studied drugs and observation period
  filter(  Drug_name %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
  arrange( bill_date) %>% 
  group_by( idp) %>%
  mutate( seq = row_number(), total_seq = n()) %>% # create two index
  mutate( first_bill_time = bill_date[1], first_bill_drug = Drug_name[1]) %>% 
  ungroup() %>% 
  # filter (select one billing record for each person)
  filter( seq == 1) %>% 
  ungroup()  

#===========================================================================#
# Demographic variables, Age(age group), sex, economic levels, residence areas
#============================================================================#
Baseline_Demographic_dataset <- 
  First_billing_dataset %>% 
  select( idp, first_bill_time) %>% 
  left_join( demography, by = "idp") %>% 
  left_join( social_variables, by = "idp") %>% 
  mutate( economic_level = case_when(economic_level == "" ~ NA_character_,
                                    TRUE ~ economic_level),
          rural = case_when(rural == "" ~ NA_character_,
                           TRUE ~ rural)) %>% 
  mutate( initiation_age = as.numeric( difftime(first_bill_time, date_of_birth, units = "days")/365 )) %>% 
  # mutate( age_group = case_when( initiation_age >= 18 & initiation_age < 40 ~ "18-39",
  #                                initiation_age >= 40 & initiation_age < 60 ~ "40-59",
  #                                initiation_age >= 60 & initiation_age < 80 ~ "60-79",
  #                                initiation_age >= 80  ~ ">=80",
  #                                TRUE ~ "<18")) %>% 
  mutate( sex = factor( sex, levels = c( "D", "H")),
          rural = factor( rural, levels = c( "U", "R")),
          economic_level = factor( economic_level, levels = c( "U1", "U2", "U3", "U4", "U5"))) %>% 
  # mutate( age_group = factor( age_group, levels = c( "18-39", "40-59", "60-79", ">=80", "<18"))) %>% 
  select( -first_bill_time, -date_of_birth, -entry_date, -situacio, -departure_date)
  
sapply(Baseline_Demographic_dataset, function(x)sum(is.na(x)))
#===========================================================================#
# lifestyle factors, BMI, smoking, drinking (looking back period: 2 years)
#============================================================================#
Baseline_Lefestyle_dataset <- 
  First_billing_dataset %>% 
  select( idp, first_bill_time) %>% 
  left_join( filter(clinical_variables, clinical_agr == "IMC"), by = "idp") %>% 
  arrange( desc(clinical_date)) %>% 
  mutate( BMI_index_records = case_when( clinical_date <= first_bill_time & clinical_date >= (first_bill_time - 365) ~ 1,
                                       TRUE ~ 0)) %>% 
  group_by(idp) %>%
  summarise( BMI_value = case_when(any(BMI_index_records == 1) ~ val[1],
                                TRUE ~ NA_real_)) %>% 
  ungroup() 
  # mutate( BMI_group = case_when( BMI_value < 18.5 ~ "Underweight",
  #                                BMI_value >= 18.5 & BMI_value < 25 ~ "Normal",
  #                                BMI_value >= 25 & BMI_value < 30 ~ "Overweight",
  #                                BMI_value >= 30  ~ "Obese",
  #                                TRUE ~ "Ms"))

sapply(Baseline_Lefestyle_dataset, function(x)sum(is.na(x)))
sapply(Baseline_Lefestyle_dataset, function(x)sum(is.na(x)/length(x)))

#===========================================================================#
# medical conditions, previous years
#============================================================================#
Baseline_medical_condition_dataset <- 
  First_billing_dataset %>% 
  select( idp, first_bill_time) %>% 
  left_join( check_dup_diagnosis, by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  rename( disease_name = English_label, disease_group = variable_group) %>% 
  # index for eligiable records of history of other disease
  mutate( commorbidities_index_records = case_when( dia_start_date <= first_bill_time & disease_group == "baseline"  ~ 1,
                                                    TRUE ~ 0)) %>%
  # replace unnecessary disease label with Other_or_non_commorbidities
  mutate( commorbidity_label = case_when(commorbidities_index_records == 1 ~ disease_name,
                                         TRUE ~ "Other_or_non_commorbidities")) %>%
  distinct(idp, commorbidity_label, commorbidities_index_records) %>% 
  spread(commorbidity_label, commorbidities_index_records, fill = 0) %>% 
  select( -Other_or_non_commorbidities) %>% 
  #create this variable for the convinience of subsequent analyses
  mutate( any_MSK = pmax( fybromialgia, oa, rheumatoid_arthritis, osteoporosis, other_musculskeletal_disorders),
          any_pain = pmax( neck_pain, back_pain))

sapply(Baseline_medical_condition_dataset[,-1], function(x)sum(x)/length(x)*100)

Baseline_medical_condition_dataset[, names(Baseline_medical_condition_dataset)[-1]] <- lapply(Baseline_medical_condition_dataset[, names(Baseline_medical_condition_dataset)[-1]], factor)


#===========================================================================#
# medication, previous one years
#============================================================================#
Baseline_medication_dataset <- 
  First_billing_dataset %>% 
  select( idp, first_bill_time) %>% 
  left_join( select( billing_delay, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label, variable_group), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label, Drug_group = variable_group) %>% 
  # index for eligiable records of history of other medications
  mutate( medication_index_records = case_when( bill_date <= first_bill_time & bill_date >= first_bill_time - 365 & Drug_group == "baseline"  ~ 1,
                                                    TRUE ~ 0)) %>%
  # replace unnecessary disease label with Other_or_non_medications
  mutate( medication_label = case_when(medication_index_records == 1 ~ Drug_name,
                                         TRUE ~ "Other_drug")) %>%
  distinct(idp, medication_label, medication_index_records) %>% 
  spread( medication_label, medication_index_records, fill = 0) %>% 
  select( -Other_drug) %>% 
  #create this variable for the convinience of subsequent analyses
  mutate( any_NSAIDs = pmax( Celecoxib, Diclofenaco, Ibuprofeno, Naproxeno, NSAID, morphine, fentanyl),
          any_Psychotropic= pmax( anticonvulsant, benzodiazepines, hypnotics, Metamizole))


 
# sapply(Baseline_medication_dataset[,-1], function(x)sum(x)/length(x)*100) 

Baseline_medication_dataset[, names(Baseline_medication_dataset)[-1]] <- lapply(Baseline_medication_dataset[, names(Baseline_medication_dataset)[-1]], factor)



#===========================================================================#
# health_utilisation, previous one years
#============================================================================#
Baseline_health_utilisation_dataset <- 
  First_billing_dataset %>% 
  select( idp, first_bill_time) %>% 
  left_join( select( admission, idp, admission_cod, admission_date), by = "idp") %>% 

  mutate( admission_index_records_10999 = case_when( admission_date <= first_bill_time & admission_date >= first_bill_time - 365  & admission_cod == 10999 ~ 1,
                                                     TRUE ~ 0)) %>%
  mutate( admission_index_records_30999 = case_when( admission_date <= first_bill_time & admission_date >= first_bill_time - 365  & admission_cod == 30999 ~ 1,
                                                     TRUE ~ 0)) %>%
  group_by(idp) %>%
  summarise( admission_times_10999 = sum(admission_index_records_10999),
             admission_times_30999 = sum(admission_index_records_30999)) %>% 
  ungroup() %>% 
  rename( GP_visits = admission_times_10999, HP_admissions = admission_times_30999)
  
#===========================================================================#
# Charlson comorbidity index 
#============================================================================#
Baseline_CCI_dataset <- 
  First_billing_dataset %>% 
  select( idp, first_bill_time) %>% 
  left_join( check_dup_diagnosis, by = "idp") %>%  
  filter(  dia_start_date <= first_bill_time) %>% 
  # right_join to keep whole population
  right_join( select( First_billing_dataset, idp), by = "idp") %>% 
  select( idp, dia_cod) %>% 
  rename(  code =dia_cod) %>% 
  comorbidity( id = "idp", code = "code", score = "charlson", assign0 = FALSE) %>% 
  mutate( cci_group = factor(case_when( wscore == 0 ~ "0",
                                 wscore > 0 & wscore <= 2 ~ "1-2",
                                 TRUE ~ ">=3"), levels = c("0", "1-2", ">=3"))) %>% 
  select( idp, cci_group)


table(Baseline_CCI_dataset$cci_group)

#===========================================================================#
# complete baseline dataset 
#============================================================================#
Baseline_covariate_complete <- 
  First_billing_dataset %>% 
  select( idp) %>% 
  left_join( Baseline_Demographic_dataset, by = "idp") %>% 
  left_join( Baseline_Lefestyle_dataset, by = "idp") %>% 
  left_join( Baseline_medical_condition_dataset, by = "idp") %>% 
  left_join( Baseline_CCI_dataset, by = "idp") %>% 
  left_join( Baseline_medication_dataset, by = "idp") %>% 
  left_join( Baseline_health_utilisation_dataset, by = "idp")

#==================save=============================================#
# load("R_datasets/Baseline_dataset.RData")
save(First_billing_dataset,
Baseline_Demographic_dataset,
Baseline_Lefestyle_dataset,
Baseline_medical_condition_dataset,
Baseline_CCI_dataset,
Baseline_medication_dataset,
Baseline_health_utilisation_dataset,
Baseline_covariate_complete, file="R_datasets/Baseline_dataset.RData")
#==================save=============================================#

# Baseline on treatment period preparation-----------------------------------------------------
# set.seed(1)
# baseline_test<- sample_frac(baseline_cohort_codeine_add_outcome_100, 0.01)


on_treatment_period <- function(input_dataset ){
  start.time <- Sys.time()
  #==================================================================#
  #  switching, analyses based on two drugs together
  #==================================================================#
  
  switching_dataset <- 
    input_dataset %>% 
    select( idp, first_bill_time, first_bill_drug) %>%
    left_join( select( billing, -billing_agr), by = "idp") %>% 
    left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
    rename( Drug_name = English_label) %>% 
    # filter studied drugs and observation period
    filter(  Drug_name %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
    # calculate prescription duration
    mutate( pre_days = env * 30) %>% 
    # first arrange then group by to save computation time
    arrange( bill_date) %>% 
    group_by( idp) %>% 
    mutate( bill_seq = as.numeric( row_number()),
            total_seq = n()) %>% 
    # cencored date for switching
    mutate( switcher = case_when( any(first_bill_drug != Drug_name) ~ 1,
                                  TRUE ~ 0),
            switcher_seq = case_when( first_bill_drug != Drug_name ~ bill_seq,
                                      TRUE ~ 9999),
            switcher_seq_index = case_when( switcher == 1  ~ min(switcher_seq, na.rm = TRUE) - 1,
                                            TRUE ~ 9999),
            switch_date = case_when( switcher == 1 ~ bill_date[switcher_seq_index] + pre_days[switcher_seq_index] + 15,
                                     TRUE ~ bill_date[n()] + pre_days[n()] + 15)) %>% 
    ungroup() %>% 
    filter( bill_seq == 1) %>% 
    select( idp, switch_date)
  
  #==================================================================#
  #  discontinuation, analyses based on one drug only
  #==================================================================#
  #tramadol
  tramadol_discontinuation_dataset <- 
    First_billing_dataset %>% 
    select( idp, first_bill_time, first_bill_drug) %>%
    filter( first_bill_drug == "tramadol") %>% 
    left_join( select( billing, -billing_agr), by = "idp") %>% 
    left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
    rename( Drug_name = English_label) %>% 
    # filter studied drugs and observation period
    filter(  Drug_name %in% c("tramadol"), bill_date >= as.Date("2007-01-01")) %>%  
    # calculate prescription duration
    mutate( pre_days = env * 30) %>% 
    # first arrange then group by to save computation time
    arrange( bill_date) %>% 
    group_by( idp) %>% 
    mutate( bill_seq = as.numeric( row_number()),
            total_seq = n()) %>% 
    #==================================================================#
    # cencored date for discontinuation 
    #==================================================================# 
    mutate( bill_date_diff = bill_date - lag(bill_date, default = first(bill_date))) %>% 
    mutate( discontinuationer = case_when( any(bill_date_diff > 90 ) ~ 1,
                                           TRUE ~ 0),
            discontinuation_seq = case_when( bill_date_diff > 90 ~ bill_seq,
                                             TRUE ~ 9999),
            discontinuation_index = case_when( discontinuationer == 1  ~ min(discontinuation_seq, na.rm = TRUE) - 1,
                                               TRUE ~ 9999),
            discontinuation_date = case_when( discontinuationer == 1  ~ bill_date[discontinuation_index] + pre_days[discontinuation_index] + 15,
                                              TRUE ~ bill_date[n()] + pre_days[n()] + 15) ) %>% 
    ungroup() %>% 
    filter( bill_seq == 1) %>% 
    select( idp, discontinuation_date)
  
  
  #codeine
  codeine_discontinuation_dataset <- 
    First_billing_dataset %>% 
    select( idp, first_bill_time, first_bill_drug) %>%
    filter( first_bill_drug == "codeine") %>% 
    left_join( select( billing, -billing_agr), by = "idp") %>% 
    left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
    rename( Drug_name = English_label) %>% 
    # filter studied drugs and observation period
    filter(  Drug_name %in% c("codeine"), bill_date >= as.Date("2007-01-01")) %>%  
    # calculate prescription duration
    mutate( pre_days = env * 30) %>% 
    # first arrange then group by to save computation time
    arrange( bill_date) %>% 
    group_by( idp) %>% 
    mutate( bill_seq = as.numeric( row_number()),
            total_seq = n()) %>% 
    #==================================================================#
    # cencored date for discontinuation 
    #==================================================================# 
    mutate( bill_date_diff = bill_date - lag(bill_date, default = first(bill_date))) %>% 
    mutate( discontinuationer = case_when( any(bill_date_diff > 90 ) ~ 1,
                                           TRUE ~ 0),
            discontinuation_seq = case_when( bill_date_diff > 90 ~ bill_seq,
                                             TRUE ~ 9999),
            discontinuation_index = case_when( discontinuationer == 1  ~ min(discontinuation_seq, na.rm = TRUE) - 1,
                                               TRUE ~ 9999),
            discontinuation_date = case_when( discontinuationer == 1  ~ bill_date[discontinuation_index] + pre_days[discontinuation_index] + 15,
                                              TRUE ~ bill_date[n()] + pre_days[n()] + 15) ) %>% 
    ungroup() %>% 
    filter( bill_seq == 1) %>% 
    select( idp, discontinuation_date)
  
  #combine
  
  combine_on_treat_dataset <- 
    bind_rows( tramadol_discontinuation_dataset, codeine_discontinuation_dataset) %>% 
    right_join( switching_dataset, by = "idp")
  
  end.time <- Sys.time()
  print(end.time - start.time)
  return( combine_on_treat_dataset)
}

Baseline_on_treat_dataset <- on_treatment_period( input_dataset = First_billing_dataset)
sapply(Baseline_on_treat_dataset, function(x)sum(is.na(x)))

#==================save=============================================#
#save(Baseline_on_treat_dataset, file="R_datasets/Baseline_on_treat_dataset.RData")
#==================save=============================================#

# source population -----------------------------------------------------
# set.seed(1)
# sub_Denominator <- sample_frac(Denominator_data, 0.1)
#=======================================================#
# all registered subjects with billing data after 2007
#=======================================================#
database_population_codeine <- 
  Denominator_data %>% 
  select( idp) %>% 
  left_join( select( billing_delay, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  # filter studied drugs and observation period
  filter(  Drug_name %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
  arrange( bill_date) %>% 
  group_by( idp) %>% 
  mutate( seq = row_number(), total_seq = n()) %>% # create two index
  mutate( first_bill_time = bill_date[1], first_bill_drug = Drug_name[1]) %>% 
  # index for two drugs dispensed on the same same entry day
  mutate( index_double_user = case_when(any(seq >= 2 & first_bill_time == bill_date & Drug_name != first_bill_drug) ~ 1,
                                       TRUE ~ 0)) %>% 
  ungroup() %>% 
  # filter (select one billing record for each person)
  filter( seq == 1) %>% 
  ungroup() %>% 
  select( idp, first_bill_time, first_bill_drug, index_double_user) %>% 

  #==================================================================#
  # generate population with study drug during the look-back period 
  #==================================================================#
  left_join( select( billing_delay, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  filter( Drug_name %in% c("tramadol", "codeine")) %>% 
  group_by( idp) %>% 
  mutate( index_look_back = case_when( any(bill_date < first_bill_time & bill_date >= first_bill_time - 365 ) ~ 1,
                                       TRUE ~ 0)) %>% 
  ungroup() %>% 
  distinct( idp, first_bill_time, first_bill_drug, index_double_user, index_look_back) %>% 

#==================================================================#
# Aged >= 18 years old on the date of first dispensation (entry date) of studied drugs 
#==================================================================#
  left_join( demography, by = "idp") %>% 
  #important: check if there are missing values for demographics variables
  # sapply(function(x)sum(is.na(x)))
  #index for intiation age and continuous register duration
  mutate( initiation_age = as.numeric( difftime(first_bill_time, date_of_birth, units = "days")/365 ),
          initiation_gap = as.numeric( difftime(first_bill_time, entry_date, units = "days"))) %>% 
  # filter age >= 18
  mutate( index_age = case_when( initiation_age < 18 ~ 1,
                                 TRUE ~ 0),
          index_continuous_enrolment = case_when( initiation_gap < 365 ~ 1,
                                 TRUE ~ 0)) %>% 
  
#==================================================================#
# No previous study outcomes before or at the time of the entry date
#==================================================================#
  left_join( select(check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp")  %>% 
  left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  rename( disease_name = English_label, disease_group = variable_group) %>% 
  group_by( idp) %>% 
  #index for previous outcomes event
  mutate( index_history_outcomes = case_when( any( disease_group == "outcome" & dia_start_date <= first_bill_time ) ~ 1,
                                              TRUE ~ 0)) %>% 
  distinct( idp, first_bill_time, first_bill_drug, index_double_user, index_look_back, index_age, index_continuous_enrolment, index_history_outcomes) %>% 
  ungroup()

# flow_chart cohort -----------------------------------------------------------
study_idp <- 
  database_population_codeine %>% 
  filter( index_double_user ==0)

study_summary <- 
  study_idp %>% 
  group_by( first_bill_drug) %>% 
  summarise( index_age = sum(index_age),
             index_look_back = sum(index_look_back),
             index_continuous_enrolment = sum(index_continuous_enrolment),
             index_history_outcomes = sum(index_history_outcomes))


Base_new_user_idp <- 
  study_idp %>% 
  filter( index_age == 0, # Continuous enrolment in the database < 1 year before the entry date 
          index_look_back == 0,# withou dispensation one year before index date
          index_continuous_enrolment == 0,# Dispensed both tramadol and codeine on the entry date
          index_history_outcomes == 0)  # No outcomes of interest previous or at the time of the entry date

#==================save=============================================#
load("R_datasets/database_population_codeine.RData")
# save(database_population_codeine, file="R_datasets/database_population_codeine.RData")
#==================save=============================================#

# Link to baseline variables ----------------------------------------
Base_new_user_cohort <- 
  Base_new_user_idp %>% 
  select( idp, first_bill_drug) %>% 
  left_join( Baseline_covariate_complete, by = "idp") %>% 
  mutate( first_bill_drug = case_when( first_bill_drug == "codeine" ~ 0,
                                       TRUE ~ 1))

sapply( Base_new_user_cohort, function(x)sum(is.na(x)))

# save(Base_new_user_cohort, file="Base_new_user_cohort.RData")

# Missing data imputation -------------------------------------------------
imputation_data <- 
  Base_new_user_cohort %>% 
  select( -idp , -first_bill_drug) 

mice_model <- mice( data = imputation_data, m = 1, maxit = 2, seed = 500)
Base_new_user_cohort_imputed <- complete(mice_model,1) %>% mutate( idp = Base_new_user_cohort$idp, first_bill_drug = Base_new_user_cohort$first_bill_drug)

#==================save=============================================#
# save(Base_new_user_cohort_imputed, file="R_datasets/Base_new_user_cohort_imputed.RData")
#==================save=============================================#

# Propensity score modelling and matching ----------------------------------------------
## Get variables names
dput( names( Base_new_user_cohort))

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
               data    = Base_new_user_cohort_imputed)

Base_new_user_cohort_probability <- 
  Base_new_user_cohort_imputed %>% 
  mutate( P_tramadol = psModel$fitted.values,
          P_codeine = 1 - psModel$fitted.values)

listMatch <- Matching::Match(Tr       = Base_new_user_cohort_probability$first_bill_drug,      # Need to be in 0,1
                             ## logit of PS,i.e., log(PS/(1-PS)) as matching scale
                             X        = log(Base_new_user_cohort_probability$P_tramadol / Base_new_user_cohort_probability$P_codeine),
                             ## 1:1 matching
                             M        = 1,
                             ## caliper = 0.2 * SD(logit(PS))
                             caliper  = 0.2,
                             replace  = FALSE,
                             ties     = FALSE,
                             version  = "fast")


Mathced_new_user_cohort <- Base_new_user_cohort_probability[unlist(listMatch[c("index.treated","index.control")]), ]

#==================save=============================================#
load("R_datasets/Mathced_new_user_cohort.RData")
# save(Mathced_new_user_cohort, file="R_datasets/Mathced_new_user_cohort.RData")
#==================save=============================================#



# Follow-up outcomes -------------------------------------------------------
set.seed(1)
test_data<- sample_frac(First_billing_dataset, 0.1)

#==================on treatment analysis=============================================#
#==================on treatment analysis=============================================#
specific_outcome <- 
  catalog_clear %>% 
  filter( variable_group == "outcome") %>% 
  distinct( Composite_label) %>% 
  unlist() %>% 
  unname()

names(specific_outcome) <- specific_outcome
# dput(specific_outcome)

#==================intention-to-treatment analysis=============================================#
#==================intention-to-treatment analysis=============================================#

#define function
Follow_ITT_func <- function(input, specific){
  start.time <- Sys.time()
  
  pre_data <- 
    input %>% 
    select( idp, first_bill_drug, first_bill_time) %>% 
    left_join( select( demography, idp, departure_date), by = "idp") %>% 
    #important, because the bill date has been adjusted forward for one month, the departure date should do the same
    mutate( one_year_combined_date = pmin( departure_date + 31, first_bill_time + 365))
  
  one_year_outcome_dataset <-   
    pre_data %>% 
    #==================================================================#
    # index for on-treatment period (current outcome)
    #==================================================================#
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, Composite_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = Composite_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & one_year_combined_date >= dia_start_date & disease_name == specific ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( one_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                       TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( one_year_outcome_occur_date = case_when( one_year_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                    TRUE ~ one_year_combined_date),
            one_year_outcome_name = case_when( one_year_outcome_occur_subject == 1 ~ English_label[1],
                                                     TRUE ~ "No outcome ouccured")) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp, first_bill_time,  one_year_outcome_name, one_year_outcome_occur_subject, one_year_outcome_occur_date, one_year_combined_date) %>%  
    mutate( one_year_censored_date = pmin(one_year_outcome_occur_date, one_year_combined_date),
            one_year_follow_up_days  = as.numeric( difftime( one_year_censored_date , first_bill_time, units = "days"))) %>% 
    ungroup()  
  
  # print("check right")
  # #==================================================================#
  # # follow-up two year period
  # #==================================================================#
  # two_year_outcome_dataset <-   
  #   one_year_outcome_dataset %>% 
  #   select( idp, one_year_outcome_occur_subject,  one_year_combined_date, departure_date) %>% 
  #   filter( one_year_outcome_occur_subject ==0, departure_date > one_year_combined_date) %>% 
  #   
  #   mutate( two_year_combined_date = pmin(one_year_combined_date + 365),  departure_date)%>% 
  #   
  #   left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  #   left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  #   rename( disease_name = English_label, disease_group = variable_group) %>% 
  #   
  #   # identification of outcome
  #   mutate( outcome_occur_record = case_when( dia_start_date > one_year_combined_date & two_year_combined_date >= dia_start_date & disease_name == specific_outcome ~ 1,
  #                                             TRUE ~ 0)) %>%
  #   group_by( idp) %>%
  #   mutate( two_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
  #                                                       TRUE ~ 0)) %>%
  #   arrange( desc(outcome_occur_record), dia_start_date) %>% 
  #   mutate( two_year_outcome_occur_date = case_when( two_year_outcome_occur_subject == 1 ~ dia_start_date[1],
  #                                                    TRUE ~ two_year_combined_date)) %>% 
  #   ungroup() %>%
  #   # condense
  #   distinct( idp, one_year_combined_date, two_year_outcome_occur_subject, two_year_outcome_occur_date, two_year_combined_date, departure_date) %>%  
  #   mutate( two_year_censored_date = pmin(two_year_outcome_occur_date, two_year_combined_date),
  #           two_year_follow_up_days  = as.numeric( difftime( two_year_censored_date , one_year_combined_date, units = "days"))) %>% 
  #   ungroup() 
  # print("check right")
  # #==================================================================#
  # # follow-up three year period
  # #==================================================================#
  # three_year_outcome_dataset <-   
  #   two_year_outcome_dataset %>% 
  #   select( idp, two_year_outcome_occur_subject,  two_year_combined_date, departure_date) %>% 
  #   filter( two_year_outcome_occur_subject ==0, departure_date > two_year_combined_date) %>% 
  #   
  #   mutate( three_year_combined_date = pmin(two_year_combined_date + 365),  departure_date)%>% 
  #   
  #   left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  #   left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  #   rename( disease_name = English_label, disease_group = variable_group) %>% 
  #   
  #   # identification of outcome
  #   mutate( outcome_occur_record = case_when( dia_start_date > two_year_combined_date & three_year_combined_date >= dia_start_date & disease_name == specific_outcome ~ 1,
  #                                             TRUE ~ 0)) %>%
  #   group_by( idp) %>%
  #   mutate( three_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
  #                                                         TRUE ~ 0)) %>%
  #   arrange( desc(outcome_occur_record), dia_start_date) %>% 
  #   mutate( three_year_outcome_occur_date = case_when( three_year_outcome_occur_subject == 1 ~ dia_start_date[1],
  #                                                      TRUE ~ three_year_combined_date)) %>% 
  #   ungroup() %>%
  #   # condense
  #   distinct( idp, two_year_combined_date, three_year_outcome_occur_subject, three_year_outcome_occur_date, three_year_combined_date, departure_date) %>%  
  #   mutate( three_year_censored_date = pmin(three_year_outcome_occur_date, three_year_combined_date),
  #           three_year_follow_up_days  = as.numeric( difftime( three_year_censored_date , two_year_combined_date, units = "days"))) %>% 
  #   ungroup() 
  # print("check right")
  output = list( 
    # overall_outcome_dataset = select( overall_outcome_dataset, idp, overall_outcome_occur_subject, overall_follow_up_days) %>% 
    #  rename(  outcome_occur_subject = overall_outcome_occur_subject, follow_up_days = overall_follow_up_days),
    one_year_outcome_dataset = select( one_year_outcome_dataset, idp, one_year_outcome_occur_subject, one_year_follow_up_days, one_year_outcome_name) %>%
      rename(  outcome_occur_subject = one_year_outcome_occur_subject, follow_up_days = one_year_follow_up_days)
    # two_year_outcome_dataset = select( two_year_outcome_dataset, idp, two_year_outcome_occur_subject, two_year_follow_up_days) %>%
    #   rename(  outcome_occur_subject = two_year_outcome_occur_subject, follow_up_days = two_year_follow_up_days),
    # three_year_outcome_dataset = select( three_year_outcome_dataset, idp, three_year_outcome_occur_subject, three_year_follow_up_days) %>%
    #   rename(  outcome_occur_subject = three_year_outcome_occur_subject, follow_up_days = three_year_follow_up_days)
  )
  
  output <- bind_rows( output, .id = "group_label")
  end.time <- Sys.time()
  print(end.time - start.time)
  return( output)
  
}
Follow_ITT_phase_func <- function(input, specific){
  start.time <- Sys.time()
  
  pre_data <- 
    input %>% 
    select( idp, first_bill_drug, first_bill_time) %>% 
    left_join( select( demography, idp, departure_date), by = "idp") %>% 
    mutate( one_year_combined_date = pmin(departure_date, first_bill_time + 90))
  
  one_year_outcome_dataset <-   
    pre_data %>% 
    #==================================================================#
    # index for on-treatment period (current outcome)
    #==================================================================#
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, Composite_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = Composite_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & one_year_combined_date >= dia_start_date & disease_name == specific ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( one_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                        TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( one_year_outcome_occur_date = case_when( one_year_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                     TRUE ~ one_year_combined_date)) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp, first_bill_time, one_year_outcome_occur_subject, one_year_outcome_occur_date, one_year_combined_date, departure_date) %>%  
    mutate( one_year_censored_date = pmin(one_year_outcome_occur_date, one_year_combined_date),
            one_year_follow_up_days  = as.numeric( difftime( one_year_censored_date , first_bill_time, units = "days"))) %>% 
    ungroup()  
  
  print("check right")
  # #==================================================================#
  # # follow-up two year period
  # #==================================================================#
  two_year_outcome_dataset <-
    one_year_outcome_dataset %>%
    select( idp, one_year_outcome_occur_subject,  one_year_combined_date, departure_date) %>%
    filter( one_year_outcome_occur_subject ==0, departure_date > one_year_combined_date) %>%

    mutate( two_year_combined_date = pmin(one_year_combined_date + 90),  departure_date)%>%

    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, Composite_label, variable_group), by = c("dia_cod" = "cod")) %>%
    rename( disease_name = Composite_label, disease_group = variable_group) %>%

    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > one_year_combined_date & two_year_combined_date >= dia_start_date & disease_name == specific ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( two_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                        TRUE ~ 0)) %>%
    arrange( desc(outcome_occur_record), dia_start_date) %>%
    mutate( two_year_outcome_occur_date = case_when( two_year_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                     TRUE ~ two_year_combined_date)) %>%
    ungroup() %>%
    # condense
    distinct( idp, one_year_combined_date, two_year_outcome_occur_subject, two_year_outcome_occur_date, two_year_combined_date, departure_date) %>%
    mutate( two_year_censored_date = pmin(two_year_outcome_occur_date, two_year_combined_date),
            two_year_follow_up_days  = as.numeric( difftime( two_year_censored_date , one_year_combined_date, units = "days"))) %>%
    ungroup()
  print("check right")
  # #==================================================================#
  # # follow-up three year period
  # #==================================================================#
  three_year_outcome_dataset <-
    two_year_outcome_dataset %>%
    select( idp, two_year_outcome_occur_subject,  two_year_combined_date, departure_date) %>%
    filter( two_year_outcome_occur_subject ==0, departure_date > two_year_combined_date) %>%

    mutate( three_year_combined_date = pmin(two_year_combined_date + 185),  departure_date)%>%

    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, Composite_label, variable_group), by = c("dia_cod" = "cod")) %>%
    rename( disease_name = Composite_label, disease_group = variable_group) %>%

    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > two_year_combined_date & three_year_combined_date >= dia_start_date & disease_name == specific ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( three_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                          TRUE ~ 0)) %>%
    arrange( desc(outcome_occur_record), dia_start_date) %>%
    mutate( three_year_outcome_occur_date = case_when( three_year_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                       TRUE ~ three_year_combined_date)) %>%
    ungroup() %>%
    # condense
    distinct( idp, two_year_combined_date, three_year_outcome_occur_subject, three_year_outcome_occur_date, three_year_combined_date, departure_date) %>%
    mutate( three_year_censored_date = pmin(three_year_outcome_occur_date, three_year_combined_date),
            three_year_follow_up_days  = as.numeric( difftime( three_year_censored_date , two_year_combined_date, units = "days"))) %>%
    ungroup()
  print("check right")
  output = list( 
    # overall_outcome_dataset = select( overall_outcome_dataset, idp, overall_outcome_occur_subject, overall_follow_up_days) %>% 
    #  rename(  outcome_occur_subject = overall_outcome_occur_subject, follow_up_days = overall_follow_up_days),
    one_year_outcome_dataset = select( one_year_outcome_dataset, idp, one_year_outcome_occur_subject, one_year_follow_up_days) %>%
      rename(  outcome_occur_subject = one_year_outcome_occur_subject, follow_up_days = one_year_follow_up_days),
    two_year_outcome_dataset = select( two_year_outcome_dataset, idp, two_year_outcome_occur_subject, two_year_follow_up_days) %>%
      rename(  outcome_occur_subject = two_year_outcome_occur_subject, follow_up_days = two_year_follow_up_days),
    three_year_outcome_dataset = select( three_year_outcome_dataset, idp, three_year_outcome_occur_subject, three_year_follow_up_days) %>%
      rename(  outcome_occur_subject = three_year_outcome_occur_subject, follow_up_days = three_year_follow_up_days)
  )
  
  output <- bind_rows( output, .id = "group_label")
  end.time <- Sys.time()
  print(end.time - start.time)
  return( output)
  
}

#running function
ITT_outcomes_list <- plyr::llply( specific_outcome[3], Follow_ITT_func, input = First_billing_dataset) 
# ITT_outcomes_phase_list <- plyr::llply( specific_outcome, Follow_ITT_phase_func, input = First_billing_dataset) 

ITT_outcomes_dataframe <- bind_rows( ITT_outcomes_list, .id = "outcome_label")
ITT_outcomes_phase_dataframe <- bind_rows( ITT_outcomes_phase_list, .id = "outcome_label")

#==================save=============================================#
load("R_datasets/ITT_outcomes_dataframe.RData")
# save(ITT_outcomes_dataframe, file="R_datasets/ITT_outcomes_dataframe.RData")
# save(ITT_outcomes_phase_dataframe, file="R_datasets/ITT_outcomes_phase_dataframe.RData")

#==================save=============================================#

# Medication Possession Ratio (MPR) ---------------------------------------------
MPR <- 
  Mathced_new_user_cohort %>% 
  select( idp, first_bill_drug) %>% 
  left_join( select(First_billing_dataset, idp, first_bill_time, env), by = "idp") %>% 
  left_join( ITT_outcomes_dataframe, by = "idp") %>% 
  filter( outcome_label %in%  c("composite_CVD", "fracturas", "all_cause_mortality") ) %>% 
  mutate( calendar_year = substring( first_bill_time,1,4)) %>% 
  select( -group_label)

MPR_data <-   list(env_1 = filter( MPR, (first_bill_drug == 1 & env == 1) | first_bill_drug == 0) ,
                   env_2 = filter( MPR, (first_bill_drug == 1 & env == 2) | first_bill_drug == 0) ,
                   env_3 = filter( MPR, (first_bill_drug == 1 & env > 2) | first_bill_drug == 0) 
                   ) %>% 
                    bind_rows( .id = "group_label")  

res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$group_label)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug + calendar_year,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = MPR_data)

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

MPR_survial_hazard <- survial_hazard_func(input = res.separate)

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
MPR_survial_event <- summary_table_func( inputdata = MPR_data)

# #==================================================================#
# # combine all statistics 
# #==================================================================#
MPR_survial_summary <- 
  MPR_survial_event %>% 
  mutate( variable = paste( outcome_label,group_label, sep = ".")) %>% 
  left_join( MPR_survial_hazard, by = "variable") %>% 
  ungroup() %>% 
  mutate( outcome_label = factor( outcome_label, levels = c("composite_CVD", "fracturas",  "all_cause_mortality", "falls", "constipation", "sleep_disorders", "delirium"))) %>% 
  arrange( outcome_label)


# Cox-model outcome stratification (ITT) ------------------------------------------------
cox_dataset_ITT <- 
  Mathced_new_user_cohort %>% 
  select( idp, first_bill_drug) %>% 
  left_join( ITT_outcomes_dataframe, by = "idp") %>% 
  left_join( select(First_billing_dataset, idp, first_bill_time), by = "idp") %>%  
  filter( outcome_label != "opioid_abuse") %>% 
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
  mutate( outcome_label = factor( outcome_label, levels = c("composite_CVD", "fracturas",  "all_cause_mortality", "falls", "constipation", "sleep_disorders", "delirium"))) %>% 
  arrange( outcome_label)


# Cox-model outcome stratification CVD and fractures (ITT) ------------------------------------------------
cox_dataset_CVD_fracture <- 
  Mathced_new_user_cohort %>% 
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
                                                              "falls", "constipation",  "sleep_disorders", "delirium"))) %>% 
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






# Propensity score matching -----------------------------------------------

#==================================================================#
# # data preparation for PS modelling
#==================================================================#
PS_model_dataset <- 
  baseline_cohort_codeine_add_outcome_100_imputation %>% 
  mutate( first_bill_drug = case_when(first_bill_drug == "tramadol" ~ 1,
                                      TRUE ~ 0)) 
factor_list <- setdiff( names(PS_model_dataset), c("idp", "first_bill_time", "first_bill_drug", "initiation_age", "BMI_value", "admission_times_10999", "admission_times_30999"))
PS_model_dataset[factor_list] <- lapply( PS_model_dataset[factor_list], factor)

covariates <- setdiff( names(PS_model_dataset), c( "idp", "first_bill_time", "first_bill_drug"))
dependent_variable <- "first_bill_drug"

# set.seed(1)
# test_data_sub <- sample_frac(PS_model_dataset, 0.1)
# str(test_data_sub)

#==================================================================#
# propensity score modelling
#==================================================================#
start.time <- Sys.time()
PS_model_codeine_add_outcome_100 <- matchit( reformulate(termlabels = covariates, response = dependent_variable), 
                                  method = "nearest",
                                  caliper = 0.2,
                                  ratio=1,
                                  data = PS_model_dataset)
end.time <- Sys.time()
print(end.time - start.time)
#==================save=============================================#
# save(PS_model_codeine_add_outcome_100, file="R_datasets/PS_model_codeine_add_outcome_100.RData")
#==================save=============================================#

#==================================================================#
# diagnostic of propensity score matching 
#==================================================================#
summary(PS_model_codeine_add_outcome_100)
sd_data <- bal.tab(PS_model_codeine_add_outcome_100, binary = "std", un = TRUE)
sd_data
#==================================================================#
# PS score visualisation
#==================================================================#
plot(PS_model_codeine_add_outcome_100, type = 'jitter', interactive = FALSE)


#index for matched rows
index <- 
  data.frame(treat =  rownames(PS_model_codeine_add_outcome_100$match.matrix), control = PS_model_codeine_add_outcome_100$match.matrix[,1] ) %>% 
  filter( !is.na(control)) %>%   
  mutate( control = as.character(control), treat = as.character(treat)) %>% 
  gather( condition, index, control, treat) %>% 
  mutate( index = as.numeric( index)) %>% 
  select( index) %>% 
  unlist()


score_before_matching <- data.frame( first_bill_drug = PS_model_codeine_add_outcome_100$model$y, 
                                         pscore= PS_model_codeine_add_outcome_100$model$fitted.values)

score_after_matching <- data.frame( first_bill_drug = PS_model_codeine_add_outcome_100$model$y[index], 
                                         pscore = PS_model_codeine_add_outcome_100$model$fitted.values[index])



plot_distribution <- function(dataset){

plot <- 
  ggplot( ) +
  # scale_x_continuous( limits = c( 0, 1), breaks = seq( 0, 1, 0.2)) +
  # scale_y_continuous( limits = c( -5, 5), breaks = seq( -5, 5, 2.5)) +
  # Top
  geom_density( data = filter( dataset, first_bill_drug == 1),  aes(x = pscore, y = ..density..),  fill="#69b3a2" ) +
  # geom_label( aes(x= 0.6, y= 2.5, label="Tramadol cohort"), color="#69b3a2") +
  # Bottom
  geom_density( data = filter( dataset, first_bill_drug == 0),  aes(x = pscore, y = -..density..), fill= "#404080") +
  # geom_label( aes(x= 0.6, y= -2.5, label="Codeine cohort"), color="#404080") +
  
  labs( x = "Score",
        y = "Density") +
  
  theme_ipsum() +
  theme(aspect.ratio = 0.66)

return(plot)


}
Distribution_before_matching <- plot_distribution(dataset = score_before_matching)
Distribution_before_matching

Distribution_after_matching <- plot_distribution(dataset = score_after_matching)
Distribution_after_matching

#==================================================================#
# PS mactched cohort
#==================================================================#
mathched_cohort_codine_100 <- 
  PS_model_dataset %>% 
  filter( row_number() %in% index)






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
sd_plot_dataset <- 
  sd_data$Balance %>% 
  select( Type, Diff.Un, Diff.Adj) %>% 
  tibble::rownames_to_column( var = "variable_names") %>% 
  filter( variable_names != "distance") %>% 
  mutate( Diff.Un = Diff.Un * 100, Diff.Adj = Diff.Adj * 100) 
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




  ggplot( data = sd_plot_dataset ) +
  geom_point( aes(x = Diff.Un , y = variable_names), shape = 1, size = 2) +
  geom_point( aes(x = Diff.Adj , y = variable_names, color = "red"), shape = 17, size = 2.5) +
  geom_vline(xintercept = 0, color = "grey") +
  geom_vline(xintercept = -10, color = "grey", linetype="longdash", size = 1) +
  geom_vline(xintercept = 10, color = "grey", linetype="longdash", size = 1) +
  scale_x_continuous(limits = c( -60, 60),
                     breaks = seq( -60, 60, 10))+
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

ggsave(filename = "sd_plot.png",
       path = "Figures",
       plot = sd_plot,
       width = 5.5,
       height = 5.5,
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

