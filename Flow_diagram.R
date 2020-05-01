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
install.packages( "survivalAnalysis")
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

# library(fastDummies)

getwd()
setwd("D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research")
# load raw and label datasets -------------------------------------------------------
load("D:/DPhil/Project_Opioid_use/Data/billing.RData")
# attention: which diagnosis dataset is used
load("D:/DPhil/Project_Opioid_use/Data/diagnosis_complete.RData")
diagnosis <- diagnosis_complete
rm(diagnosis_complete)
load("D:/DPhil/Project_Opioid_use/Data/demography.RData")
load("D:/DPhil/Project_Opioid_use/Data/social_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/clinical_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/admission.RData")
Denominator_data <- read_delim("D:/DPhil/Data_raw/OPIODU/OPIOIDES_entregable_poblacio_denominadors_20191219_143606.txt", 
                               delim = "|",
                               col_names = TRUE)

#dictionary datasets
catalog_clear <- read_excel("D:/DPhil/Project_Opioid_use/Notes/catalog_clear.xlsx") %>% 
                 filter( agr != "CCI" ) %>% 
                 filter( cod_type != "farmacs_prescrits") #important filter, otherwise some row would be duplicated when linked

#prepared datasets
# load("R_datasets/Final_cohort_codeine_100.RData")
load("R_datasets/baseline_cohort_codeine_add_outcome_100.RData")
load("R_datasets/baseline_cohort_codeine_add_outcome_100_imputation.RData")
load("R_datasets/PS_model_codeine_add_outcome_100.RData")

load("R_datasets/On_treatment_codeine_add_outcome_100.RData")
load("R_datasets/ATT_combined_add_outcome_codeine_dataframe.RData")
load("R_datasets/Intention_combined_add_outcome_codeine_dataframe.RData")

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
set.seed(1)
sub_Denominator <- sample_frac(Denominator_data, 0.01)
#=======================================================#
# all registered subjects with billing data after 2007
#=======================================================#
database_population_codeine <- 
  Denominator_data %>% 
  select( idp) %>% 
  left_join( select( billing, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  # filter studied drugs and observation period
  filter(  Drug_name %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
  group_by( idp) %>% 
  arrange( bill_date) %>% 
  mutate( seq = row_number(), total_seq = n()) %>% # create two index
  mutate( first_bill_time = bill_date[1], first_bill_drug = Drug_name[1]) %>% 
  # index for two drugs dispensed on the same same entry day
  mutate( index_double_user = case_when(any(seq >= 2 & first_bill_time == bill_date & Drug_name != first_bill_drug) ~ 1,
                                       TRUE ~ 0)) %>% 
  ungroup() %>% 
  # filter (select one billing record for each person)
  filter( seq == 1) %>% 
  ungroup()  

nrow(database_population_codeine)

#==================================================================#
# generate population with study durg during the look-back period 
#==================================================================#
look_back_population_codeine <- 
  database_population_codeine %>% 
  select( idp, first_bill_time) %>% 
  left_join( select( billing, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  filter( Drug_name %in% c("tramadol", "codeine")) %>% 
  mutate( look_back_date = first_bill_time - 365) %>% 
  filter( bill_date >= look_back_date, bill_date < first_bill_time) %>% 
  distinct( idp) %>% 
  mutate( look_back_index = 1)


database_population_codeine <- 
  database_population_codeine %>% 
  left_join( look_back_population_codeine, by = "idp") %>% 
  mutate( look_back_index = case_when( look_back_index == 1 ~ 1,
                                       TRUE ~ 0))


# study population --------------------------------------------------------

#==================================================================#
# Aged >= 18 years old on the date of first dispensation (entry date) of studied drugs 
#==================================================================#
study_population_stage1_codeine <- 
  database_population_codeine %>% 
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
study_population_stage2_codeine <- 
  study_population_stage1_codeine %>% 
  left_join( select(check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp")  %>% 
  left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  rename( disease_name = English_label, disease_group = variable_group) %>% 
  group_by( idp) %>% 
  #index for previous cancer event
  mutate( history_cancer = case_when( any( disease_group == "baseline_exclude" & dia_start_date <= first_bill_time ) ~ 1,
                                              TRUE ~ 0)) %>% 
  mutate( history_outcomes = case_when( any( disease_group == "outcome" & dia_start_date <= first_bill_time ) ~ 1,
                                              TRUE ~ 0)) %>% 
  ungroup() %>% 
  # filter subjects with cancer
  filter( history_cancer == 0) %>% 
  group_by( idp) %>% 
  filter( row_number() == 1) %>% 
  select( -dia_cod, -dia_start_date, -disease_name, -disease_group, -history_cancer ) %>% 
  ungroup()




nrow(study_population_stage2_codeine)



# Finial cohort -----------------------------------------------------------

Final_cohort_codeine_add_outcome_100 <- 
  study_population_stage2_codeine %>% 
  #==================================================================#
  # Continuous enrolment in the database < 1 year before the entry date 
  #==================================================================#
  filter( initiation_gap >= 1) %>% 
  #==================================================================#
  # withou dispensation one year before index date
  #==================================================================#
  filter(  look_back_index == 0) %>% 
  #==================================================================#
  # Dispensed both tramadol and codeine on the entry date 
  #==================================================================#
  filter( index_double_user == 0) %>% 
  #==================================================================#
  # No outcomes of interest previous or at the time of the entry date
  #==================================================================#
  filter( history_outcomes == 0) 
  
  
Exclusion_summary <- 
  study_population_stage2_codeine %>% 
  summarise( not_continuous_enrol = sum(initiation_gap < 1),
             with_prior_dispensation = sum(look_back_index == 1),
             double_user = sum(index_double_user == 1),
             with_outcome = sum(history_outcomes == 1))
  

# save(Final_cohort_codeine_add_outcome_100, file="R_datasets/Final_cohort_codeine_add_outcome_100.RData")

# set.seed(1)
# Final_cohort_codeine_100 <- sample_frac(Final_cohort_codeine_100, 0.1)
# str(Final_cohort_codeine)



# Link to other baseline variables ----------------------------------------
baseline_cohort_codeine_add_outcome_100 <- 
  Final_cohort_codeine_add_outcome_100 %>% 
  left_join( social_variables, by = "idp") %>%
  left_join( BMI_dataset, by = "idp") %>%
  # 
  # #==================================================================#
  # # deal with missing variables: economic_level, rural 
  # #==================================================================#
  # 
  mutate(economic_level = case_when(economic_level == "" ~ "Ms",
                                    TRUE ~ economic_level),
         rural = case_when(rural == "" ~ "Ms",
                           TRUE ~ rural)) %>%
  mutate(BMI_record_gap = as.numeric( difftime(first_bill_time, clinical_date, units = "days"))) %>%
  # 
  # #==================================================================#
  # # index for eligiable records of BMI (6 months before index date)
  # #==================================================================#
  # 
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
  # 
  # #==================================================================#
  # #  GP and hospital visits one year before cohort entry
  # #==================================================================#
  left_join( select( admission, idp, admission_cod, admission_date), by = "idp") %>%
  mutate( admission_record_gap = as.numeric( difftime(first_bill_time, admission_date, units = "days"))) %>%
  mutate( admission_index_records_10999 = case_when( admission_record_gap >= 0 & admission_record_gap <= 365 & admission_cod == 10999 ~ 1,
                                                     TRUE ~ 0)) %>%
  mutate( admission_index_records_30999 = case_when( admission_record_gap >= 0 & admission_record_gap <= 365 & admission_cod == 30999 ~ 1,
                                                     TRUE ~ 0)) %>%
  group_by(idp) %>%
  mutate( admission_times_10999 = case_when(any(admission_index_records_10999 == 1) ~ sum(admission_index_records_10999),
          TRUE ~ 0)) %>%
  mutate( admission_times_30999 = case_when(any(admission_index_records_30999 == 1) ~ sum(admission_index_records_30999),
                                            TRUE ~ 0)) %>%
  distinct(idp, sex, first_bill_time, first_bill_drug, initiation_age,
           economic_level, rural,
           BMI_value,
           admission_times_10999,
           admission_times_30999) %>%
  ungroup() %>%
  
  # #==================================================================#
  # #  medication history
  # #==================================================================#
  left_join( select( billing, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label, variable_group), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label, Drug_group = variable_group) %>% 
  # index for eligiable records of history of other disease
  mutate( medication_index_records = case_when( bill_date <= first_bill_time & Drug_group == "baseline"  ~ 1,
                                                    TRUE ~ 0)) %>%
  # replace unnecessary disease label with NA
  mutate( medication_label = case_when(medication_index_records == 1 ~ Drug_name,
                                         TRUE ~ "Other_drug")) %>%
  # index for eligiable subjects of history of other disease
  group_by(idp) %>%
  mutate( medication_index_subjects = case_when(any(medication_index_records == 1) ~ 1,
                                                    TRUE ~ 0)) %>%
  ungroup() %>% 
  distinct(idp, sex, first_bill_time, first_bill_drug, initiation_age,
           economic_level, rural,
           BMI_value,
           admission_times_10999,
           admission_times_30999,
           medication_label, medication_index_records) %>% 
  spread( medication_label, medication_index_records, fill = 0) %>% 
  
  # ==================================================================#
  # commorbidity history, use check_dup_diagnosis rather than diagnosis
  # ==================================================================#
  left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  rename( disease_name = English_label, disease_group = variable_group) %>% 
  # index for eligiable records of history of other disease
  mutate( commorbidities_index_records = case_when( dia_start_date <= first_bill_time & disease_group == "baseline"  ~ 1,
                                                    TRUE ~ 0)) %>%
  # replace unnecessary disease label with NA
  mutate( commorbidity_label = case_when(commorbidities_index_records == 1 ~ disease_name,
                                         TRUE ~ "Other_or_non_commorbidities")) %>%
  # index for eligiable subjects of history of other disease
  group_by(idp) %>%
  mutate( commorbidities_index_subjects = case_when(any(commorbidities_index_records == 1) ~ 1,
                                                    TRUE ~ 0)) %>%
  ungroup() %>%
  # # filter unnecessary records
  # filter( !(commorbidities_index_subjects == 1 & commorbidities_index_records == 0) ) %>% 
  # remove duplicate diagnosis
  distinct(idp, sex, first_bill_time, first_bill_drug, initiation_age,
           economic_level, rural,
           BMI_value,
           admission_times_10999,
           admission_times_30999,
           anticonvulsant, benzodiazepines, hypnotics, SSIR,
           commorbidity_label, commorbidities_index_records) %>% 
  # transform from long to wide data
  spread(commorbidity_label, commorbidities_index_records, fill = 0)
  
# save(baseline_cohort_codeine_add_outcome_100, file="R_datasets/baseline_cohort_codeine_add_outcome_100.RData")




# On treatment period -----------------------------------------------------

set.seed(1)
baseline_test<- sample_frac(baseline_cohort_codeine_add_outcome_100, 0.01)


on_treatment_period <- function(input_dataset ){
  start.time <- Sys.time()
  output_dataset <- 
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
    mutate( bill_seq = as.numeric( row_number())) %>% 
    #==================================================================#
    # cencored date for switching
    #==================================================================#
    mutate( switcher = case_when( any(first_bill_drug != Drug_name) ~ 1,
                                  TRUE ~ 0),
            switcher_seq = case_when( first_bill_drug != Drug_name ~ bill_seq,
                                      TRUE ~ NA_real_),
            switcher_seq_index = case_when( switcher == 1  ~ min(switcher_seq, na.rm = TRUE) - 1,
                                            TRUE ~ NA_real_),
            switch_date = case_when( switcher == 1 ~ bill_date[switcher_seq_index] + pre_days[switcher_seq_index] + 15,
                                     TRUE ~ bill_date[n()] + pre_days[n()] + 15)) %>% 
    #==================================================================#
    # cencored date for discontinuation 
    #==================================================================# 
    mutate( bill_date_diff = bill_date - lag(bill_date, default = first(bill_date))) %>% 
    mutate( discontinuationer = case_when( any(bill_date_diff > 90 ) ~ 1,
                                           TRUE ~ 0),
            discontinuation_seq = case_when( bill_date_diff > 90 ~ bill_seq,
                                             TRUE ~ NA_real_),
            discontinuation_index = case_when( discontinuationer == 1  ~ min(discontinuation_seq, na.rm = TRUE) - 1,
                                               TRUE ~ NA_real_),
            discontinuation_date = case_when( discontinuationer == 1  ~ bill_date[discontinuation_index] + pre_days[discontinuation_index] + 15,
                                              TRUE ~ bill_date[n()] + pre_days[n()] + 15) ) %>% 
    ungroup( idp) %>% 
    left_join( select( demography, idp, departure_date), by = "idp") %>% 
    mutate( current_combined_date = pmin( switch_date, discontinuation_date, departure_date)) 
  end.time <- Sys.time()
  print(end.time - start.time)
  return( output_dataset)
}

On_treatment_codeine_add_outcome_100 <- on_treatment_period(input_dataset = baseline_cohort_codeine_add_outcome_100)



#==================save=============================================#
save(On_treatment_codeine_add_outcome_100, file="R_datasets/On_treatment_codeine_add_outcome_100.RData")
#==================save=============================================#

# Medication Possession Ratio (MPR) ---------------------------------------------
MPR <- 
  baseline_cohort_codeine_add_outcome_100 %>% 
  select( idp, first_bill_time, first_bill_drug) %>%
  left_join( select( billing, -billing_agr), by = "idp") %>% 
  left_join( select( catalog_clear, cod, English_label), by = c("billing_cod" = "cod")) %>% 
  rename( Drug_name = English_label) %>% 
  # filter studied drugs and observation period
  filter(  Drug_name %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
  mutate( preb_within_one_year = case_when( (bill_date >= first_bill_time & bill_date < first_bill_time + 365) & Drug_name == first_bill_drug ~ 1,
                                            TRUE ~ 0)) %>% 
  group_by(idp) %>% 
  mutate( sum_preb = sum(preb_within_one_year)) %>% 
  ungroup() %>% 
  distinct( idp, first_bill_drug, sum_preb)
  
options(scipen = 999)
bb <- 
  aa %>% 
  group_by( first_bill_drug, sum_preb) %>% 
  summarise( numb = n()) %>% 
  group_by( first_bill_drug) %>% 
  mutate( perc = as.numeric( numb/ sum( numb) * 100))
  


# Follow-up periods -------------------------------------------------------
# set.seed(1)
# On_treatment_codeine_test<- sample_frac(On_treatment_codeine_add_outcome_100, 0.1)

#==================on treatment analysis=============================================#
#==================on treatment analysis=============================================#
specific_outcome <- 
  catalog_clear %>% 
  filter( variable_group == "outcome") %>% 
  distinct( English_label) %>% 
  unlist() %>% 
  unname()

names(specific_outcome) <- specific_outcome
specific_outcome



Follow_ATT_whole_func <- function(){
  start.time <- Sys.time()
  current_outcome_dataset <-   
    On_treatment_codeine_add_outcome_100 %>% 
    filter( bill_seq == 1) %>% 
    #==================================================================#
    # index for on-treatment period (current outcome)
    #==================================================================#
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & current_combined_date >= dia_start_date & disease_group == "outcome" ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( current_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                       TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( current_outcome_occur_date = case_when( current_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                    TRUE ~ current_combined_date)) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp, first_bill_time, current_outcome_occur_subject, current_outcome_occur_date, current_combined_date, departure_date) %>%  
    mutate( current_censored_date = pmin(current_outcome_occur_date, current_combined_date),
            current_follow_up_days  = as.numeric( difftime( current_censored_date , first_bill_time, units = "days"))) %>% 
    ungroup()  
  
  recent_outcome_dataset <- 
    current_outcome_dataset %>% 
    select( idp, current_outcome_occur_subject, current_outcome_occur_date, current_censored_date, departure_date) %>% 
    filter( current_outcome_occur_subject ==0) %>% 
    mutate( recent_combined_date = pmin( current_censored_date + 180, departure_date)) %>% 
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > current_censored_date & recent_combined_date  >= dia_start_date & disease_group == "outcome" ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( recent_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                      TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( recent_outcome_occur_date = case_when( recent_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                   TRUE ~ recent_combined_date + 180)) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp,  recent_outcome_occur_subject, recent_outcome_occur_date, recent_combined_date, current_censored_date, departure_date) %>%  
    mutate( recent_censored_date = pmin(recent_outcome_occur_date, recent_combined_date),
            recent_follow_up_days  = as.numeric( difftime( recent_censored_date , current_censored_date, units = "days"))) %>% 
    ungroup() 
  
  
  past_outcome_dataset <- 
    recent_outcome_dataset %>% 
    select( idp, recent_outcome_occur_subject, recent_outcome_occur_date, recent_censored_date, departure_date) %>% 
    filter( recent_outcome_occur_subject ==0) %>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > recent_censored_date & departure_date  >= dia_start_date & disease_group == "outcome" ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( past_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                    TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( past_outcome_occur_date = case_when( past_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                 TRUE ~ departure_date)) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp,  past_outcome_occur_subject, past_outcome_occur_date, recent_censored_date, departure_date) %>%  
    mutate( past_censored_date = pmin(past_outcome_occur_date, departure_date),
            past_follow_up_days  = as.numeric( difftime( past_censored_date , recent_censored_date, units = "days"))) %>% 
    ungroup() 
  
  output = list( current_outcome_dataset = select( current_outcome_dataset, idp, current_outcome_occur_subject, current_follow_up_days) %>% 
                   rename(  outcome_occur_subject = current_outcome_occur_subject, follow_up_days = current_follow_up_days),
                 
                 recent_outcome_dataset = select( recent_outcome_dataset, idp, recent_outcome_occur_subject, recent_follow_up_days) %>% 
                   rename(  outcome_occur_subject = recent_outcome_occur_subject, follow_up_days = recent_follow_up_days),
                 
                 past_outcome_dataset = select( past_outcome_dataset, idp, past_outcome_occur_subject, past_follow_up_days) %>% 
                   rename(  outcome_occur_subject = past_outcome_occur_subject, follow_up_days = past_follow_up_days))
  output <- bind_rows( output, .id = "group_label")
  end.time <- Sys.time()
  print(end.time - start.time)
  return( output)
  
}
Follow_ATT_specific_func <- function(specific_outcome){
  start.time <- Sys.time()
  current_outcome_dataset <-   
    On_treatment_codeine_add_outcome_100 %>% 
    filter( bill_seq == 1) %>% 
    #==================================================================#
    # index for on-treatment period (current outcome)
    #==================================================================#
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & current_combined_date >= dia_start_date & disease_name == specific_outcome ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( current_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                       TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( current_outcome_occur_date = case_when( current_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                    TRUE ~ current_combined_date)) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp, first_bill_time, current_outcome_occur_subject, current_outcome_occur_date, current_combined_date, departure_date) %>%  
    mutate( current_censored_date = pmin(current_outcome_occur_date, current_combined_date),
            current_follow_up_days  = as.numeric( difftime( current_censored_date , first_bill_time, units = "days"))) %>% 
    ungroup()  
  
  recent_outcome_dataset <- 
    current_outcome_dataset %>% 
    select( idp, current_outcome_occur_subject, current_outcome_occur_date, current_censored_date, departure_date) %>% 
    filter( current_outcome_occur_subject ==0) %>% 
    mutate( recent_combined_date = pmin( current_censored_date + 180, departure_date)) %>% 
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > current_censored_date & recent_combined_date  >= dia_start_date & disease_name == specific_outcome ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( recent_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                      TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( recent_outcome_occur_date = case_when( recent_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                   TRUE ~ recent_combined_date + 180)) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp,  recent_outcome_occur_subject, recent_outcome_occur_date, recent_combined_date, current_censored_date, departure_date) %>%  
    mutate( recent_censored_date = pmin(recent_outcome_occur_date, recent_combined_date),
            recent_follow_up_days  = as.numeric( difftime( recent_censored_date , current_censored_date, units = "days"))) %>% 
    ungroup() 
  
  
  past_outcome_dataset <- 
    recent_outcome_dataset %>% 
    select( idp, recent_outcome_occur_subject, recent_outcome_occur_date, recent_censored_date, departure_date) %>% 
    filter( recent_outcome_occur_subject ==0) %>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    mutate( outcome_occur_record = case_when( dia_start_date > recent_censored_date & departure_date  >= dia_start_date & disease_name == specific_outcome ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( past_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                    TRUE ~ 0)) %>%
    
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( past_outcome_occur_date = case_when( past_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                 TRUE ~ departure_date)) %>% 
    ungroup() %>%
    #==================================================================#
    # condense
    #==================================================================#
    distinct( idp,  past_outcome_occur_subject, past_outcome_occur_date, recent_censored_date, departure_date) %>%  
    mutate( past_censored_date = pmin(past_outcome_occur_date, departure_date),
            past_follow_up_days  = as.numeric( difftime( past_censored_date , recent_censored_date, units = "days"))) %>% 
    ungroup() 
  
  output = list( current_outcome_dataset = select( current_outcome_dataset, idp, current_outcome_occur_subject, current_follow_up_days) %>% 
                   rename(  outcome_occur_subject = current_outcome_occur_subject, follow_up_days = current_follow_up_days),
                 
                 recent_outcome_dataset = select( recent_outcome_dataset, idp, recent_outcome_occur_subject, recent_follow_up_days) %>% 
                   rename(  outcome_occur_subject = recent_outcome_occur_subject, follow_up_days = recent_follow_up_days),
                 
                 past_outcome_dataset = select( past_outcome_dataset, idp, past_outcome_occur_subject, past_follow_up_days) %>% 
                   rename(  outcome_occur_subject = past_outcome_occur_subject, follow_up_days = past_follow_up_days))
  end.time <- Sys.time()
  print(end.time - start.time)
  output <- bind_rows( output, .id = "group_label")
  
  return( output)
  
}


ATT_whole_codeine <- Follow_ATT_whole_func()
ATT_specific_codeine <- plyr::llply( specific_outcome, Follow_ATT_specific_func) 

ATT_specific_codeine$whole <- ATT_whole_codeine


ATT_combined_add_outcome_codeine_dataframe <- bind_rows( ATT_specific_codeine, .id = "outcome_label")

# save(ATT_combined_add_outcome_codeine_dataframe, file="R_datasets/ATT_combined_add_outcome_codeine_dataframe.RData")

#==================intention-to-treatment analysis=============================================#
#==================intention-to-treatment analysis=============================================#

Follow_intention_whole_func <- function(input_data){
  start.time <- Sys.time()
  
  #==================================================================#
  # follow-up overall period
  #==================================================================#
  # overall_outcome_dataset <-   
  #   input_data %>% 
  #   # important filter to remove unnecessary duplication
  #   filter( bill_seq == 1) %>% 
  #   left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  #   left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  #   rename( disease_name = English_label, disease_group = variable_group) %>% 
  #   
  #   mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & departure_date >= dia_start_date & disease_group == "outcome" ~ 1,
  #                                             TRUE ~ 0)) %>%
  #   group_by( idp) %>%
  #   mutate( overall_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
  #                                                      TRUE ~ 0)) %>%
  #   
  #   arrange( desc(outcome_occur_record), dia_start_date) %>% 
  #   mutate( overall_outcome_occur_date = case_when( overall_outcome_occur_subject == 1 ~ dia_start_date[1],
  #                                                   TRUE ~ departure_date)) %>% 
  #   ungroup() %>%
  #   # condense
  #   distinct( idp, first_bill_time, overall_outcome_occur_subject, overall_outcome_occur_date, departure_date) %>%  
  #   mutate( overall_censored_date = pmin(overall_outcome_occur_date, departure_date),
  #           overall_follow_up_days  = as.numeric( difftime( overall_censored_date , first_bill_time, units = "days"))) %>% 
  #   ungroup()  
  #==================================================================#
  # follow-up one year period
  #==================================================================#
  one_year_outcome_dataset <-   
    input_data %>% 
    # important filter to remove unnecessary duplication
    filter( bill_seq == 1) %>% 
    mutate( one_year_combined_date = pmin(first_bill_time + 365),  departure_date)%>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & one_year_combined_date >= dia_start_date & disease_group == "outcome" ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( one_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                       TRUE ~ 0)) %>%
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( one_year_outcome_occur_date = case_when( one_year_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                    TRUE ~ one_year_combined_date)) %>% 
    ungroup() %>%
    # condense
    distinct( idp, first_bill_time, one_year_outcome_occur_subject, one_year_outcome_occur_date, one_year_combined_date, departure_date) %>%  
    mutate( one_year_censored_date = pmin(one_year_outcome_occur_date, one_year_combined_date),
            one_year_follow_up_days  = as.numeric( difftime( one_year_censored_date , first_bill_time, units = "days"))) %>% 
    ungroup()  
  print("check right")
  #==================================================================#
  # follow-up two year period
  #==================================================================#
  two_year_outcome_dataset <-   
    one_year_outcome_dataset %>% 
    select( idp, one_year_outcome_occur_subject,  one_year_combined_date, departure_date) %>% 
    filter( one_year_outcome_occur_subject ==0, departure_date > one_year_combined_date) %>% 

    mutate( two_year_combined_date = pmin(one_year_combined_date + 365),  departure_date)%>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > one_year_combined_date & two_year_combined_date >= dia_start_date & disease_group == "outcome" ~ 1,
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
  #==================================================================#
  # follow-up three year period
  #==================================================================#
  three_year_outcome_dataset <-   
    two_year_outcome_dataset %>% 
    select( idp, two_year_outcome_occur_subject,  two_year_combined_date, departure_date) %>% 
    filter( two_year_outcome_occur_subject ==0, departure_date > two_year_combined_date) %>% 
    
    mutate( three_year_combined_date = pmin(two_year_combined_date + 365),  departure_date)%>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > two_year_combined_date & three_year_combined_date >= dia_start_date & disease_group == "outcome" ~ 1,
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
Follow_intention_specific_func <- function(input_data, specific_outcome){
  start.time <- Sys.time()
  
  #==================================================================#
  # follow-up overall period
  #==================================================================#
  # overall_outcome_dataset <-   
  #   input_data %>% 
  #   # important filter to remove unnecessary duplication
  #   filter( bill_seq == 1) %>% 
  #   left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  #   left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
  #   rename( disease_name = English_label, disease_group = variable_group) %>% 
  #   
  #   mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & departure_date >= dia_start_date & disease_group == "outcome" ~ 1,
  #                                             TRUE ~ 0)) %>%
  #   group_by( idp) %>%
  #   mutate( overall_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
  #                                                      TRUE ~ 0)) %>%
  #   
  #   arrange( desc(outcome_occur_record), dia_start_date) %>% 
  #   mutate( overall_outcome_occur_date = case_when( overall_outcome_occur_subject == 1 ~ dia_start_date[1],
  #                                                   TRUE ~ departure_date)) %>% 
  #   ungroup() %>%
  #   # condense
  #   distinct( idp, first_bill_time, overall_outcome_occur_subject, overall_outcome_occur_date, departure_date) %>%  
  #   mutate( overall_censored_date = pmin(overall_outcome_occur_date, departure_date),
  #           overall_follow_up_days  = as.numeric( difftime( overall_censored_date , first_bill_time, units = "days"))) %>% 
  #   ungroup()  
  #==================================================================#
  # follow-up one year period
  #==================================================================#
  one_year_outcome_dataset <-   
    input_data %>% 
    # important filter to remove unnecessary duplication
    filter( bill_seq == 1) %>% 
    mutate( one_year_combined_date = pmin(first_bill_time + 365),  departure_date)%>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & one_year_combined_date >= dia_start_date & disease_name == specific_outcome ~ 1,
                                              TRUE ~ 0)) %>%
    group_by( idp) %>%
    mutate( one_year_outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                                        TRUE ~ 0)) %>%
    arrange( desc(outcome_occur_record), dia_start_date) %>% 
    mutate( one_year_outcome_occur_date = case_when( one_year_outcome_occur_subject == 1 ~ dia_start_date[1],
                                                     TRUE ~ one_year_combined_date)) %>% 
    ungroup() %>%
    # condense
    distinct( idp, first_bill_time, one_year_outcome_occur_subject, one_year_outcome_occur_date, one_year_combined_date, departure_date) %>%  
    mutate( one_year_censored_date = pmin(one_year_outcome_occur_date, one_year_combined_date),
            one_year_follow_up_days  = as.numeric( difftime( one_year_censored_date , first_bill_time, units = "days"))) %>% 
    ungroup()  
  print("check right")
  #==================================================================#
  # follow-up two year period
  #==================================================================#
  two_year_outcome_dataset <-   
    one_year_outcome_dataset %>% 
    select( idp, one_year_outcome_occur_subject,  one_year_combined_date, departure_date) %>% 
    filter( one_year_outcome_occur_subject ==0, departure_date > one_year_combined_date) %>% 
    
    mutate( two_year_combined_date = pmin(one_year_combined_date + 365),  departure_date)%>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > one_year_combined_date & two_year_combined_date >= dia_start_date & disease_name == specific_outcome ~ 1,
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
  #==================================================================#
  # follow-up three year period
  #==================================================================#
  three_year_outcome_dataset <-   
    two_year_outcome_dataset %>% 
    select( idp, two_year_outcome_occur_subject,  two_year_combined_date, departure_date) %>% 
    filter( two_year_outcome_occur_subject ==0, departure_date > two_year_combined_date) %>% 
    
    mutate( three_year_combined_date = pmin(two_year_combined_date + 365),  departure_date)%>% 
    
    left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
    left_join( select( catalog_clear, cod, English_label, variable_group), by = c("dia_cod" = "cod")) %>% 
    rename( disease_name = English_label, disease_group = variable_group) %>% 
    
    # identification of outcome
    mutate( outcome_occur_record = case_when( dia_start_date > two_year_combined_date & three_year_combined_date >= dia_start_date & disease_name == specific_outcome ~ 1,
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


intention_whole_codeine <- Follow_intention_whole_func(input_data = On_treatment_codeine_add_outcome_100)

intention_specific_codeine <- plyr::llply( specific_outcome, Follow_intention_specific_func, input_data = On_treatment_codeine_add_outcome_100) 

intention_specific_codeine$whole <- intention_whole_codeine

Intention_combined_add_outcome_codeine_dataframe <- bind_rows( intention_specific_codeine, .id = "outcome_label")

#==================save=============================================#
save(Intention_combined_add_outcome_codeine_dataframe, file="R_datasets/Intention_combined_add_outcome_codeine_dataframe.RData")
#==================save=============================================#
  

 
# Missing data imputation -------------------------------------------------

summary(baseline_cohort_codeine_add_outcome_100)
sapply(baseline_cohort_codeine_add_outcome_100, function(x)sum(is.na(x)))

mice_cohort <- 
  baseline_cohort_codeine_add_outcome_100 %>% 
  select(  -Other_or_non_commorbidities) %>% 
  mice( method = 'mean', m = 1, maxit = 1)

# 
# imputation_plot <- densityplot(mice_cohort, ~BMI_value)
# imputation_plot
baseline_cohort_codeine_add_outcome_100_imputation <- complete(mice_cohort)

#==================save=============================================#
# save(baseline_cohort_codeine_add_outcome_100_imputation, file="R_datasets/baseline_cohort_codeine_add_outcome_100_imputation.RData")
#==================save=============================================#


# trellis.device(device="png", filename="Figures/imputation_plot.png")
# print(imputation_plot)
# dev.off()


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


# Cox-model ---------------------------------------------------------------
cox_dataset <- 
  mathched_cohort_codine_100 %>% 
  left_join(ATT_combined_add_outcome_codeine_dataframe, by = "idp") %>% 
  filter( !is.na(outcome_label))


cox_model <- coxph( Surv(follow_up_days, outcome_occur_subject) ~ first_bill_drug ,
                    data =  cox_dataset)
summary(cox_model)

# set.seed(1)
# cc <- sample_n(cox_dataset, 1000, replace = FALSE)
# 
# plot_list <- ggadjustedcurves(cox_model, data = cc, method = "average", variable = "first_bill_drug")  
# plot_data <- plot_list$data
# plot_data$surv <- (1 - (plot_data$surv))


names(cox_dataset)



# Cox-model outcome stratification (ATT) ------------------------------------------------

cox_dataset_ATT <- 
  mathched_cohort_codine_100 %>% 
  left_join(ATT_combined_add_outcome_codeine_dataframe, by = "idp") %>% 
  filter( !is.na(outcome_label))


res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$group_label)),
                        FUN = function(DF) {
                           coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                                  first_bill_drug ,
                                  data =  DF)
                       })}

res.separate <- res.separate_func(input = cox_dataset_ATT)


survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
  dt <- summary(x)
  # p.value<-signif(dt$wald["pvalue"], digits=2)
  # wald.test<-signif(x$wald["test"], digits=3)
  # beta<-signif(x$coef[1], digits=3);#coeficient beta
  sd <- signif(dt$coef[3], digits=3);
  HR <-dt$coef[2];#exp(beta)
  HR.confint.lower <-dt$conf.int[,"lower .95"]
  HR.confint.upper <- dt$conf.int[,"upper .95"]
  # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
  res<-c( HR , sd, HR.confint.lower, HR.confint.upper)
  names(res)<-c( "estimate", "stderr", "lower", "upper")
  return(res)
  #return(exp(cbind(coef(x),confint(x)))) 
}) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), "-",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")"))}

ATT_survial_hazard <- survial_hazard_func(input = res.separate)

# #==================================================================#
# # Calculate addtional statistics ( mean follow up, rate)
# #==================================================================#
summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( outcome_label, group_label, first_bill_drug) %>% 
    summarise(n = n(), 
              mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days)/365)) %>% 
    group_by(outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>% 
     mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2))
  
   
  return(summary_table)
}
ATT_survial_event <- summary_table_func( inputdata = cox_dataset_ATT)

# #==================================================================#
# # combine all statistics 
# #==================================================================#

ATT_survial_summary <- 
  filter( ATT_survial_event, first_bill_drug ==0) %>% 
  left_join( filter( ATT_survial_event, first_bill_drug ==1), by = c("outcome_label", "group_label")) %>% 
  mutate( variable = paste( outcome_label,group_label, sep = ".")) %>% 
  left_join( ATT_survial_hazard, by = "variable")


# Cox-model outcome stratification (ITT) ------------------------------------------------

cox_dataset_ITT <- 
  mathched_cohort_codine_100 %>% 
  left_join(Intention_combined_add_outcome_codeine_dataframe, by = "idp") 


res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$group_label)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug ,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = cox_dataset_ITT)


survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
    dt <- summary(x)
    # p.value<-signif(dt$wald["pvalue"], digits=2)
    # wald.test<-signif(x$wald["test"], digits=3)
    # beta<-signif(x$coef[1], digits=3);#coeficient beta
    sd <- signif(dt$coef[3], digits=3);
    HR <-dt$coef[2];#exp(beta)
    HR.confint.lower <-dt$conf.int[,"lower .95"]
    HR.confint.upper <- dt$conf.int[,"upper .95"]
    # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c( HR , sd, HR.confint.lower, HR.confint.upper)
    names(res)<-c( "estimate", "stderr", "lower", "upper")
    return(res)
    #return(exp(cbind(coef(x),confint(x)))) 
  }) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), "-",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")"))}

ITT_survial_hazard <- survial_hazard_func(input = res.separate)

# #==================================================================#
# # Calculate addtional statistics ( mean follow up, rate)
# #==================================================================#
summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( outcome_label, group_label, first_bill_drug) %>% 
    summarise(n = n(), 
              mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days)/365)) %>% 
    group_by(outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>% 
    mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2))
  
  
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
  left_join( ITT_survial_hazard, by = "variable")


# Cox-model outcome stratification (ITT, dose-dependent response) ------------------------------------------------
cox_dataset_dose_dependent <- 
  mathched_cohort_codine_100 %>% 
  left_join(Intention_combined_add_outcome_codeine_dataframe, by = "idp") %>% 
  filter( outcome_label %in% c( "myocardial_infarction", "stroke", "fracturas", "all_cause_mortality"), group_label == "one_year_outcome_dataset") %>% 
  left_join( select(MPR, idp, sum_preb), by = "idp") %>% 
  mutate( dose_group = case_when( sum_preb == 1 ~ "1",
                                  sum_preb == 2 ~ "2",
                                  TRUE ~ "3"))
tramadol_doese <- 
  cox_dataset_dose_dependent %>% 
  filter( first_bill_drug == 1)

codeine_reference <- 
  cox_dataset_dose_dependent %>% 
  filter( first_bill_drug == 0) %>% 
  select( -dose_group)


aa <- 
  bind_rows( list(codeine_reference, codeine_reference, codeine_reference), .id = "dose_group")

bb <- 
  bind_rows( tramadol_doese, aa)

table(bb$dose_group)

res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$dose_group)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug ,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = bb)


survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
    dt <- summary(x)
    # p.value<-signif(dt$wald["pvalue"], digits=2)
    # wald.test<-signif(x$wald["test"], digits=3)
    # beta<-signif(x$coef[1], digits=3);#coeficient beta
    sd <- signif(dt$coef[3], digits=3);
    HR <-dt$coef[2];#exp(beta)
    HR.confint.lower <-dt$conf.int[,"lower .95"]
    HR.confint.upper <- dt$conf.int[,"upper .95"]
    # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c( HR , sd, HR.confint.lower, HR.confint.upper)
    names(res)<-c( "estimate", "stderr", "lower", "upper")
    return(res)
    #return(exp(cbind(coef(x),confint(x)))) 
  }) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), "-",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")"))}

ITT_survial_hazard_Dose <- survial_hazard_func(input = res.separate)

# #==================================================================#
# # Calculate addtional statistics ( mean follow up, rate)
# #==================================================================#
summary_table_func <- function( inputdata){
  summary_table <- 
    inputdata %>% 
    group_by( outcome_label, dose_group, first_bill_drug) %>% 
    summarise(n = n(), 
              mean_age = mean( initiation_age), 
              events = sum( outcome_occur_subject), 
              mean_follow = as.numeric( mean(follow_up_days)/365)) %>% 
    group_by(outcome_label, dose_group) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    
    mutate( mean_follow = substring(mean_follow, 1,str_locate(as.character(mean_follow), "[.]")[,1] + 2)) %>% 
    mutate( rate = substring(rate, 1,str_locate(as.character(rate), "[.]")[,1] + 2))
  
  
  return(summary_table)
}
ITT_survial_event_Dose <- summary_table_func( inputdata = bb)

# #==================================================================#
# # combine all statistics 
# #==================================================================#

ITT_survial_summary_Dose <- 
  filter( ITT_survial_event_Dose, first_bill_drug ==0) %>% 
  left_join( filter( ITT_survial_event_Dose, first_bill_drug ==1), by = c("outcome_label", "dose_group")) %>% 
  mutate( variable = paste( outcome_label,dose_group, sep = ".")) %>% 
  left_join( ITT_survial_hazard_Dose, by = "variable")


# Cox-model interaction stratification ------------------------------------------------
cox_dataset_whole_current <- 
  mathched_cohort_codine_100 %>% 
  left_join(Intention_combined_add_outcome_codeine_dataframe, by = "idp") %>% 
  filter( outcome_label %in% c( "myocardial_infarction", "stroke", "fracturas", "all_cause_mortality"), group_label == "one_year_outcome_dataset") %>% 
  mutate( separate_label = outcome_label) %>% 
  select( -outcome_label, -group_label) %>% 
  mutate( age_group = case_when( initiation_age >=18 & initiation_age < 45 ~ "18-44",
                                 initiation_age >=45 & initiation_age < 65 ~ "45-65",
                                 initiation_age >=65 & initiation_age < 80 ~ "65-80",
                                 initiation_age >=70 ~ ">=80"))


cox_dataset_interaction <-   list( overall = cox_dataset_whole_current,
                                   sex = cox_dataset_whole_current,
                                   initiation_age = cox_dataset_whole_current,
                                   back_pain = filter(cox_dataset_whole_current, back_pain ==1),
                                   neck_pain = filter(cox_dataset_whole_current, neck_pain ==1),
                                   oa = filter(cox_dataset_whole_current, oa ==1),
                                   other_musculskeletal_disorders = filter(cox_dataset_whole_current, other_musculskeletal_disorders ==1),
                                   neurologic_pathologies = filter(cox_dataset_whole_current, neurologic_pathologies ==1)) %>% bind_rows( .id = "outcome_label") %>% 
                              mutate( group_label = case_when( outcome_label == "overall" ~ "overall", 
                                                               outcome_label == "sex" ~ as.character(sex), 
                                                               outcome_label == "initiation_age" ~ age_group,
                                                               outcome_label == "back_pain" ~ "back_pain",
                                                               outcome_label == "neck_pain" ~ "neck_pain",
                                                               outcome_label == "oa" ~ "oa",
                                                               outcome_label == "other_musculskeletal_disorders" ~ "other_musculskeletal_disorders",
                                                               outcome_label == "neurologic_pathologies" ~ "neurologic_pathologies")) %>% 
                              mutate( outcome_label = case_when( outcome_label %in% c( "back_pain", "neck_pain", "oa", "other_musculskeletal_disorders", "neurologic_pathologies") ~ "pain_indication",
                                                                 TRUE ~ outcome_label)) %>% 
                              mutate( variable = paste(outcome_label, group_label, sep = "."))

table(cox_dataset_interaction$variable)


res.separate_func <- function(input){
  lapply( split(input, list(input$separate_label, input$variable)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug ,
                   data =  DF)
          })}

res.separate <- res.separate_func(input = cox_dataset_interaction)


survial_hazard_func <- function(input){  
  sapply(input,function(x){ 
    dt <- summary(x)
    # p.value<-signif(dt$wald["pvalue"], digits=2)
    # wald.test<-signif(x$wald["test"], digits=3)
    # beta<-signif(x$coef[1], digits=3);#coeficient beta
    sd <- signif(dt$coef[3], digits=3);
    HR <-dt$coef[2];#exp(beta)
    HR.confint.lower <-dt$conf.int[,"lower .95"]
    HR.confint.upper <- dt$conf.int[,"upper .95"]
    # CI <- paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res<-c( HR , sd, HR.confint.lower, HR.confint.upper)
    names(res)<-c( "estimate", "stderr", "lower", "upper")
    return(res)
    #return(exp(cbind(coef(x),confint(x)))) 
  }) %>% 
    t() %>% 
    as_tibble(rownames = NA) %>% 
    tibble::rownames_to_column() %>% 
    rename( variable = rowname) %>% 
    mutate( CI = paste(substring(estimate, 1,str_locate(as.character(estimate), "[.]")[,1] + 2), " (",
                       substring(lower, 1,str_locate(as.character(lower), "[.]")[,1] + 2), "-",
                       substring(upper, 1,str_locate(as.character(upper), "[.]")[,1] + 2), ")"))}

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
              mean_follow = as.numeric( mean(follow_up_days)/365)) %>% 
    group_by(outcome_label, group_label) %>% 
    mutate( rate = events / (n * mean_follow) * 1000 ) %>% 
    mutate( ratio = rate[2] / rate[1]) %>% 
    mutate( dif = rate[2] - rate[1]) %>% 
    
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
  left_join( survial_hazard, by = "variable")



# Univariate survival analyses outcome stratification ---------------------
KM_dataset_intention <- 
  mathched_cohort_codine_100 %>% 
  left_join(Intention_combined_add_outcome_codeine_dataframe, by = "idp") %>% 
  filter( !is.na(outcome_label))

aa <- 
  KM_dataset_intention %>% 
  filter( outcome_label == "stroke") 


res.separate_func <- function(input){
  lapply( split(input, list(input$outcome_label, input$group_label)),
          FUN = function(DF) {
            coxph( Surv(follow_up_days, outcome_occur_subject) ~ 
                     first_bill_drug ,
                   data =  DF)
          })}


fit <- analyse_survival(vars(follow_up_days, outcome_occur_subject),
                   by = first_bill_drug,
                   data = aa)


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

ggsave(filename = "Distribution_before_matching.png",
       path = "Figures",
       plot = Distribution_before_matching,
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

