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
library(ckbplotr)

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
load("R_datasets/baseline_cohort.RData")


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
database_population <- 
    Denominator_data %>% 
    select( idp) %>% 
    left_join( select( billing, -billing_agr), by = "idp") %>% 
    left_join( select( Dic_analgesics, ATC_code, Specific_drug), by = c("billing_cod" = "ATC_code")) %>% 
    # filter studied drugs and observation period
    filter( !is.na(Specific_drug), Specific_drug %in% c("tramadol", "codeine"), bill_date >= as.Date("2007-01-01")) %>% 
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

nrow(database_population)

# study population --------------------------------------------------------

#==================================================================#
# Aged >= 18 years old on the date of first dispensation (entry date) of studied drugs 
#==================================================================#
study_population_stage1 <- 
  database_population %>% 
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
study_population_stage2 <- 
  study_population_stage1 %>% 
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

nrow(study_population_stage2)



# Finial cohort -----------------------------------------------------------
#==================================================================#
# Continuous enrolment in the database < 1 year before the entry date 
#==================================================================#
Final_cohort_stage1 <- 
  study_population_stage2 %>% 
  filter( initiation_gap >= 1)

nrow(study_population_stage2) - nrow(Final_cohort_stage1) 

#==================================================================#
# Dispensed both tramadol and codeine on the entry date 
#==================================================================#
Final_cohort_stage2 <- 
  Final_cohort_stage1 %>% 
  filter( index_double_user == 0) 

nrow(Final_cohort_stage1) - nrow(Final_cohort_stage2) 
#==================================================================#
# No outcomes of interest previous or at the time of the entry date
#==================================================================#
Final_cohort_stage3 <- 
  Final_cohort_stage2 %>% 
  filter( history_outcomes == 0) 

nrow(Final_cohort_stage2) - nrow(Final_cohort_stage3) 
nrow(Final_cohort_stage3) 


# Link to other baseline variables ----------------------------------------
baseline_cohort <- 
  Final_cohort_stage3 %>% 
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
  

  
save(baseline_cohort, file="R_datasets/baseline_cohort_100.RData")





# Follow-up periods -------------------------------------------------------
follow_up_dateset <- 
  baseline_cohort %>% 
  select( idp, first_bill_time, first_bill_drug) %>%
  left_join( select( demography, idp, departure_date), by = "idp") %>% 
  left_join( select( check_dup_diagnosis, idp, dia_cod, dia_start_date), by = "idp") %>%
  left_join( Dic_CER_adverse_events, by = "dia_cod") %>% 

  #index for follow-up period
  mutate( outcome_occur_record = case_when( dia_start_date > first_bill_time & !is.na(Disease_Specific) ~ 1,
                                            TRUE ~ 0)) %>% 
  group_by( idp) %>% 
  mutate( outcome_occur_subject = case_when( any(outcome_occur_record == 1) ~ 1,
                                             TRUE ~ 0)) %>% 
  ungroup() %>% 
  filter( !(outcome_occur_subject == 1 & outcome_occur_record == 0)) %>% 
  group_by( idp) %>% 
  mutate( outcome_occur_time = case_when( outcome_occur_subject == 1 ~ min(dia_start_date),
                                          TRUE ~ as.Date("2017-12-31"))) %>% 
  distinct( idp, departure_date, first_bill_time, outcome_occur_subject, outcome_occur_time) %>% 
  ungroup() %>% 
  mutate( censored_time = pmin(departure_date, outcome_occur_time),
          follow_up_time = as.numeric( difftime( censored_time, first_bill_time, units = "days"))) %>% 
  select( idp, outcome_occur_subject, follow_up_time)

save(follow_up_dateset, file="R_datasets/follow_up_dateset_100.RData")

 
# Missing data imputation -------------------------------------------------
baseline_cohort_imputation <- 
  baseline_cohort %>% 
  mutate( BMI_value_impute = case_when( is.na(BMI_value) ~ 24,
                                        TRUE ~ BMI_value))

# Propensity score matching (method 1)-----------------------------------------------
str(baseline_cohort_imputation)

PS_model_dataset <- 
  baseline_cohort_imputation %>% 
  mutate( first_bill_drug = case_when(first_bill_drug == "tramadol" ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(sex = factor(sex, levels = c("H", "D")),
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

names(PS_model_dataset)


 
set.seed(1)
test_data_sub <- sample_frac(PS_model_dataset, 0.2)
str(test_data_sub)

m_out <- matchit(formula=
                 first_bill_drug ~ 
                 initiation_age + 
                 sex +
                 economic_level + 
                 rural + 
                 BMI_value_impute + 
                 Alzheimer_disease +
                 Chronic_cough +
                 Chronic_kidney_disease +
                 Chronic_liver_disease +
                 Chronic_musculoskeletal_pain_disorders +
                 COPD +
                 Diabetes +
                 Parkinson_disease +
                 Peripheral_vascular_disease +
                 admission_times_10999 +
                 admission_times_30999,
                 
                 method = "nearest",
                 ratio=1 ,
                 data = select(PS_model_dataset, -BMI_value))

summary(m_out)

index <- 
  c(rownames(m_out$match.matrix), m_out$match.matrix) %>% 
  as.numeric()

plot_score_before_matching <- data.frame( first_bill_drug = m_out$model$y, 
                                         pscore= m_out$model$fitted.values)

plot_score_after_matching <- data.frame( first_bill_drug = m_out$model$y[index], 
                                         pscore = m_out$model$fitted.values[index])

plot_distribution <- function(dataset){

plot <- 
  ggplot( ) +
  scale_x_continuous( limits = c( 0, 1), breaks = seq( 0, 1, 0.2)) +
  scale_y_continuous( limits = c( -5, 5), breaks = seq( -5, 5, 2.5)) +
  # Top
  geom_density( data = filter( dataset, first_bill_drug == 1),  aes(x = pscore, y = ..density..),  fill="#69b3a2" ) +
  geom_label( aes(x= 0.6, y= 2.5, label="Tramadol cohort"), color="#69b3a2") +
  # Bottom
  geom_density( data = filter( dataset, first_bill_drug == 0),  aes(x = pscore, y = -..density..), fill= "#404080") +
  geom_label( aes(x= 0.6, y= -2.5, label="Codeine cohort"), color="#404080") +
  
  labs( x = "Score",
        y = "Density") +
  
  theme_ipsum() +
  theme(aspect.ratio = 0.66)

return(plot)


}

plot_distribution(dataset = plot_score_before_matching)
plot_distribution(dataset = plot_score_after_matching)


plot(m_out, type = 'jitter', interactive = FALSE)
# 
mathched_cohort <- 
  PS_model_dataset %>% 
  filter( row_number() %in% index)







# Cox-model ---------------------------------------------------------------
cox_dataset <- 
  mathched_cohort %>% 
  left_join(follow_up_dateset, by = "idp")

cox_model <- coxph( Surv(follow_up_time, outcome_occur_subject) ~ 
                      first_bill_drug +
                      initiation_age +
                      sex +
                      economic_level +
                      rural +
                      BMI_value_impute +
                      Alzheimer_disease +
                      Chronic_cough +
                      Chronic_kidney_disease +
                      Chronic_liver_disease +
                      Chronic_musculoskeletal_pain_disorders +
                      COPD +
                      Diabetes +
                      Parkinson_disease +
                      Peripheral_vascular_disease +
                      admission_times_10999 +
                      admission_times_30999,
                      data =  cox_dataset)
summary(cox_model)

set.seed(1)
cc <- sample_n(bb, 1000, replace = FALSE)

plot_list <- ggadjustedcurves(cox_model, data = cc, method = "average", variable = "first_drug")  
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




