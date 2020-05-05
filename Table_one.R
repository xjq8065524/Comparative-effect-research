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
install.packages("table1")


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
# library(tidyr)
# library(stringr)
# library(xlsx)
# library(naniar)
# library(visdat)
# library(finalfit)
# library(gridExtra)  #masking  'package:dplyr'  combine
library(rms)
library(tableone)

getwd()
setwd("D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research")
# load raw and label datasets -------------------------------------------------------
#SIDIAP raw datasets
load("D:/DPhil/Project_Opioid_use/Data/billing.RData")
load("D:/DPhil/Project_Opioid_use/Data/diagnosis.RData")
load("D:/DPhil/Project_Opioid_use/Data/demography.RData")
load("D:/DPhil/Project_Opioid_use/Data/social_variables.RData")
load("D:/DPhil/Project_Opioid_use/Data/clinical_variables.RData")
Denominator_data <- read_delim("D:/DPhil/Data_raw/OPIODU/OPIOIDES_entregable_poblacio_denominadors_20191219_143606.txt", 
                               delim = "|",
                               col_names = TRUE)
#dictionary datasets
Dic_analgesics <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_analgesics.xlsx")
Dic_CER_adverse_events <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_CER_adverse_events.xlsx")

#derived datasets
load("R_datasets/stage_two_saved_data.RData")
# Baseline table one ----------------------------------------------------------


# data preparation including label, unit, format etc.

#first check the missing value of each variable

sapply(Base_new_user_cohort_imputed, function(x)sum(is.na(x)))

Before_match_cohort <- 
  Base_new_user_cohort_probability %>% 
  mutate( age_group = case_when( initiation_age >= 18 & initiation_age < 40 ~ "18-39",
                                 initiation_age >= 40 & initiation_age < 60 ~ "40-59",
                                 initiation_age >= 60 & initiation_age < 80 ~ "60-79",
                                 initiation_age >= 80  ~ ">=80",
                                 TRUE ~ "<18")) %>%
  mutate( age_group = factor( age_group, levels = c( "18-39", "40-59", "60-79", ">=80", "<18"))) %>% 
  mutate( BMI_group = case_when( BMI_value < 18.5 ~ "Underweight",
                                 BMI_value >= 18.5 & BMI_value < 25 ~ "Normal",
                                 BMI_value >= 25 & BMI_value < 30 ~ "Overweight",
                                 BMI_value >= 30  ~ "Obese",
                                 TRUE ~ "Ms")) %>% 
  mutate( BMI_group = factor( BMI_group, levels = c( "Underweight", "Normal", "Overweight", "Obese", "Ms"))) %>% 
  mutate( rural = factor( rural, levels = c( "U", "R", "Ms")))
  

After_match_cohort <- 
  Mathced_new_user_cohort_probability %>% 
  mutate( age_group = case_when( initiation_age >= 18 & initiation_age < 40 ~ "18-39",
                                 initiation_age >= 40 & initiation_age < 60 ~ "40-59",
                                 initiation_age >= 60 & initiation_age < 80 ~ "60-79",
                                 initiation_age >= 80  ~ ">=80",
                                 TRUE ~ "<18")) %>%
  mutate( age_group = factor( age_group, levels = c( "18-39", "40-59", "60-79", ">=80", "<18"))) %>% 
  mutate( BMI_group = case_when( BMI_value < 18.5 ~ "Underweight",
                                 BMI_value >= 18.5 & BMI_value < 25 ~ "Normal",
                                 BMI_value >= 25 & BMI_value < 30 ~ "Overweight",
                                 BMI_value >= 30  ~ "Obese",
                                 TRUE ~ "Ms")) %>% 
  mutate( BMI_group = factor( BMI_group, levels = c( "Underweight", "Normal", "Overweight", "Obese", "Ms"))) %>% 
  mutate( rural = factor( rural, levels = c( "U", "R", "Ms")))



## Vector of variables to summarize
myVars <- c("age_group", "initiation_age", "sex", "economic_level", "rural", 
            # lifestyle factors
            "BMI_group", "BMI_value", 
            # medical conditions
            "cancer",
            "peripheral_vascular_disease", "cardiac_insufficiency_heart_failure", "angina", "tia", 
             "oa", "osteoporosis", "fybromialgia","rheumatoid_arthritis","other_musculskeletal_disorders",
            "back_pain","neck_pain", 
            "diabetes","chronic_liver_disease","chronic_kidney_disease", "cough", "dyspnea","pulmonary_oedema","diarrhoea", 
            "malabsorption_disorder",  "copd", "neurologic_pathologies", "parkinson_disease","alzheimer_disease", "burn_injuries", "surgery", "traffic", 
            #CCI
            "windex", 
            #medications
            "hypnotics","benzodiazepines", "SSIR",  "anticonvulsant", 
            "Naproxeno","Diclofenaco","Ibuprofeno", "Celecoxib","NSAID","Paracetamol", "Metamizole", "fentanyl", "morphine",   
            #health utilisation
            "GP_visits", "HP_admissions")

## Vector of categorical variables that need transformation
catVars <- c("alzheimer_disease", 
             "angina", "back_pain", "burn_injuries", "cancer", "cardiac_insufficiency_heart_failure", 
             "chronic_kidney_disease", "chronic_liver_disease", "copd", "cough", 
             "diabetes", "diarrhoea", "dyspnea", "fybromialgia", "malabsorption_disorder", 
             "neck_pain", "neurologic_pathologies", "oa", "osteoporosis", 
             "other_musculskeletal_disorders", "parkinson_disease", "peripheral_vascular_disease", 
             "pulmonary_oedema", "rheumatoid_arthritis", "surgery", "tia", 
             "traffic", 
             "anticonvulsant", "benzodiazepines", 
             "Celecoxib", "Diclofenaco", "fentanyl", "hypnotics", "Ibuprofeno", 
             "Metamizole", "morphine", "Naproxeno", "NSAID", "Paracetamol", 
             "SSIR")


summary_before <- 
  CreateTableOne( data = Before_match_cohort, vars = myVars, factorVars = catVars,
                strata = "first_bill_drug",
                test = FALSE) %>% 
  print( smd = TRUE) %>% 
  tibble::as_tibble(rownames = "var_label") %>% 
  select( "var_label", "1", "0", "SMD" )
  
names(summary_before)
summary_after <- 
  CreateTableOne( data = After_match_cohort, vars = myVars, factorVars = catVars,
                  strata = "first_bill_drug",
                  test = FALSE) %>% 
  print( smd = TRUE) %>% 
  tibble::as_tibble(rownames = "var_label") %>% 
  select( "var_label", "1", "0", "SMD" )


summary_combine <- data.frame( summary_before, summary_after)



