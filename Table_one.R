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

#derived datasets
load("R_datasets/stage_two_saved_data.RData")
# Baseline table one ----------------------------------------------------------
dput(names(MSK_new_user_cohort))
# data preparation including label, unit, format etc.

#first check the missing value of each variable

sapply( MSK_new_user_cohort, function(x)sum(is.na(x)))

Before_match_cohort <- 
  MSK_new_user_cohort %>% 
  mutate( age_group = factor(case_when( initiation_age >= 18 & initiation_age < 40 ~ "18-39",
                                 initiation_age >= 40 & initiation_age < 60 ~ "40-59",
                                 initiation_age >= 60  ~ ">=60"), levels = c( "18-39", "40-59",  ">=60"))) %>%
  mutate( economic_level = factor( economic_level, levels = c( "U1", "U2", "U3", "U4", "U5"))) %>% 
  mutate( rural = factor( rural, levels = c( "U", "R", "MS"))) %>% #must keep more than 2 categories
  mutate( BMI_group = factor(case_when( BMI_value < 18.5 ~ "Underweight",
                                 BMI_value >= 18.5 & BMI_value < 25 ~ "Normal",
                                 BMI_value >= 25 & BMI_value < 30 ~ "Overweight",
                                 BMI_value >= 30  ~ "Obese"), levels = c( "Underweight", "Normal", "Overweight", "Obese"))) %>% 
  mutate( Demographics = 1,
          Lifestyle_factors = 1,
          Medical_conditions = 1,
          Cardiovascular_diseases = 1,
          Musculoskeletal_diseases = 1,
          Chronic_Pain = 1,
          Other_conditions = 1,
          Concomitant_medication = 1,
          Psychotropic_drug = 1,
          Other_analgesics = 1,
          Other_opioids = 1,
          Health_care_utilization = 1)

  
names(Before_match_cohort)
After_match_cohort <- 
  Mathced_new_MSK_user_cohort %>% 
  mutate( age_group = factor(case_when( initiation_age >= 18 & initiation_age < 40 ~ "18-39",
                                        initiation_age >= 40 & initiation_age < 60 ~ "40-59",
                                        initiation_age >= 60  ~ ">=60"), levels = c( "18-39", "40-59",  ">=60"))) %>%
  mutate( economic_level = factor( economic_level, levels = c( "U1", "U2", "U3", "U4", "U5"))) %>% 
  mutate( rural = factor( rural, levels = c( "U", "R", "MS"))) %>% 
  mutate( BMI_group = factor(case_when( BMI_value < 18.5 ~ "Underweight",
                                        BMI_value >= 18.5 & BMI_value < 25 ~ "Normal",
                                        BMI_value >= 25 & BMI_value < 30 ~ "Overweight",
                                        BMI_value >= 30  ~ "Obese"), levels = c( "Underweight", "Normal", "Overweight", "Obese"))) %>% 
  mutate( Demographics = 1,
          Lifestyle_factors = 1,
          Medical_conditions = 1,
          Cardiovascular_diseases = 1,
          Musculoskeletal_diseases = 1,
          Chronic_Pain = 1,
          Other_conditions = 1,
          Concomitant_medication = 1,
          Psychotropic_drug = 1,
          Other_analgesics = 1,
          Other_opioids = 1,
          Health_care_utilization = 1)

  
  

## Vector of variables to summarize
myVars <- c("Demographics",
            "initiation_age","age_group",  "sex", "economic_level", "rural", 
            # lifestyle factors
            "Lifestyle_factors",
            "BMI_value", "BMI_group", 
            # medical conditions
            "Medical_conditions",
            "cancer",
            "Chronic_Pain",
            "back_pain","neck_pain", 
            "Cardiovascular_diseases",
            "peripheral_vascular_disease", "cardiac_arrhythmia",  "angina", "tia", 
            "Musculoskeletal_diseases",
             "oa", "osteoporosis", "fybromialgia","rheumatoid_arthritis","other_musculskeletal_disorders",
            "Other_conditions",
            "diabetes","chronic_liver_disease","chronic_kidney_disease", "cough", "dyspnea","pulmonary_oedema","diarrhoea", 
            "malabsorption_disorder",  "copd", "neurologic_pathologies", "parkinson_disease","alzheimer_disease", "burn_injuries", "surgery", "traffic", 
            #CCI
            "cci_group", 
            #medications
            "Concomitant_medication",
            "Psychotropic_drug",
            "hypnotics","benzodiazepines", "SSIR",  "anticonvulsant", 
            "Other_analgesics",
            "Naproxeno","Diclofenaco","Ibuprofeno", "Celecoxib","NSAID","Paracetamol", "Metamizole", "fentanyl", "morphine",
            "Other_opioids",
            #health utilisation
            "Health_care_utilization",
            "GP_visits", "HP_admissions")

## Vector of categorical variables that need transformation
# catVars <- c("alzheimer_disease", 
#              "angina", "back_pain", "burn_injuries", "cancer", "cardiac_insufficiency_heart_failure", 
#              "chronic_kidney_disease", "chronic_liver_disease", "copd", "cough", 
#              "diabetes", "diarrhoea", "dyspnea", "fybromialgia", "malabsorption_disorder", 
#              "neck_pain", "neurologic_pathologies", "oa", "osteoporosis", 
#              "other_musculskeletal_disorders", "parkinson_disease", "peripheral_vascular_disease", 
#              "pulmonary_oedema", "rheumatoid_arthritis", "surgery", "tia", 
#              "traffic", 
#              "anticonvulsant", "benzodiazepines", 
#              "Celecoxib", "Diclofenaco", "fentanyl", "hypnotics", "Ibuprofeno", 
#              "Metamizole", "morphine", "Naproxeno", "NSAID", "Paracetamol", 
#              "SSIR")


summary_before <- 
  CreateTableOne( data = Before_match_cohort, vars = myVars, includeNA = TRUE,
                strata = "first_bill_drug",
                test = FALSE) %>% 
  print( smd = TRUE) %>% 
  tibble::as_tibble(rownames = "var_label") %>% 
  rename( "before_tramadol" = "1", "before_codeine" = "0", "before_smd" = "SMD" ) %>% 
  mutate( before_tramadol = sub("\\( ", "\\(", before_tramadol),
          before_codeine = sub("\\( ", "\\(", before_codeine)) %>% 
  select( var_label, before_tramadol, before_codeine, before_smd)
  
  
names(summary_before)
summary_after <- 
  CreateTableOne( data = After_match_cohort, vars = myVars, includeNA = TRUE,
                  strata = "first_bill_drug",
                  test = FALSE) %>% 
  print( smd = TRUE) %>% 
  tibble::as_tibble(rownames = "var_label") %>% 
  rename( "after_tramadol" = "1", "after_codeine" = "0", "after_smd" = "SMD" ) %>% 
  mutate( after_tramadol = sub("\\( ", "\\(", after_tramadol),
          after_codeine = sub("\\( ", "\\(", after_codeine))%>% 
  select( var_label, after_tramadol, after_codeine, after_smd)


summary_combine <- 
  summary_before %>% 
  left_join( summary_after, by = "var_label") 

