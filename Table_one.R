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
install.packages("knitr")


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
library(table1)
library(knitr)
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
load("R_datasets/baseline_cohort.RData")
# Baseline table one (method 1)----------------------------------------------------------
# data preparation including label, unit, format etc.

#first check the missing value of each variable
sapply(baseline_cohort, function(x)sum(is.na(x)))


str(table_baseline_cohort) 
  

labels <- list(
  variables=list(initiation_age=" Age (years)",
                 sex= "Sex",
                 economic_level= "Socioeconomic deprivation",
                 rural= "Residence area",
                 BMI_value= "BMI",
                 Diabetes = "Diabetes" ,
                 Peripheral_vascular_disease = "Peripheral_vascular_disease",
                 COPD = "COPD", 
                 Chronic_cough = "Chronic_cough",
                 Chronic_kidney_disease = " kidney_disease" ,
                 Chronic_liver_disease = "liver_disease",
                 Chronic_musculoskeletal_pain_disorders = "musculoskeletal_pain_disorders" ,
                 Parkinson_disease = "Parkinson_disease",
                 Alzheimer_disease = "Alzheimer_disease",
                
                 admission_times_10999 = "GP visit",
                 admission_times_30999 = "hospital visit"),
  groups=list("", ""))


strata <- c( split(table_baseline_cohort, table_baseline_cohort$first_bill_drug))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("", "Mean (SD) "= sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
}


table1(strata, labels, groupspan=c( 1, 1),
       render.continuous=my.render.cont, render.categorical=my.render.cat)









# Baseline table one (method 2) -------------------------------------------
library(tableone)

str(table_baseline_cohort) 

table_baseline_cohort_fentanyl <- 
  baseline_cohort_imputation_fentanyl %>% 
  select( -idp, -first_bill_time) %>% 
  mutate( first_bill_drug = factor( first_bill_drug, levels = c( "tramadol", "fentanyl")),
          sex = factor(sex, levels = c("H", "D")),
          Alzheimer_disease = as.factor(Alzheimer_disease),
          Chronic_cough = as.factor( Chronic_cough),
          Chronic_kidney_disease = as.factor( Chronic_kidney_disease),
          Chronic_liver_disease = as.factor( Chronic_liver_disease),
          Chronic_musculoskeletal_pain_disorders = as.factor( Chronic_musculoskeletal_pain_disorders),
          COPD = as.factor( COPD),
          Diabetes = as.factor( Diabetes),
          Parkinson_disease = as.factor( Parkinson_disease),
          Peripheral_vascular_disease = as.factor( Peripheral_vascular_disease))

vars <- c("initiation_age","sex", "economic_level", "rural",
          "BMI_value", 
          "Diabetes", "Peripheral_vascular_disease", "COPD", "Chronic_cough", "Chronic_kidney_disease", "Chronic_liver_disease", "Chronic_musculoskeletal_pain_disorders", "Parkinson_disease", "Alzheimer_disease",
          "admission_times_10999", "admission_times_30999")


table_unmatched <- CreateTableOne(vars = vars, strata = "first_bill_drug", data = table_baseline_cohort_fentanyl, test = FALSE)
print_out <- print(table_unmatched, smd = TRUE, noSpaces = TRUE)




table_matched <- CreateTableOne(vars = vars, strata = "first_bill_drug", data = mathched_cohort, test = FALSE)
print_out <- print(table_matched, smd = TRUE, quote = TRUE, noSpaces = TRUE)


