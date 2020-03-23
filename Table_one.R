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
library(table1)

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
sapply(stage_three_saved_data, function(x)sum(is.na(x)))



table_stage_three_saved_data <- 
  stage_three_saved_data %>% 
  mutate( first_drug = factor( first_drug, 
                               levels=c("tramadol","celecoxib"),
                               labels=c("Tramadol", "Celecoxib"))) %>% 
  mutate( sex = factor( sex, 
                        levels=c("H","D"),
                        labels=c("Male", "Female"))) %>% 
  mutate( economic_level = factor( economic_level, 
                           levels=c("U1", "U2", "U3", "U4", "U5"),
                           labels=c("U1", "U2", "U3", "U4", "U5"))) %>% 
  mutate( rural = factor( rural, 
                          levels=c("R", "U"),
                          labels=c("Rural", "Urban"))) %>% 
  rename( Alzheimer_disease = "Alzheimer disease") %>% 
  mutate( Alzheimer_disease = factor( Alzheimer_disease, 
                          levels=c(1),
                          labels=c("Yes"))) %>% 
  rename( Cancer = "Cancer") %>% 
  mutate( Cancer = factor( Cancer, 
                          levels=c(1),
                          labels=c("Yes")))

  
  

table1(~ initiation_age + 
         sex + 
         economic_level + 
         rural | first_drug, data = table_stage_three_saved_data)



labels <- list(
  variables=list(initiation_age=" Age (years)",
                 sex= "Sex",
                 economic_level= "Socioeconomic deprivation",
                 rural= "Residence area",
                 BMI_value= "BMI",
                 Alzheimer_disease = "Alzheimer_disease",
                 Cancer = "Cancer"),
  groups=list("", ""))


strata <- c( split(table_stage_three_saved_data, table_stage_three_saved_data$first_drug))

my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("", "Mean (SD) "= sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
}


table1(strata, labels, groupspan=c( 1, 1),
       render.continuous=my.render.cont, render.categorical=my.render.cat)








