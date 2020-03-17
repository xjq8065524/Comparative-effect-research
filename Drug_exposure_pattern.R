### Set up library ----------------------------------------------------------
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
install.packages("installr") #Updating R/RStudio: https://uvastatlab.github.io/phdplus/installR.html#updateR

# memory.limit()
# memory.limit(62468)
library(dplyr)
library(zoo)
library(lubridate)
library(readxl)
library(ggplot2)
library(remotes)
library(survival)
library(survminer)
library(cowplot)
library(scales)
library(ggsci)
library(mefa4)
library(pryr)
library(gmodels)
library(extrafont)
library(labeling)
library(readr)
library(tidyr)
library(stringr)
library(xlsx)
library(naniar)
library(visdat)
library(finalfit)
library(gridExtra)  #masking  'package:dplyr'  combine
getwd()
setwd("D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/R codes/Comparative-effect-research")
# load raw and label datasets -------------------------------------------------------
load("D:/DPhil/Project_Opioid_use/Data/billing.RData")
load("R_datasets/pattern_cod_tra_stage3.RData")

Denominator_data <- read_delim("D:/DPhil/Data_raw/OPIODU/OPIOIDES_entregable_poblacio_denominadors_20191219_143606.txt", 
                               delim = "|",
                               col_names = TRUE)

#dictionary datasets
Dic_analgesics <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_analgesics.xlsx")


# Little explore with raw dta ---------------------------------------------
head(billing)
str(billing)
glimpse(billing)

# check missing of each variable (no missing)
sapply(billing, function(x)sum(is.na(x)))

#make sure there is no duplicate (no duplicate row)
check_dup <- distinct(billing, idp, billing_cod, bill_date, .keep_all = TRUE)



# Number of dispensation each person --------------------------------------
set.seed(1)

sub_Denominator <- sample_frac(Denominator_data, 0.1)


#codeine only
number_cod <- 
  Denominator_data %>% 
  left_join(billing, by = "idp") %>% 
  left_join(Dic_analgesics, by = c("billing_cod" = "ATC_code")) %>% 
  filter(!is.na(Drug), Drug %in% c("codeine")) %>% 
  group_by(idp) %>% 
  arrange(bill_date) %>% 
  mutate(seq = row_number(), total_seq = n()) %>% 
  ungroup() %>% 
  distinct(idp, total_seq)


#codeine only
number_tra <- 
  Denominator_data %>% 
  left_join(billing, by = "idp") %>% 
  left_join(Dic_analgesics, by = c("billing_cod" = "ATC_code")) %>% 
  filter(!is.na(Drug), Drug %in% c("tramadol")) %>% 
  group_by(idp) %>% 
  arrange(bill_date) %>% 
  mutate(seq = row_number(), total_seq = n()) %>% 
  ungroup() %>% 
  distinct(idp, total_seq)


#codeine + tramadol
number_cod.tra <- 
  Denominator_data %>% 
  left_join(billing, by = "idp") %>% 
  left_join(Dic_analgesics, by = c("billing_cod" = "ATC_code")) %>% 
  filter(!is.na(Drug), Drug %in% c("codeine","tramadol")) %>% 
  group_by(idp) %>% 
  arrange(bill_date) %>% 
  mutate(seq = row_number(), total_seq = n()) %>% 
  ungroup() %>% 
  distinct(idp, total_seq)

# Explore switchers -------------------------------------------------------
set.seed(1)

sub_Denominator <- sample_frac(Denominator_data, 0.1)

#select all codeine and tramadol users, generate some auxiliary variables
pattern_cod_tra_stage1 <- 
  Denominator_data %>% 
  left_join(billing, by = "idp") %>% 
  left_join(Dic_analgesics, by = c("billing_cod" = "ATC_code")) %>% 
  filter(!is.na(Drug), Drug %in% c("codeine","tramadol")) %>% 
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
  mutate(first_time = bill_date[1], first_drug = Drug[1]) %>%  # make sure the first position means ealiest date
  #generate one variable to indicate whether double initiators. 0: no, 1:yes
  mutate(index_double_user = case_when(any(seq >= 2 & first_time == bill_date & Drug != first_drug) ~ 1,
                                      TRUE ~ 0)) %>% 
  ungroup()

pattern_cod_tra_stage2 %>% 
  filter(index_double_user == 1) %>% 
  summarise(n = n_distinct(idp))

#generate switchers and no_switchers index
pattern_cod_tra_stage3 <- 
  pattern_cod_tra_stage2 %>% 
  filter(index_double_user == 0) %>% #only for non double users
  mutate(index = case_when(Drug == "codeine" ~ 1,
                           Drug == "tramadol" ~ -1)) %>% 
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
                                       switchs == "Switchers" ~ as.numeric(min((which(first_drug != Drug)))))) %>% 
  ungroup()



save(pattern_cod_tra_stage4, file="R_datasets/pattern_cod_tra_stage4.RData")



#generate dispensation gap index
pattern_cod_tra_stage5 <- 
  pattern_cod_tra_stage4 %>% 
  group_by(idp) %>%
  arrange(bill_date) %>%
  mutate(diff = bill_date - lag(bill_date, default = first(bill_date)))






  

# Explore patterns --------------------------------------------------------
a <- 
  pattern_cod_tra_stage4 %>% 
  filter(total_seq <=5, first_drug == "codeine", switchs == "Switchers") %>% 
  select(idp, first_drug, total_seq, seq, index) %>% 
  spread(seq, index) %>% #from long to wide
  rename("First" = "1", "Second" = "2", "Third" = "3", "Fourth" = "4", "Fifth" = "5") %>% 
  replace_na(list(First = 0, Second = 0, Third = 0, Fourth = 0, Fifth = 0))  #replace NA with 0
  
#calculate frequency and percentage of different pattern
b <- 
  a %>% 
  unite(new_col, -c(idp, first_drug, total_seq)) %>%   #paste together
  group_by( new_col) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  mutate(per = round(n/sum(n) * 100, dig = 2)) %>% 
  mutate(cum_per = cumsum(per))
  
# Create some baseline tables ---------------------------------------------
#select codeine intiator
codeine_switch_cat <- 
  pattern_cod_tra_stage4 %>% 
  filter(first_drug == "codeine" ) %>% 
  group_by(switchs) %>% 
  summarise(n = n_distinct(idp)) %>%  
  ungroup() %>% 
  mutate(sum = sum(n)) %>% 
  mutate(per = substr(round(n/sum * 100, dig = 2), 1,5)) %>% 
  mutate(combine = paste(n, " (", per,")", sep = ""))


codeine_dispense_times_cat <- 
  pattern_cod_tra_stage4 %>% 
  filter(first_drug == "codeine" ) %>% 
  mutate(total_before_switch_group = case_when(total_before_switch > 5 ~ 6,
                                     TRUE ~ total_before_switch)) %>% 
  group_by(total_before_switch_group) %>% 
  summarise(n = n_distinct(idp)) %>% 
  ungroup() %>% 
  mutate(per = round(n/sum(n) * 100, dig = 2), 1,5) %>% 
  mutate(cum_per = cumsum(per)) %>% 
  mutate(combine = paste(n, " (", per,")", sep = "")) 
 
codeine_dispense_times_num <- 
  pattern_cod_tra_stage4 %>% 
  filter(first_drug == "codeine" ) %>% 
  distinct(idp, total_before_switch) %>% 
  summarise(mean = mean(total_before_switch))

#select tramadol  intiator 
tramadol_switch_cat <- 
  pattern_cod_tra_stage4 %>% 
  filter(first_drug == "tramadol" ) %>% 
  group_by(switchs) %>% 
  summarise(n = n_distinct(idp)) %>%  
  ungroup() %>% 
  mutate(sum = sum(n)) %>% 
  mutate(per = substr(round(n/sum * 100, dig = 2), 1,5)) %>% 
  mutate(combine = paste(n, " (", per,")", sep = ""))


tramadol_dispense_times_cat <- 
  pattern_cod_tra_stage4 %>% 
  filter(first_drug == "tramadol" ) %>% 
  mutate(total_before_switch_group = case_when(total_before_switch > 5 ~ 6,
                                     TRUE ~ total_before_switch)) %>% 
  group_by(total_before_switch_group) %>% 
  summarise(n = n_distinct(idp)) %>% 
  ungroup() %>% 
  mutate(per = round(n/sum(n) * 100, dig = 2), 1,5) %>% 
  mutate(cum_per = cumsum(per)) %>% 
  mutate(combine = paste(n, " (", per,")", sep = "")) 
 


tramadol_dispense_times_num <- 
  pattern_cod_tra_stage4 %>% 
  filter(first_drug == "tramadol" ) %>% 
  distinct(idp, total_before_switch) %>% 
  summarise(mean = mean(total_before_switch))




# Plot Cumulative Frequency Graph--------------------------------------------------------------------
#R: Cumulative Frequency Graph
#http://stats4stem.weebly.com/r-cumulative-frequency-graph.html
codeine_cum_plot <- 
  codeine_dispense_times_cat %>% 
  ggplot(aes(x = factor(total_before_switch_group), weight = per)) +
  geom_bar(width = 0.5, fill = "blue") +
  scale_x_discrete(expand = c(0.1,0),label = c("1","2","3","4","5",">=6"))+ 
  geom_point(aes(x = total_before_switch_group, y = cum_per)) +
  geom_line(aes(x = total_before_switch_group, y = cum_per, group = 1)) +
  # NB: Must use "group = 1"
  labs(x = "Number of dispensations", 
       y = "Relative frequency", 
       subtitle  = "Incident codeine users cohort") +
  theme(
    text = element_text(family = "Candara", colour = "black"),
    plot.subtitle = element_text(hjust = 0.5))

tramadol_cum_plot <- 
  tramadol_dispense_times_cat %>% 
  ggplot(aes(x = factor(total_before_switch_group), weight = per)) +
  geom_bar(width = 0.5, fill = "blue") +
  scale_x_discrete(expand = c(0.1,0),label = c("1","2","3","4","5",">=6"))+ 
  geom_point(aes(x = total_before_switch_group, y = cum_per)) +
  geom_line(aes(x = total_before_switch_group, y = cum_per, group = 1)) +
  # NB: Must use "group = 1"
  labs(x = "Number of dispensations", 
       y = "Relative frequency", 
       subtitle  = "Incident tramadol users cohort") +
  theme(
    text = element_text(family = "Candara", colour = "black"),
    plot.subtitle = element_text(hjust = 0.5))


save_test_incidence <- plot_grid(codeine_cum_plot, tramadol_cum_plot,ncol = 2 ,align = "v")
save_test_incidence

# save dataset ------------------------------------------------------------

write.csv(plotting_df, "D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/Summary data/number_distribution_tramadol.csv")

