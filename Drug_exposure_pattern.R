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


Denominator_data <- read_delim("D:/DPhil/Data_raw/OPIODU/OPIOIDES_entregable_poblacio_denominadors_20191219_143606.txt", 
                               delim = "|",
                               col_names = TRUE)
#dictionary datasets
Dic_analgesics <- read_excel("D:/DPhil/Project_Opioid_use/Notes/Dic_analgesics.xlsx")


# Little explore with raw dta ---------------------------------------------
head(billing)
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


#R: Cumulative Frequency Graph
#http://stats4stem.weebly.com/r-cumulative-frequency-graph.html
max.data <- max(number_tra$total_seq)
min.data <- min(number_tra$total_seq)
breaks=c(1:6, max.data+1)
cut.data=cut(number_tra$total_seq, breaks, right=FALSE)

frequency=table(cut.data)
relative.frequency=frequency/sum(frequency)
cummul.freq=cumsum(frequency)
cummul.percentile=cummul.freq/max(cummul.freq)

plotting_df <- 
  cbind(frequency,relative.frequency,cummul.freq, cummul.percentile) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column()

plotting_df %>% 
  ggplot(aes(x = rowname, weight = relative.frequency)) +
  geom_bar(width = 0.5, fill = "blue") +
  scale_x_discrete(label = c("1","2","3","4","5", ">=6"))+ 
  scale_y_continuous(label = scales::percent) +
  geom_point(aes(x = rowname, y = cummul.percentile)) +
  geom_line(aes(x = rowname, y = cummul.percentile, group = 1)) +
  # NB: Must use "group = 1"
  labs(x = "", 
       y = "Relative frequency", 
       subtitle  = "A Pareto diagram for numbers of opioid prescription") +
    theme(
      text = element_text(family = "Candara", colour = "black"),
      plot.subtitle = element_text(hjust = 0.5))


# Explore switchers -------------------------------------------------------
set.seed(1)

sub_Denominator <- sample_frac(Denominator_data, 0.1)

#select all codeine and tramadol users, generate some auxiliary variables
pattern_cod_tra <- 
  Denominator_data %>% 
  left_join(billing, by = "idp") %>% 
  left_join(Dic_analgesics, by = c("billing_cod" = "ATC_code")) %>% 
  filter(!is.na(Drug), Drug %in% c("codeine","tramadol")) %>% 
  group_by(idp) %>% 
  arrange(bill_date) %>% 
  mutate(seq = row_number(), total_seq = n()) %>% # create two index
  ungroup() %>% 
  filter(total_seq <= 5) %>% #select cases with less than 5 times dispensation
  group_by(idp) %>% # quite important function in R: any()
  #generate first drug initiation
  mutate(initiate_drug = case_when( any(seq == 1 & Drug == "codeine") ~ "Codeine",
                                    any(seq == 1 & Drug == "tramadol") ~ "Tramadol")) %>% 
  ungroup() %>% 
  mutate(index = case_when(Drug == "codeine" ~ 1,
                           Drug == "tramadol" ~ -1)) %>% 
  group_by(idp) %>% 
  mutate(index_sum = sum(index)) %>% 
  ungroup() %>% 
  #generate switchers and no_switchers
  mutate(switchs = case_when((total_seq == index_sum | total_seq == -index_sum) ~ "No_switchers",
                            TRUE ~ "Switchers"))



a <- 
  pattern_cod_tra %>% 
  group_by(initiate_drug, switchs) %>% 
  summarise(n = n_distinct(idp)) %>%  
  group_by(initiate_drug) %>% 
  mutate(switch_per = n/sum(n), total = sum(n))

#select switchers
switchers <- 
  pattern_cod_tra %>% 
  filter(!(total_seq == index_sum | total_seq == -index_sum))

#select no_switchers
no_switchers <- 
  pattern_cod_tra %>% 
  filter((total_seq == index_sum | total_seq == -index_sum))




trial_data <- 
  pattern_cod_fen %>% 
  select(idp, seq, index) 

pattern_cod_fen_wide <- spread(trial_data, seq, index)
pattern_cod_fen_wide <- pattern_cod_fen_wide[,2:6]
pattern_cod_fen_wide[is.na(pattern_cod_fen_wide)] <- 0
pattern_cod_fen_matrix <- as.matrix(pattern_cod_fen_wide)

heatmap(pattern_cod_fen_matrix, Colv = NA, scale="column")

is.numeric(pattern_cod_fen_matrix)

ggplot(data = trial_data, aes(x = seq, y = idp, fill = index ))+
  geom_tile()+
  theme(axis.text.y =element_blank(),
        axis.ticks.y  =element_blank())
  

# save dataset ------------------------------------------------------------

write.csv(plotting_df, "D:/DPhil/Project_Opioid_use/Analysis/Comparative effectiveness and safety/Summary data/number_distribution_tramadol.csv")

