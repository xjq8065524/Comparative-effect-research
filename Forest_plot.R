install.packages("forestplot")
install.packages( "DataCombine")
install.packages( "meta")
library(forestplot)
library(scales)
library(grid)
library(gridExtra)
library( DataCombine)
library( meta)
# ATT phrase only ---------------------------------------------------------
forest <- 
  survial_summary %>% 
  ungroup() %>% 

  add_row( outcome_label = c(unique(survial_summary$outcome_label))) %>% 
  mutate( outcome_label = factor( outcome_label, 
                                  levels = c("constipation", "sleep_disorders", "delirium", "opioid_abuse", "cardiac_arrhythmia", "myocardial_infarction", "stroke", "falls", "fracturas", "all_cause_mortality", "whole" ),
                                  labels = c("Constipation", "Sleep disorders", "Delirium", "Opioid abuse", "Cardiac arrhythmia", "Myocardial infarction", "Stroke", "Falls", "Fractures", "All cause mortality", "Any above outcomes" ))) %>% 

  mutate( stage = case_when( grepl("current", variable) ~ "current",
                             grepl("recent", variable) ~ "recent",
                             grepl("past", variable) ~ "past",
                             TRUE ~ "space"))  %>% 
  mutate( label = case_when( stage == "current" ~ "     Acute/short",
                                stage == "recent" ~ "     Medium",
                                stage == "past" ~ "     Long-term",
                                TRUE ~ as.character(outcome_label))) %>%   

  mutate(stage = factor( stage, levels = c( "space","current","recent","past"))) %>% 
  mutate(bold = case_when( is.na( group_label) ~ TRUE,
                           TRUE ~ FALSE)) %>% 
  arrange( outcome_label, stage) 

# Cochrane data from the 'rmeta'-package
graph_data <- 
  structure(list(
    mean  = c(NA, NA, NA, as.numeric(forest$estimate)), 
    lower = c(NA, NA, NA, as.numeric(forest$lower)),
    upper = c(NA, NA, NA, as.numeric(forest$upper))),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -47L), 

    class = "data.frame")



tabletext<-cbind( 
  c("Serious", "adverse outcomes", "", forest$label),
  c("Tramadol", "Cohort size", "(n)", forest$n.y),
  c("", "Events", "(n)", forest$events.y),
  c("", "Mean follow-up", "(pys)", forest$mean_follow.y),
  c("", "Incidence", "(per 1000 pys)", forest$rate.y),
  c(rep(NA, 47)),
  c("Codeine", "Cohort size", "(n)", forest$n.x),
  c("", "Events", "(n)", forest$events.x),
  c("", "Mean follow-up", "(pys)",forest$mean_follow.x),
  c("", "Incidence", "(per 1000 pys)", forest$rate.x),
  c("Tramadol vs Codeine", "PS-matching HR", "(95% CI)", forest$CI))



# tiff(file = 'Figures/forestplot_add_outcomes_same_sample.tiff',
#     units = "cm",
#     width = 26,
#     height = 20,
#     compression = "lzw",
#     res = 300)


forestplot(tabletext, 
           graph_data,
           new_page = TRUE,
           
           txt_gp = fpTxtGp( label = gpar( fontfamily = "Candara", cex = 0.6),
                             ticks = gpar( fontfamily = "Candara", cex = 0.5)),
           is.summary = c(rep(TRUE,3), forest$bold),
           boxsize = 0.3,
           col=fpColors(box=c("royalblue"), line=c("royalblue"), zero = "black"),
           
           colgap = unit(2, "mm"),
           

           graphwidth = unit(65, "mm"),
           lineheight = unit( 4.2, "mm"),
           
          
           graph.pos = 11,
           hrzl_lines = list("1" = gpar(lty=1, lwd=1,columns=c(1:12)),
                             "2" = gpar(lty=1, columns=c(2:5, 7:10, 12)), 
                             "4" = gpar(lty=1, lwd=1, columns=1:10, 12)),
                            
           lwd.zero = gpar(lwd=1),
           clip=c(0.1,3.5), 
           xticks = c( 0.5, 1, 1.5, 3, 6),
           grid = structure( c(1.5, 3), gp = gpar(lty =2, lwd=1, col = "grey")),
           
           xlog=TRUE)

dev.off()


# ATT and ITT combined -----------------------------------------------------
ATT_ITT_forest <- 
  ATT_survial_summary %>% 
  bind_rows( ITT_survial_summary) %>% 
  filter( group_label %in% c( "current_outcome_dataset", "one_year_outcome_dataset", "two_year_outcome_dataset", "three_year_outcome_dataset")) %>% 
  ungroup() %>% 
  add_row( outcome_label = c(unique(ITT_survial_summary$outcome_label))) %>% 
  mutate( outcome_label = factor( outcome_label, 
                                  levels = c("Primary outcomes" ,"myocardial_infarction", "stroke", "fracturas", "all_cause_mortality", "Secondary outcomes", "cardiac_arrhythmia", "falls", "constipation", "sleep_disorders", "delirium", "opioid_abuse",  "whole" ),
                                  labels = c("Primary outcomes", "Myocardial infarction", "Stroke", "Fractures", "All cause mortality", "Secondary outcomes", "Cardiac arrhythmia",  "Falls", "Constipation", "Sleep disorders", "Delirium", "Opioid abuse",   "Any above outcomes" ))) %>% 
  
  mutate( stage = case_when( grepl("current", variable) ~ "OT",
                             grepl("one_year", variable) ~ "ITT (0-1 year)",
                             grepl("two_year", variable) ~ "ITT (1-2 year)",
                             grepl("three_year", variable) ~ "ITT (2-3 year)",
                             TRUE ~ "space")) %>%
  mutate(stage = factor( stage, levels = c( "space","OT", "ITT (0-1 year)", "ITT (1-2 year)", "ITT (2-3 year)"))) %>% 

  filter( outcome_label != "Any above outcomes") %>% 
  add_row ( outcome_label = factor( c("Primary outcomes", "Secondary outcomes"))) %>% 
  add_row ( ) %>% 
  arrange( outcome_label, stage) %>% 
  arrange( outcome_label, stage) %>% 
  mutate( label = case_when( stage == "OT" ~ "  OT",
                             stage == "ITT (0-1 year)" ~ "  ITT (0-1 year)",
                             stage == "ITT (1-2 year)" ~ "  ITT (1-2 year)",
                             stage == "ITT (2-3 year)" ~ "  ITT (2-3 year)",
                             TRUE ~ as.character(outcome_label))) %>% 
  mutate(bold = case_when( is.na( group_label) ~ TRUE,
                           TRUE ~ FALSE)) 

# Cochrane data from the 'rmeta'-package
graph_data <- 
  structure(list(
    mean  = c(NA, NA, NA, as.numeric(ATT_ITT_forest$estimate)), 
    lower = c(NA, NA, NA, as.numeric(ATT_ITT_forest$lower)),
    upper = c(NA, NA, NA, as.numeric(ATT_ITT_forest$upper))),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -(nrow(ATT_ITT_forest) + 3)), 
    class = "data.frame")




tabletext<-cbind( 
  c("Serious", "adverse outcomes", "", ATT_ITT_forest$label),
  c("Tramadol", "Cohort size", "(n)", ATT_ITT_forest$n.y),
  # c("", "Events", "(n)", ATT_ITT_forest$events.y),
  c("", "Follow-up", "(mean pys)", ATT_ITT_forest$mean_follow.y),
  c("", "Incidence", "(1000 pys)", ATT_ITT_forest$rate.y),
  c(rep(NA, nrow(ATT_ITT_forest) + 3)),
  c("Codeine", "Cohort size", "(n)", ATT_ITT_forest$n.x),
  # c("", "Events", "(n)", ATT_ITT_forest$events.x),
  c("", "Follow-up", "(mean pys)",ATT_ITT_forest$mean_follow.x),
  c("", "Incidence", "(1000 pys)", ATT_ITT_forest$rate.x),
  c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", ATT_ITT_forest$CI))



tiff(file = 'Figures/forestplot_ATT_ITT_three_years.tiff',
     units = "cm",
     width = 14.6,
     height = 19,
     compression = "lzw",
     res = 300)

forestplot(tabletext, 
           graph_data,
           new_page = TRUE,
           
           txt_gp = fpTxtGp( label = gpar( fontfamily = "Candara", cex = 0.45),
                             ticks = gpar( fontfamily = "Candara", cex = 0.4)),
           is.summary = c(rep(TRUE,3), ATT_ITT_forest$bold),
           boxsize = 0.3,
           col=fpColors(box=c("royalblue"), line=c("royalblue"), zero = "black"),
           
           colgap = unit(1, "mm"),
           
           
           graphwidth = unit(38, "mm"),
           lineheight = unit( 3.2, "mm"),
           
           
           graph.pos = 9,
           hrzl_lines = list("1" = gpar(lty=1, lwd=1,columns=c(1:10)),
                             "2" = gpar(lty=1, columns=c(2:4, 6:8, 10)), 
                             "4" = gpar(lty=1, lwd=1, columns=c(1:8, 10)),
                             "25" = gpar(lty=1, lwd=1, columns=c(1:8, 10))),
           
           lwd.zero = gpar(lwd=1),
           clip=c(0.8,3.5), 
           xticks = c( 0.8, 1, 1.5, 3, 6),
           # grid = structure( c(1.5, 3), gp = gpar(lty =2, lwd=1, col = "grey")),
           
           xlog=TRUE)

dev.off()





# ITT only -----------------------------------------------------
ITT_forest <- 
  ITT_survial_summary %>% 
  ungroup() %>% 
  add_row( outcome_label = c("Primary outcomes", "Secondary outcomes")) %>% 
  mutate( outcome_label = factor( outcome_label, 
                                  levels = c("Primary outcomes", "composite_CVD","stroke",  "fracturas",  "all_cause_mortality",
                                             "Secondary outcomes", "cardiac_arrhythmia", "constipation",  "delirium", "sleep_disorders", "falls", "opioid_abuse"),
                                  labels = c("Primary outcomes", "Composite CVEs","Stroke",  "Fracture",  "All-cause mortality",
                                             "Secondary outcomes  ","Cardiac arrhythmia", "Constipation",  "Delirium", "Sleep disorders", "Fall", "Opioid abuse"))) %>% 
  # 
  # mutate(stage = factor( stage, levels = c( "space","On-treatment","Intention-to-treatment"))) %>% 
  mutate(bold = case_when( is.na(group_label) ~ TRUE,
                           TRUE ~ FALSE)) %>% 
  arrange( outcome_label)
# Cochrane data from the 'rmeta'-package
graph_data <- 
  structure(list(
    mean  = c(NA, NA, NA, as.numeric(ITT_forest$estimate)), 
    lower = c(NA, NA, NA, as.numeric(ITT_forest$lower)),
    upper = c(NA, NA, NA, as.numeric(ITT_forest$upper))),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -(nrow(ITT_forest) + 3)), 
    class = "data.frame")

tabletext<-cbind( 
  c("", "", "Outcomes", as.character(ITT_forest$outcome_label)),
  c("", "No. at", "Risk", ITT_forest$n.x),
  c(rep("  ", nrow(ITT_forest) + 3)),
  c("Tramadol", "No. of", "Events", ITT_forest$events.y),
  c("", "Incidence", "Rate", ITT_forest$rate.y),
  c(rep("  ", nrow(ITT_forest) + 3)),
  c("Codeine", "No. of", "Events", ITT_forest$events.x),
  c("", "Incidence", "Rate", ITT_forest$rate.x),
  c(rep("  ", nrow(ITT_forest) + 3)),
  c("Absolute Risk", "Increase, ‰", "(95% CI)", ITT_forest$dif_CI.x),
  c(rep("  ", nrow(ITT_forest) + 3)),
  c("", "Hazard ratio", "(95% CI)", ITT_forest$CI)
  )
  

test <- function(){
  forestplot(
    labeltext=tabletext,
    graph_data,
    new_page = TRUE,
    align = c(rep("l",8)),
    
    txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 8, cex = 0.68),
                      ticks = gpar( fontfamily = "sans", fontsize = 8, cex = 0.68),
                      xlab = gpar( fontfamily = "sans", fontsize = 8, cex = 0.68)),
    
    is.summary = c(rep(TRUE,3), ITT_forest$bold),
    boxsize = 0.25,
    col=fpColors(box=c("royalblue"), line=c("royalblue"), zero = "black"),
    
    colgap = unit(1.5, "mm"),
    
    
    graphwidth = unit(38, "mm"),
    lineheight = unit(4.2, "mm"),
    
    
    # graph.pos = 9,
    # hrzl_lines = list("1" = gpar(lty=1, lwd=0.5, columns=c(1:13)),
    #                   "2" = gpar(lty=1, lwd=0.5, columns=c(4:5, 7:8)),
    #                   "4" = gpar(lty=1, lwd=0.5, columns=c(1:13)),
    #                   "6" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "7" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "8" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "9" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "11" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "12" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "13" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "14" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "15" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
    #                   "16" = gpar(lty=1, lwd=0.5, columns=c(1:12))
    # ),
    
    lwd.xaxis = 0.5,
    lwd.zero = gpar(lwd=0.5),
    clip=c(0.5,3.5), 
    xticks = c( 0.5, 1, 1.5, 2, 3, 4),
    xticks.digits = 1,
    
    xlab = "Hazard Ratio (95% CI)",
    
    xlog=TRUE)
  
}
test()


tiff(file = 'Figures/forestplot_main_results.tiff',
     units = "cm",
     width = 16,
     height = 6.5,
     compression = "lzw",
res = 800)
test <- function(){
  forestplot(
    labeltext=tabletext,
    graph_data,
    new_page = TRUE,
    align = c(rep("l",8)),
    
    txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 8, cex = 0.68),
                      ticks = gpar( fontfamily = "sans", fontsize = 8, cex = 0.68),
                      xlab = gpar( fontfamily = "sans", fontsize = 8, cex = 0.68)),
    
    is.summary = c(rep(TRUE,3), ITT_forest$bold),
    boxsize = 0.25,
    col=fpColors(box=c("royalblue"), line=c("royalblue"), zero = "black"),
    
    colgap = unit(1.5, "mm"),
    
    
    graphwidth = unit(38, "mm"),
    lineheight = unit(4.2, "mm"),
    
    
    # graph.pos = 9,
    hrzl_lines = list("1" = gpar(lty=1, lwd=0.5, columns=c(1:13)),
                      "2" = gpar(lty=1, lwd=0.5, columns=c(4:5, 7:8)),
                      "4" = gpar(lty=1, lwd=0.5, columns=c(1:13)),
                      "6" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "7" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "8" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "9" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "11" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "12" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "13" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "14" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "15" = gpar(lty=1, lwd=0.5, columns=c(1:12), col = "grey"),
                      "16" = gpar(lty=1, lwd=0.5, columns=c(1:12))
    ),
    
    lwd.xaxis = 0.5,
    lwd.zero = gpar(lwd=0.5),
    clip=c(0.5,3.5), 
    xticks = c( 0.5, 1, 1.5, 2, 3, 4),
    xticks.digits = 1,
    
    xlab = "Hazard Ratio (95% CI)",
    
    xlog=TRUE)
  
}
test()
# upViewport()
downViewport( "Line_2_25")
grid.text("Favors",
          x=unit(0.05, "npc"), 
          y=unit(0.2, "npc"),
          just=c("left", "bottom"),
          gp = gpar(col="black", 
                    fontsize = 8, 
                    fontface = "bold",
                    cex = 0.75))
upViewport()
downViewport( "Line_3_25")
grid.text("Tramadol",
          x=unit(0.05, "npc"), 
          y=unit(0.2, "npc"),
          just=c("left", "bottom"),
          gp = gpar(col="black", 
                    fontsize = 8, 
                    fontface = "bold",
                    cex = 0.75))

upViewport()
downViewport( "Line_2_25")
grid.text("Favors",
          x=unit(0.5, "npc"), 
          y=unit(0.2, "npc"),
          just=c("left", "bottom"),
          gp = gpar(col="black", 
                    fontsize = 8, 
                    fontface = "bold",
                    cex = 0.75))

upViewport()
downViewport( "Line_3_25")
grid.text("Codeine",
          x=unit(0.5, "npc"), 
          y=unit(0.2, "npc"),
          just=c("left", "bottom"),
          gp = gpar(col="black", 
                    fontsize = 8, 
                    fontface = "bold",
                    cex = 0.75))

dev.off()








# ITT primary table (24/06)-------------------------------------------------------
ITT_forest <- 
  ITT_survial_summary %>% 
  ungroup() %>% 
  mutate( outcome_label = factor( outcome_label, 
                                  levels = c("composite_CVD", "fracturas",  "all_cause_mortality",
                                              "constipation", "falls", "sleep_disorders",  "delirium", "opioid_abuse"),
                                  labels = c("Cardiovascular events ", "Fracture",  "All-cause mortality",
                                              "Constipation", "Fall","Sleep disorders",   "Delirium", "Opioid abuse"))) %>% 
  # 
  # mutate(stage = factor( stage, levels = c( "space","On-treatment","Intention-to-treatment"))) %>% 
  mutate(bold = case_when( is.na(group_label) ~ TRUE,
                           TRUE ~ FALSE)) %>% 
  arrange( outcome_label)
# Cochrane data from the 'rmeta'-package
graph_data <- 
  structure(list(
    mean  = c(NA, NA, NA, NA,as.numeric(ITT_forest$estimate)), 
    lower = c(NA, NA, NA, NA,as.numeric(ITT_forest$lower)),
    upper = c(NA, NA, NA, NA,as.numeric(ITT_forest$upper))),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -(nrow(ITT_forest) + 4)), 
    class = "data.frame")

tabletext<-cbind( 
  c("", "", "", "Outcomes", as.character(ITT_forest$outcome_label)),
  # c("", "No. at", "Risk", ITT_forest$n.x),
  # c(rep("  ", nrow(ITT_forest) + 3)),
  c("Tramadol", "(n = 203164)", "No. of",  "Events", ITT_forest$events.y),
  c("", "", "Incidence", "Rate", ITT_forest$rate.y),
  c(rep("  ", nrow(ITT_forest) + 4)),
  c("Codeine", "(n = 203164)", "No. of", "Events", ITT_forest$events.x),
  c("",  "", "Incidence", "Rate", ITT_forest$rate.x),
  c(rep("  ", nrow(ITT_forest) + 4)),
  c("", "", "Rate Difference", "(95% CI)", ITT_forest$dif_CI.x),
  c(rep("  ", nrow(ITT_forest) + 4)),
  c("", "", "Hazard Ratio", "(95% CI)", ITT_forest$CI)
)



png(file = 'Figures/Primary_results.png',
     units = "cm",
     width = 18.2,
     height = 8,
     res = 800)

test <- function(){
  forestplot(
    labeltext=tabletext,
    graph_data,
    new_page = TRUE,
    align = c(rep("l",7), "c", "c", "c"),
    
    txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 7),
                      ticks = gpar( fontfamily = "sans", fontsize = 7),
                      xlab = gpar( fontfamily = "sans", fontsize = 10)),
    
    is.summary = c(rep(TRUE,4), ITT_forest$bold),
    boxsize = 0.25,
    col=fpColors(box=c("royalblue"), line=c("royalblue"), zero = "black"),
    
    colgap = unit(1.5, "mm"),
    
    
    graphwidth = unit(38, "mm"),
    lineheight = unit(6, "mm"),
    
    
    graph.pos = 11,
    hrzl_lines = list("1" = gpar(lty=1, lwd=0.5, columns=c(1:10)),
                      "3" = gpar(lty=1, lwd=0.5, columns=c(2:3, 5:6)),
                      "5" = gpar(lty=1, lwd=0.5, columns=c(1:10)),
                      "6" = gpar(lty=1, lwd=0.5, columns=c(1:10), col = "grey"),
                      "7" = gpar(lty=1, lwd=0.5, columns=c(1:10), col = "grey"),
                      "8" = gpar(lty=1, lwd=0.5, columns=c(1:10), col = "grey"),
                      "9" = gpar(lty=1, lwd=0.5, columns=c(1:10), col = "grey"),
                      "10" = gpar(lty=1, lwd=0.5, columns=c(1:10), col = "grey"),
                      "11" = gpar(lty=1, lwd=0.5, columns=c(1:10), col = "grey"),
                      "12" = gpar(lty=1, lwd=0.5, columns=c(1:10), col = "grey"),
                      "13" = gpar(lty=1, lwd=0.5, columns=c(1:10))
    ),
    
    lwd.xaxis = 0.5,
    lwd.zero = gpar(lwd=0.5),
    clip=c(0.2,5), 
    xticks = c( 0.2, 0.5, 1, 1.5, 2.5, 5),
    xticks.digits = 1,
    xlab = "Hazard Ratio (95% CI)\n <---Favor Tramadol   Favor Codeine--->",
    
    xlog=TRUE)
  
}
test()

dev.off()


# Interaction in ITT ------------------------------------------------------
c("composite_CVD", "fracturas", "all_cause_mortality")

plot_data_preparation <- function( outcome){
  interaction_forest <- 
    survial_summary %>% 
    filter( separate_label == outcome) %>% 
    ungroup() %>% 
    add_row( outcome_label = c(unique(survial_summary$outcome_label))) %>% 
    mutate( outcome_label = factor( outcome_label, 
                                    levels = c("initiation_age", "sex",  "cancer","any_pain",  "cough", "any_MSK", "cci_group", "any_Psychotropic", "any_NSAIDs", "overall" ),
                                    labels = c("Age", "Sex",  "Cancer", "Back pain",  "Chronic cough", "Musculoskeletal diseases", "Charlson comorbidity index", "Psychotropic drug use", "Other analgesics use","Overall"))) %>% 
    #in order to set NA as an separate factor level
    mutate( group_label = case_when(is.na(group_label) ~ " ",
                                    TRUE ~ group_label)) %>% 
    mutate( group_label = factor(group_label,
                                 levels = c(" ", "overall", "18-39", "40-59", ">=60", "H", "D",  "1", "0", "0dis", "1-2dis", ">=3dis"),
                                 labels = c(" ", "Overall", "  18-39", "  40-59", "  >=60", "  Male", "  Female", "  Yes", "  No",  "  0", "  1-2", "  >=3"))) %>% 
    
    mutate( label = case_when( group_label == " " ~ as.character(outcome_label),
                               TRUE ~ as.character(group_label))) %>% 
    # 
    # mutate(stage = factor( stage, levels = c( "space","On-treatment","Intention-to-treatment"))) %>% 
    mutate(bold = case_when( group_label == " " ~ TRUE,
                             TRUE ~ FALSE)) %>%
    mutate( label = case_when( label == "Overall" & is.na(CI) ~ NA_character_,
                               TRUE ~ label)) %>% 
    mutate( bold = case_when( label == "Overall" & bold == FALSE ~ TRUE,
                              TRUE ~ bold)) %>% 
    arrange( outcome_label, group_label) %>% 
    mutate( box = case_when( row_number() == n() ~ 0.3,
                             TRUE ~ 0.25)) 
  
  
  # Cochrane data from the 'rmeta'-package
  graph_data <- 
    structure(list(
      mean  = c(NA, NA, NA, as.numeric(interaction_forest$estimate)), 
      lower = c(NA, NA, NA, as.numeric(interaction_forest$lower)),
      upper = c(NA, NA, NA, as.numeric(interaction_forest$upper))),
      .Names = c("mean", "lower", "upper"), 
      row.names = c(NA, -(nrow(interaction_forest) + 3)), 
      class = "data.frame")
  
  tabletext<-cbind( 
    c("", "", "Subgroup", interaction_forest$label),
    c("", "No. of", "Patients", paste(interaction_forest$n.y, " /",interaction_forest$n.x,  sep = "")),
    c("Tramadol/Codeine", "No. of", "Events", paste(interaction_forest$events.y, " /",interaction_forest$events.x,  sep = "")),
    c("", "Incidence", "Rate  ", paste(interaction_forest$rate.y, " /",interaction_forest$rate.x,  sep = "")),
    c(rep("   ", nrow(interaction_forest) + 3)),
    c("", "Risk difference", "(95% CI)", interaction_forest$RD_CI),
    c(rep("   ", nrow(interaction_forest) + 3)),
    c("", "Hazard ratio", "(95% CI)", interaction_forest$CI),
    c(rep("  ", nrow(interaction_forest) + 3)))
  
  
  # 
  # tabletext<-cbind( 
  #   c("", "Subgroup", "", interaction_forest$label),
  #   c("Tramadol/codeine", "No. at risk", "", paste(interaction_forest$n.y, interaction_forest$n.x, sep = "/")),
  #   c("  ", "No. of events", "per 1000 Person-years", paste(interaction_forest$rate.y, interaction_forest$rate.x, sep = "/")))
  #   # c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", interaction_forest$CI))
  
  tabletext[tabletext == "NA /NA"] <- NA
  
  # tabletext<-cbind( 
  #   c("", "Subgroup", "", interaction_forest$label),
  #   c("Tramadol", "N. at risk", "", interaction_forest$n.y),
  #   c("", "No. of Events", "(1000 Person-years)", interaction_forest$rate.y),
  #   c(rep(NA, nrow(interaction_forest) + 3)),
  #   c("Codeine", "N. at risk", "", interaction_forest$n.x),
  #   c("", "No. of Events", "(1000 Person-years)", interaction_forest$rate.x),
  #   c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", interaction_forest$CI))
  
  return(plot_data = list(interaction_forest = interaction_forest, graph_data = graph_data, tabletext = tabletext))
}

composite_CVD <- plot_data_preparation( outcome = "composite_CVD")
fracturas <- plot_data_preparation( outcome = "fracturas")
all_cause_mortality <- plot_data_preparation( outcome = "all_cause_mortality")



plot_func_interaction <- function(input){
  forestplot(input$tabletext, 
             input$graph_data,
             align = c("l","c","c","c", "c", "c"),
             new_page = TRUE,
             
             txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 6),
                               ticks = gpar( fontfamily = "sans", fontsize = 8),
                               xlab = gpar( fontfamily = "sans", fontsize = 8)),
             is.summary = c(rep(TRUE,3), rep(FALSE, length(input$interaction_forest$bold))),
             boxsize = c(rep(TRUE,3), input$interaction_forest$box),
             col=fpColors( box=c("royalblue"), 
                           line=c("royalblue"), 
                           zero = "black", 
                           summary = "royalblue"),
             
             colgap = unit(0, "mm"),
             graphwidth = unit(30, "mm"),
             lineheight = unit( 3.2, "mm"),
             graph.pos = 10,
             hrzl_lines = list(
               "1" = gpar(lty=1, lwd=0.5, columns=c(1:10)),
               "2" = gpar(lty=1, lwd=0.5, columns=c(2:4)),
               # "3" = gpar(lty=1, lwd=0.5, columns=c(2:6)),
               "4" = gpar(lty=1, lwd=0.5, columns=c(1: 10)),
               "6" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "7" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "10" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "13" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "16" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "19" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "22" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "23" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "26" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "29" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey"),
               "32" = gpar(lty=1, lwd=0.25, columns=c(1:8), col = "grey")
               ),
             lwd.xaxis = 0.5,
             lwd.zero = gpar(lwd=0.5),
             clip=c(0.5,3.5), 
             xticks = c( 0.5, 1, 1.5, 2, 3, 4),
             grid = structure( input$graph_data$mean[nrow(input$graph_data)], gp = gpar(lty =2, lwd=0.75, col = "grey")),
             
             xlab = "Hazard Ratio (95% CI)",
             xlog=TRUE)
}

plot_func_interaction(input = composite_CVD)
plot_func_interaction(input = fracturas)
plot_func_interaction(input = all_cause_mortality)



png(file = 'Figures/subgroup_mortality.png',
    units = "cm",
    width = 16,
    height = 10,
    res = 500)

plot_func_interaction(input = all_cause_mortality)

dev.off()


 # grid.rect(gp = gpar(lty = "dashed"))
 # vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5, just = c("left", "bottom"))
 # vp2 <- viewport(x = 0.5, y = 0.5, w = 0.5, h = 0.5, just = c("left", "bottom"))
 # vp3 <- viewport(x = 0, y = 0, w = 0.5, h = 0.5, just = c("left", "bottom"))
 # vp4 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5, just = c("left", "bottom"))
 # pushViewport(vp1)
 # grid.rect(gp = gpar(col = "grey", lwd=0.5))
 # # grid.text("Some drawing in graphics region 1", y = 0.8)
 # plot_func_interaction(input = myocardial_infarction)
 # upViewport()
 # 
 # pushViewport(vp2)
 # grid.rect(gp = gpar(col = "grey", lwd=0.5))
 # plot_func_interaction(input = stroke)
 # upViewport()
 # 
 # pushViewport(vp3)
 # grid.rect(gp = gpar(col = "grey", lwd=0.5))
 # plot_func_interaction(input = fracturas)
 # upViewport()
 # 
 # pushViewport(vp4)
 # grid.rect(gp = gpar(col = "grey", lwd=0.5))
 # plot_func_interaction(input = all_cause_mortality)
 # upViewport()
 
 













# Interaction in ITT (simple version) ------------------------------------------------------
c("composite_CVD", "fracturas", "all_cause_mortality")

plot_data_preparation <- function( outcome){
  interaction_forest <- 
    survial_summary %>% 
    filter( separate_label == outcome) %>% 
    ungroup() %>% 
    add_row( outcome_label = c(unique(survial_summary$outcome_label))) %>% 
    mutate( outcome_label = factor( outcome_label, 
                                    levels = c("initiation_age", "sex",  "cancer","any_pain", "cough", "any_MSK",   "cci_group", "any_Psychotropic", "any_NSAIDs", "overall" ),
                                    labels = c("Age", "Sex",  "Cancer", "Chronic pain","Chronic cough", "MSK diseases", "Charlson index", "Psychotropic drug use", "Other analgesics use","Overall"))) %>% 
    #in order to set NA as an separate factor level
    mutate( group_label = case_when(is.na(group_label) ~ " ",
                                    TRUE ~ group_label)) %>% 
    mutate( group_label = factor(group_label,
                                 levels = c(" ", "overall", "18-39", "40-59", ">=60", "H", "D",  "1", "0", "0dis", "1-2dis", ">=3dis"),
                                 labels = c(" ", "Overall", "  18-39", "  40-59", "  >=60", "  Male", "  Female", "  Yes", "  No",  "  0", "  1-2", "  >=3"))) %>% 
    
    mutate( label = case_when( group_label == " " ~ as.character(outcome_label),
                               TRUE ~ as.character(group_label))) %>% 
    # 
    # mutate(stage = factor( stage, levels = c( "space","On-treatment","Intention-to-treatment"))) %>% 
    mutate(bold = case_when( group_label == " " ~ TRUE,
                             TRUE ~ FALSE)) %>%
    mutate( label = case_when( label == "Overall" & is.na(CI) ~ NA_character_,
                               TRUE ~ label)) %>% 
    mutate( bold = case_when( label == "Overall" & bold == FALSE ~ TRUE,
                              TRUE ~ bold)) %>% 
    arrange( outcome_label, group_label) %>% 
    mutate( box = case_when( row_number() == n() ~ 0.3,
                             TRUE ~ 0.25)) 
  
  
  # Cochrane data from the 'rmeta'-package
  graph_data <- 
    structure(list(
      mean  = c(NA, NA, NA, as.numeric(interaction_forest$estimate)), 
      lower = c(NA, NA, NA, as.numeric(interaction_forest$lower)),
      upper = c(NA, NA, NA, as.numeric(interaction_forest$upper))),
      .Names = c("mean", "lower", "upper"), 
      row.names = c(NA, -(nrow(interaction_forest) + 3)), 
      class = "data.frame")
  
  tabletext<-cbind( 
    c("", "", "Subgroup", interaction_forest$label),
    c(rep("  ", nrow(interaction_forest) + 3)),
    c("No. of Patients", "(Incidence)", "Tramadol", paste(interaction_forest$n.y, " (",interaction_forest$rate.y, ")", sep = "")),
    c("", "", "Codeine", paste(interaction_forest$n.x, " (",interaction_forest$rate.x, ")", sep = "")),
    c(rep("  ", nrow(interaction_forest) + 3)))
  
  
  # 
  # tabletext<-cbind( 
  #   c("", "Subgroup", "", interaction_forest$label),
  #   c("Tramadol/codeine", "No. at risk", "", paste(interaction_forest$n.y, interaction_forest$n.x, sep = "/")),
  #   c("  ", "No. of events", "per 1000 Person-years", paste(interaction_forest$rate.y, interaction_forest$rate.x, sep = "/")))
  #   # c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", interaction_forest$CI))
  
  tabletext[tabletext == "NA (NA)"] <- NA
  
  # tabletext<-cbind( 
  #   c("", "Subgroup", "", interaction_forest$label),
  #   c("Tramadol", "N. at risk", "", interaction_forest$n.y),
  #   c("", "No. of Events", "(1000 Person-years)", interaction_forest$rate.y),
  #   c(rep(NA, nrow(interaction_forest) + 3)),
  #   c("Codeine", "N. at risk", "", interaction_forest$n.x),
  #   c("", "No. of Events", "(1000 Person-years)", interaction_forest$rate.x),
  #   c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", interaction_forest$CI))
  
  return(plot_data = list(interaction_forest = interaction_forest, graph_data = graph_data, tabletext = tabletext))
}

composite_CVD <- plot_data_preparation( outcome = "composite_CVD")
fracturas <- plot_data_preparation( outcome = "fracturas")
all_cause_mortality <- plot_data_preparation( outcome = "all_cause_mortality")


plot_func_interaction <- function(input){
  forestplot(input$tabletext, 
             input$graph_data,
             align = c("l","l","l","l"),
             new_page = FALSE,
             
             txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 6),
                               ticks = gpar( fontfamily = "sans", fontsize = 8),
                               xlab = gpar( fontfamily = "sans", fontsize = 8)),
             is.summary = c(rep(TRUE,3), rep(FALSE, length(input$interaction_forest$bold))),
             boxsize = c(rep(TRUE,3), input$interaction_forest$box),
             col=fpColors( box=c("royalblue"), 
                           line=c("royalblue"), 
                           zero = "black", 
                           summary = "royalblue"),
             
             colgap = unit(0, "mm"),
             graphwidth = unit(25, "mm"),
             lineheight = unit( 3.2, "mm"),
             graph.pos = 6,
             hrzl_lines = list(
               "1" = gpar(lty=1, lwd=0.5, columns=c(1:6)),
               "3" = gpar(lty=1, lwd=0.5, columns=c(2:4)),
               # "3" = gpar(lty=1, lwd=0.5, columns=c(2:6)),
               "4" = gpar(lty=1, lwd=0.5, columns=c(1: 6)),
               "6" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "7" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "10" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "13" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "16" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "19" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "22" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "25" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "26" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "29" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               "32" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey")
             ),
             lwd.xaxis = 0.5,
             lwd.zero = gpar(lwd=0.5),
             clip=c(0.5,3.5), 
             xticks = c( 0.5, 1, 1.5, 2, 3, 4),
             grid = structure( input$graph_data$mean[nrow(input$graph_data)], gp = gpar(lty =2, lwd=0.75, col = "grey")),
             
             xlab = "Hazard Ratio (95% CI)",
             xlog=TRUE)
}

plot_func_interaction(input = composite_CVD)
plot_func_interaction(input = fracturas)
plot_func_interaction(input = all_cause_mortality)



png(file = 'Figures/all_subgourps.png',
    units = "cm",
    width = 18,
    height = 26,
    res = 500)


grid.newpage()

widths <- unit(c(8.5, 0.5, 8.5), c("cm", "cm", "cm"))
heights <- unit(c(1, 11, 0.1, 1, 11), c("cm", "cm", "cm", "cm", "cm"))
lay <- grid.layout(nrow = 5, 
                   ncol = 3, 
                   widths=widths,
                   heights = heights)

vp <- viewport( width = unit(16, units = "cm"),
                height = unit( 28, units = "cm"),
                layout = lay)
pushViewport(vp)

pushViewport(viewport(layout.pos.row=1,
                      layout.pos.col=1))

grid.rect(x=unit(0, "cm"), y=unit(0.1, "cm"),
          width=unit(0.25, "cm"), height=unit(0.2, "cm"),
          just=c("left", "bottom"),
          gp=gpar(col= "black"))

grid.text(" A   Composite CVD events",
          x=unit(0, "cm"), y=unit(0.1, "cm"),
          just=c("left", "bottom"),
          gp = gpar( fontsize = 6))


upViewport()

pushViewport(viewport(layout.pos.row=2,
                      layout.pos.col=1))

plot_func_interaction(input = composite_CVD)
# grid.rect(gp=gpar(col=NA, fill=rgb(1,0,0,.5)))

upViewport()

pushViewport(viewport(layout.pos.row=1,
                      layout.pos.col=3))

grid.rect(x=unit(0, "cm"), y=unit(0.1, "cm"),
          width=unit(0.25, "cm"), height=unit(0.2, "cm"),
          just=c("left", "bottom"),
          gp=gpar(col= "black"))

grid.text(" B   Fractures",
          x=unit(0, "cm"), y=unit(0.1, "cm"),
          just=c("left", "bottom"),
          gp = gpar( fontsize = 6))


upViewport()

pushViewport(viewport(layout.pos.row=2,
                      layout.pos.col=3))
plot_func_interaction(input = fracturas)

upViewport()

pushViewport(viewport(layout.pos.row=4,
                      layout.pos.col=1))

grid.rect(x=unit(0, "cm"), y=unit(0.1, "cm"),
          width=unit(0.25, "cm"), height=unit(0.2, "cm"),
          just=c("left", "bottom"),
          gp=gpar(col= "black"))

grid.text(" C   All-cause mortality",
          x=unit(0, "cm"), y=unit(0.1, "cm"),
          just=c("left", "bottom"),
          gp = gpar( fontsize = 6))


upViewport()

pushViewport(viewport(layout.pos.row=5,
                      layout.pos.col=1))
plot_func_interaction(input = all_cause_mortality)


dev.off()



















# Interaction in ITT (24/06) ------------------------------------------------------
c("composite_CVD", "fracturas", "all_cause_mortality")

plot_data_preparation <- function( outcome){
  interaction_forest <- 
    survial_summary %>% 
    filter( separate_label == outcome) %>% 
    ungroup() %>% 
    add_row( outcome_label = c(unique(survial_summary$outcome_label))) %>% 
    mutate( outcome_label = factor( outcome_label, 
                                    levels = c("initiation_age", "sex",  "cancer","any_pain", "cough", "any_MSK",   "cci_group", "any_Psychotropic", "any_NSAIDs", "overall" ),
                                    labels = c("Age", "Sex",  "Cancer", "Chronic pain","Chronic cough", "MSK diseases", "Charlson index", "Psychotropic drug use", "Other analgesics use","Overall"))) %>% 
    #in order to set NA as an separate factor level
    mutate( group_label = case_when(is.na(group_label) ~ " ",
                                    TRUE ~ group_label)) %>% 
    mutate( group_label = factor(group_label,
                                 levels = c(" ", "overall", "18-39", "40-59", ">=60", "H", "D",  "1", "0", "0dis", "1-2dis", ">=3dis"),
                                 labels = c(" ", "Overall", "  18-39", "  40-59", "  >=60", "  Male", "  Female", "  Yes", "  No",  "  0", "  1-2", "  >=3"))) %>% 
    
    mutate( label = case_when( group_label == " " ~ as.character(outcome_label),
                               TRUE ~ as.character(group_label))) %>% 
    # 
    # mutate(stage = factor( stage, levels = c( "space","On-treatment","Intention-to-treatment"))) %>% 
    mutate(bold = case_when( group_label == " " ~ TRUE,
                             TRUE ~ FALSE)) %>%
    mutate( label = case_when( label == "Overall" & is.na(CI) ~ NA_character_,
                               TRUE ~ label)) %>% 
    mutate( bold = case_when( label == "Overall" & bold == FALSE ~ TRUE,
                              TRUE ~ bold)) %>% 
    arrange( outcome_label, group_label) %>% 
    mutate( box = case_when( row_number() == n() ~ 0.3,
                             TRUE ~ 0.25)) 
  
  
  # Cochrane data from the 'rmeta'-package
  graph_data <- 
    structure(list(
      mean  = c(NA, NA, as.numeric(interaction_forest$estimate)), 
      lower = c(NA, NA,  as.numeric(interaction_forest$lower)),
      upper = c(NA, NA,  as.numeric(interaction_forest$upper))),
      .Names = c("mean", "lower", "upper"), 
      row.names = c(NA, -(nrow(interaction_forest) + 2)), 
      class = "data.frame")
  
  tabletext<-cbind( 
    c("", "Subgroup", interaction_forest$label),
    # c(rep("  ", nrow(interaction_forest) + 3)),
    c("Incidence Rate", "Tramadol", interaction_forest$rate.y),
    c("", "Codeine",interaction_forest$rate.x),
    c("Hazard Ratio", "(95% CI)", interaction_forest$CI),
    c(rep("  ", nrow(interaction_forest) + 2)))
  
  
  # 
  # tabletext<-cbind( 
  #   c("", "Subgroup", "", interaction_forest$label),
  #   c("Tramadol/codeine", "No. at risk", "", paste(interaction_forest$n.y, interaction_forest$n.x, sep = "/")),
  #   c("  ", "No. of events", "per 1000 Person-years", paste(interaction_forest$rate.y, interaction_forest$rate.x, sep = "/")))
  #   # c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", interaction_forest$CI))
  
  tabletext[tabletext == "NA (NA)"] <- NA
  
  # tabletext<-cbind( 
  #   c("", "Subgroup", "", interaction_forest$label),
  #   c("Tramadol", "N. at risk", "", interaction_forest$n.y),
  #   c("", "No. of Events", "(1000 Person-years)", interaction_forest$rate.y),
  #   c(rep(NA, nrow(interaction_forest) + 3)),
  #   c("Codeine", "N. at risk", "", interaction_forest$n.x),
  #   c("", "No. of Events", "(1000 Person-years)", interaction_forest$rate.x),
  #   c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", interaction_forest$CI))
  
  return(plot_data = list(interaction_forest = interaction_forest, graph_data = graph_data, tabletext = tabletext))
}

composite_CVD <- plot_data_preparation( outcome = "composite_CVD")
fracturas <- plot_data_preparation( outcome = "fracturas")
all_cause_mortality <- plot_data_preparation( outcome = "all_cause_mortality")


plot_func_interaction <- function(input){
  forestplot(input$tabletext, 
             input$graph_data,
             align = c("l","l","l","c"),
             new_page = TRUE,
             
             txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 8),
                               ticks = gpar( fontfamily = "sans", fontsize = 8),
                               xlab = gpar( fontfamily = "sans", fontsize = 9)),
             is.summary = c(rep(TRUE,2), rep(FALSE, length(input$interaction_forest$bold))),
             boxsize = c(rep(TRUE,3), input$interaction_forest$box),
             col=fpColors( box=c("black"), 
                           line=c("black"), 
                           zero = "black", 
                           summary = "black"),
             
             colgap = unit(2, "mm"),
             graphwidth = unit(35, "mm"),
             lineheight = unit( 4.2, "mm"),
             graph.pos = 5,
             hrzl_lines = list(
               "1" = gpar(lty=1, lwd=1, columns=c(1:4)),
               "2" = gpar(lty=1, lwd=1, columns=c(2:3)),
               "3" = gpar(lty=1, lwd=1, columns=c(1:4)),
               # "4" = gpar(lty=1, lwd=0.5, columns=c(1: 4)),
               "5" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "6" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "9" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "12" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "15" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "18" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "21" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "24" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "25" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "28" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "31" = gpar(lty=1, lwd=0.5, columns=c(1:4), col = "grey"),
               "34" = gpar(lty=1, lwd=1, columns=c(1:4))
                           ),
             lwd.xaxis = 1,
             lwd.zero = gpar(lwd=1),
             clip=c(0.5, 2.5), 
             xticks = c(0.25, 1, 2, 4),
             grid = structure( input$graph_data$mean[nrow(input$graph_data)], gp = gpar(lty =2, lwd=1, col = "grey")),
             
             xlab = "Hazard Ratio (95% CI\n <---Favor Tramadol   Favor Codeine--->",
             xlog=TRUE)
}

plot_func_interaction(input = composite_CVD)
plot_func_interaction(input = fracturas)
plot_func_interaction(input = all_cause_mortality)



png(file = 'Figures/PS_subgroup_mortality.png',
    units = "cm",
    width = 14,
    height = 14,
    res = 300)

plot_func_interaction(input = all_cause_mortality)

dev.off()  




















# Dose-response in ITT ----------------------------------------------------


interaction_forest <- 
  MPR_survial_summary %>% 
  ungroup() %>%  
  filter( first_bill_drug == 1 | (first_bill_drug == 0 & group_label == "env_1")) %>% 
  mutate( group_label = case_when( first_bill_drug == 0 ~ "Reference [codeine]",
                                   TRUE ~ group_label)) %>% 
  mutate( estimate = case_when( first_bill_drug == 0 ~ NA_real_,
                                TRUE ~ estimate),
          lower = case_when( first_bill_drug == 0 ~ NA_real_,
                                TRUE ~ lower),
          upper = case_when( first_bill_drug == 0 ~ NA_real_,
                                TRUE ~ upper),
          dif_CI = case_when( first_bill_drug == 0 ~ NA_character_,
                             TRUE ~ dif_CI),
          CI = case_when( first_bill_drug == 0 ~ NA_character_,
                             TRUE ~ CI)) %>% 
  mutate( outcome_label = as.character(outcome_label)) %>% 
  add_row( outcome_label = c(unique( as.character(MPR_survial_summary$outcome_label)))) %>% 
  mutate( group_label = case_when( is.na(group_label) ~ "Initial prescription of tramadol",
                                   TRUE  ~ group_label)) %>% 
  add_row( outcome_label = c(unique( as.character(MPR_survial_summary$outcome_label)))) %>% 
  mutate( group_label = factor(case_when( is.na(group_label) ~ "NA",
                                          group_label == "env_1" ~ "1",
                                          group_label == "env_2" ~ "2",
                                          group_label == "env_3" ~ ">=3",
                                          TRUE  ~ group_label),
                               levels = c( "NA","Initial prescription of tramadol", "1", "2", ">=3", "Reference [codeine]"),
                               labels = c( "NA","Initial prescription of tramadol", "   1 pack", "   2 pack", "   >=3 pack", "   Reference [codeine]"))) %>% 
  mutate( outcome_label = factor( outcome_label, 
                                  levels = c("composite_CVD", "fracturas", "all_cause_mortality"),
                                  labels = c("Composite CVD events", "Fractures", "All cause mortality"))) %>% 
  mutate( label = case_when( group_label == "NA" ~ as.character(outcome_label),
                             TRUE ~ as.character(group_label))) %>% 
  mutate( bold = case_when( group_label == "NA" ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  arrange( outcome_label, group_label)
  
 

  # Cochrane data from the 'rmeta'-package
  graph_data <- 
    structure(list(
      mean  = c(NA, NA, as.numeric(interaction_forest$estimate)), 
      lower = c(NA, NA, as.numeric(interaction_forest$lower)),
      upper = c(NA, NA, as.numeric(interaction_forest$upper))),
      .Names = c("mean", "lower", "upper"), 
      row.names = c(NA, -(nrow(interaction_forest) + 2)), 
      class = "data.frame")
  
  tabletext<-cbind( 
    c("", "",  interaction_forest$label),
    c("No. of", "Patients", interaction_forest$n),
    c("No. of", "Events", interaction_forest$events),
    c("Incidence", "Rate", interaction_forest$rate),
    c("Risk Difference", "(95% CI)", interaction_forest$dif_CI),
    c("Hazard Ratio", "(95% CI)",interaction_forest$CI))
  
  
  tabletext[tabletext == "NA (NA)"] <- NA

  plot_data = list(interaction_forest = interaction_forest, graph_data = graph_data, tabletext = tabletext)

  plot_func_interaction <- function(input){
    forestplot(input$tabletext, 
               input$graph_data,
               align = c("l","l","l","l", "l", "l"),
               new_page = TRUE,
               
               txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 8),
                                 ticks = gpar( fontfamily = "sans", fontsize = 8),
                                 xlab = gpar( fontfamily = "sans", fontsize = 8)),
               is.summary = c(rep(TRUE,2),input$interaction_forest$bold),
               boxsize = 0.3,
               col=fpColors( box=c("royalblue"), 
                             line=c("royalblue"), 
                             zero = "black", 
                             summary = "royalblue"),
               
               colgap = unit(5, "mm"),
               graphwidth = unit(35, "mm"),
               lineheight = unit( 3.8, "mm"),
               graph.pos = 7,
               hrzl_lines = list(
                 "1" = gpar(lty=1, lwd=0.5, columns=c(1:7)),
                 "3" = gpar(lty=1, lwd=0.5, columns=c(1:7)),
                 "6" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
                 "7" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
                 "8" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
                 "9" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey")
                 # "13" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
                 # "16" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
                 # "19" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
                 # "22" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
                 # "25" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
                 # "26" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
                 # "29" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
                 # "32" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey")
               ),
               lwd.xaxis = 0.5,
               lwd.zero = gpar(lwd=0.5),
               clip=c(0.5,3.5), 
               xticks = c( 0.5, 1, 1.5, 3, 5),
               # grid = structure( input$graph_data$mean[nrow(input$graph_data)], gp = gpar(lty =2, lwd=0.75, col = "grey")),
               
               xlab = "Hazard Ratio (95% CI)",
               xlog=TRUE)
  }  
  
  
  plot_func_interaction( input = plot_data)
  
  
  




# Dose-response in ITT (new) ----------------------------------------------------
  
interaction_forest <- 
  MPR_survial_summary %>% 
  ungroup() %>%  
  filter( first_bill_drug == 1 | (first_bill_drug == 0 & group_label == "env_1")) %>% 
  mutate( group_label = case_when( first_bill_drug == 0 ~ "Ref [Codeine]",
                                   TRUE ~ group_label)) %>% 
  mutate( estimate = case_when( first_bill_drug == 0 ~ NA_real_,
                                TRUE ~ estimate),
          lower = case_when( first_bill_drug == 0 ~ NA_real_,
                             TRUE ~ lower),
          upper = case_when( first_bill_drug == 0 ~ NA_real_,
                             TRUE ~ upper),
          dif_CI = case_when( first_bill_drug == 0 ~ NA_character_,
                              TRUE ~ dif_CI),
          CI = case_when( first_bill_drug == 0 ~ NA_character_,
                          TRUE ~ CI)) %>% 
  mutate( outcome_label = as.character(outcome_label)) %>% 
  add_row( outcome_label = c(unique( as.character(MPR_survial_summary$outcome_label)))) %>% 
  mutate( group_label = factor(case_when( is.na(group_label) ~ "NA",
                                          group_label == "env_1" ~ "1",
                                          group_label == "env_2" ~ "2",
                                          group_label == "env_3" ~ ">=3",
                                          TRUE  ~ group_label),
                               levels = c( "NA", "1", "2", ">=3", "Ref [Codeine]"),
                               labels = c( "NA","   1", "   2", "   >=3", "   Ref [Codeine]"))) %>% 
  mutate( outcome_label = factor( outcome_label, 
                                  levels = c("composite_CVD", "fracturas", "all_cause_mortality"),
                                  labels = c("Composite CVD events", "Fractures", "All cause mortality"))) %>% 
  mutate( label = case_when( group_label == "NA" ~ as.character(outcome_label),
                             TRUE ~ as.character(group_label))) %>% 
  mutate( bold = case_when( group_label == "NA" ~ TRUE,
                            TRUE ~ FALSE)) %>% 
  arrange( outcome_label, group_label)



# Cochrane data from the 'rmeta'-package
graph_data <- 
  structure(list(
    mean  = c(NA, NA, as.numeric(interaction_forest$estimate)), 
    lower = c(NA, NA, as.numeric(interaction_forest$lower)),
    upper = c(NA, NA, as.numeric(interaction_forest$upper))),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -(nrow(interaction_forest) + 2)), 
    class = "data.frame")

tabletext<-cbind( 
  c("Pack of initial", "prescription [Tramadol]",  interaction_forest$label),
  c("No. of", "Patients", interaction_forest$n),
  c("No. of", "Events", interaction_forest$events),
  c("Incidence", "Rate", interaction_forest$rate),
  c("Risk Difference", "(95% CI)", interaction_forest$dif_CI),
  c("Hazard Ratio", "(95% CI)",interaction_forest$CI))


tabletext[tabletext == "NA (NA)"] <- NA

plot_data = list(interaction_forest = interaction_forest, graph_data = graph_data, tabletext = tabletext)

plot_func_interaction <- function(input){
  forestplot(input$tabletext, 
             input$graph_data,
             align = c("l","l","l","l", "l", "l"),
             new_page = TRUE,
             
             txt_gp = fpTxtGp( label = gpar( fontfamily = "sans", fontsize = 8),
                               ticks = gpar( fontfamily = "sans", fontsize = 10),
                               xlab = gpar( fontfamily = "sans", fontsize = 10)),
             # is.summary = c(rep(TRUE,2),input$interaction_forest$bold),
             boxsize = 0.25,
             col=fpColors( box=c("black"), 
                           line=c("black"), 
                           zero = "black", 
                           summary = "black"),
             
             colgap = unit(5, "mm"),
             graphwidth = unit(35, "mm"),
             lineheight = unit( 4, "mm"),
             graph.pos = 7,
             hrzl_lines = list(
               "1" = gpar(lty=1, lwd=1, columns=c(1:7)),
               "3" = gpar(lty=1, lwd=1, columns=c(1:7)),
               "5" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "6" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "7" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "10" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "11" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "12" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "15" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "16" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "17" = gpar(lty=1, lwd=0.25, columns=c(1:6), col = "grey"),
               "18" = gpar(lty=1, lwd=1, columns=c(1:6))
               # "13" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               # "16" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               # "19" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               # "22" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               # "25" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               # "26" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               # "29" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey"),
               # "32" = gpar(lty=1, lwd=0.25, columns=c(1:4), col = "grey")
             ),
             lwd.xaxis = 1,
             lwd.zero = gpar(lwd=0.5),
             clip=c(0.8,3.5), 
             xticks = c( 0.8, 1, 1.5, 3, 5),
             # grid = structure( input$graph_data$mean[nrow(input$graph_data)], gp = gpar(lty =2, lwd=0.75, col = "grey")),
             
             xlab = "Hazard Ratio (95% CI)",
             xlog=TRUE)
}  


png(file = 'Figures/dose.png',
    units = "cm",
    width = 18,
    height = 7,
    res = 500)

plot_func_interaction( input = plot_data)

dev.off()

  
  
# New_package based ITT ---------------------------------------------------
help( forest)
data(Olkin95)
m1 <- metabin(event.e, n.e, event.c, n.c,
              data = Olkin95, subset = c(41, 47, 51, 59),
              sm = "RR", method = "I",
              studlab = paste(author, year))



m1$main

















forest(m1)
