install.packages("forestplot")
install.packages( "DataCombine")
library(forestplot)
library(scales)
library(grid)
library(gridExtra)
library( DataCombine)
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




# Interaction in ITT ------------------------------------------------------
c( "myocardial_infarction", "stroke", "fracturas", "all_cause_mortality")

plot_data_preparation <- function( outcome){
  interaction_forest <- 
    survial_summary %>% 
    filter( separate_label == outcome) %>% 
    ungroup() %>% 
    add_row( outcome_label = c(unique(survial_summary$outcome_label))) %>% 
    mutate( outcome_label = factor( outcome_label, 
                                    levels = c("initiation_age", "sex",  "cancer", "oa","back_pain", "neck_pain", "windex", "Ibuprofeno", "overall" ),
                                    labels = c("Age", "Sex",  "Cancer", "OA", "Back pain", "Neck pain",  "CCI", "NSAIDs", "Overall"))) %>% 
    #in order to set NA as an separate factor level
    mutate( group_label = case_when(is.na(group_label) ~ " ",
                                    TRUE ~ group_label)) %>% 
    mutate( group_label = factor(group_label,
                                 levels = c(" ", "overall", "18-39", "40-59", "60-79", ">=80","H", "D", "0", "1", "0dis", "1-2dis", "3-4dis", ">=5dis"),
                                 labels = c(" ", "Overall", "  18-39", "  40-59", "  60-79", " >=80", "  Male", "  Female", "  No", "  Yes", "  0", "  1-2", "  3-4", "  >=5"))) %>% 
    
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
    arrange( outcome_label, group_label)
  
  
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
    c("", "Subgroup", "", interaction_forest$label),
    c("Tramadol/codeine", "No. at risk", "", paste(interaction_forest$n.y, interaction_forest$n.x, sep = "/")),
    # c("", "No. of events", "", paste(interaction_forest$events.y, interaction_forest$events.x, sep = "/")),
    c("", "No. of events", "per 1000 Person-years", paste(interaction_forest$rate.y, interaction_forest$rate.x, sep = "/")),
    c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", interaction_forest$CI))
  
  tabletext[tabletext == "NA/NA"] <- NA
  
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


myocardial_infarction <- plot_data_preparation( outcome = "myocardial_infarction")
stroke <- plot_data_preparation( outcome = "stroke")
fracturas <- plot_data_preparation( outcome = "fracturas")
all_cause_mortality <- plot_data_preparation( outcome = "all_cause_mortality")


plot_func_interaction <- function(input){
  forestplot(input$tabletext, 
             input$graph_data,
             new_page = FALSE,
             
             txt_gp = fpTxtGp( label = gpar( fontfamily = "Candara", cex = 0.45),
                               ticks = gpar( fontfamily = "Candara", cex = 0.4)),
             is.summary = c(rep(TRUE,3), input$interaction_forest$bold),
             boxsize = 0.3,
             col=fpColors(box=c("royalblue"), line=c("royalblue"), zero = "black", summary = "royalblue"),
             
             colgap = unit(0, "mm"),
             
             
             graphwidth = unit(30, "mm"),
             lineheight = unit( 3, "mm"),
             
             
             graph.pos = 4,
             hrzl_lines = list(
               "2" = gpar(lty=1, lwd=0.5, columns=c(2:3)),
               "4" = gpar(lty=1, lwd=0.5, columns=c(1: 5))
               # "31" = gpar(lty=1, lwd=0.3, columns=c(4))
               ),
             lwd.xaxis = 0.5,
             
             lwd.zero = gpar(lwd=0.5),
             clip=c(0.5,3.5), 
             xticks = c( 0.5, 1, 1.5, 3, 5),
             grid = structure( input$graph_data$mean[nrow(input$graph_data)], gp = gpar(lty =2, lwd=0.5, col = "grey")),
             
             xlog=TRUE)
}


plot_func_interaction(input = myocardial_infarction)
plot_func_interaction(input = stroke)
plot_func_interaction(input = fracturas)
plot_func_interaction(input = all_cause_mortality)



tiff(file = 'Figures/forestplot_test.tiff',
     units = "cm",
     width = 20,
     height = 22,
     compression = "lzw",
     res = 500)



 grid.rect(gp = gpar(lty = "dashed"))
 vp1 <- viewport(x = 0, y = 0.5, w = 0.5, h = 0.5, just = c("left", "bottom"))
 vp2 <- viewport(x = 0.5, y = 0.5, w = 0.5, h = 0.5, just = c("left", "bottom"))
 vp3 <- viewport(x = 0, y = 0, w = 0.5, h = 0.5, just = c("left", "bottom"))
 vp4 <- viewport(x = 0.5, y = 0, w = 0.5, h = 0.5, just = c("left", "bottom"))
 pushViewport(vp1)
 grid.rect(gp = gpar(col = "grey"))
 # grid.text("Some drawing in graphics region 1", y = 0.8)
 plot_func_interaction(input = myocardial_infarction)
 upViewport()
 
 pushViewport(vp2)
 grid.rect(gp = gpar(col = "grey"))
 plot_func_interaction(input = stroke)
 upViewport()
 
 pushViewport(vp3)
 grid.rect(gp = gpar(col = "grey"))
 plot_func_interaction(input = fracturas)
 upViewport()
 
 pushViewport(vp4)
 grid.rect(gp = gpar(col = "grey"))
 plot_func_interaction(input = all_cause_mortality)
 upViewport()
 
 dev.off()














# Dose-response in ITT ----------------------------------------------------

c("myocardial_infarction", "stroke", "fracturas", "all_cause_mortality")

aa <- function( outcomes, breaks, lim){
  
  ggplot( data = filter(ITT_survial_summary_Dose, outcome_label ==  outcomes)) +
    geom_pointrange( aes(x = dose_group, y = estimate, ymin = lower, ymax = upper), shape = 15) +
    scale_x_discrete( expand = c( 0.1, 0), label = c("1", "2", ">=3"))+
    scale_y_continuous( expand = c( 0.1, 0), limit = lim, breaks = breaks)+
    labs(x = "Prescription times",
         y = "Hazard ratio \n(95% CI)") +
    
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
      aspect.ratio = 1) 
}

aa(outcomes = "myocardial_infarction", breaks = c(0.5, 1, 1.5, 2, 2.5), lim = c(0.5,2.5))
aa(outcomes = "all_cause_mortality", breaks = c(1, 2, 3, 4, 5), lim = c(1,5))
aa(outcomes = "stroke", breaks = c(1, 1.5, 2, 2.5, 3), lim = c(1,3.2))
aa(outcomes = "fracturas", breaks = c( 1.5, 2, 2.5, 3, 3.5), lim = c(1.5,3.7))

?trans_breaks
          
  
trans_breaks("log10", function(x) 10 ^ x)(c(1, 1e6))
trans_breaks("log2", n = 5,function(x) 2^x)(c(1, 4))
