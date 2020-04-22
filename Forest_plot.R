install.packages("forestplot")
library(forestplot)
forest <- 
  survial_summary %>% 
  ungroup() %>% 

  add_row( outcome_label = c(unique(survial_summary$outcome_label))) %>% 
  mutate( outcome_label = factor( outcome_label, 
                                  levels = c("constipation", "sleep_disorders", "delirium", "opioid_abuse", "cardiac_arrhythmia", "falls", "fracturas", "all_cause_mortality", "whole" ),
                                  labels = c("Constipation", "Sleep disorders", "Delirium", "Opioid abuse", "Cardiac arrhythmia", "Falls", "Fractures", "All cause mortality", "Any above outcomes" ))) %>% 

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
    row.names = c(NA, -39L), 
    class = "data.frame")



tabletext<-cbind( 
  c("Serious", "adverse outcomes", "", forest$label),
  c("Tramadol", "Cohort size", "(n)", forest$n.y),
  c("", "Events", "(n)", forest$events.y),
  c("", "Mean follow-up", "(pys)", forest$mean_follow.y),
  c("", "Incidence", "(per 1000 pys)", forest$rate.y),
  c(rep(NA, 39)),
  c("Codeine", "Cohort size", "(n)", forest$n.x),
  c("", "Events", "(n)", forest$events.x),
  c("", "Mean follow-up", "(pys)",forest$mean_follow.x),
  c("", "Incidence", "(per 1000 pys)", forest$rate.x),
  c("Tramadol vs Codeine", "PS-adjusted HR", "(95% CI)", forest$CI))



tiff(file = 'Figures/forestplot.tiff', 
    units = "cm",
    width = 26, 
    height = 17,
    compression = "lzw",
    res = 300) 

forestplot(tabletext, 
           graph_data,
           new_page = TRUE,
           
           txt_gp = fpTxtGp( label = gpar( fontfamily = "Candara", cex = 0.6),
                             ticks = gpar( fontfamily = "Candara", cex = 0.5)),
           is.summary = c(rep(TRUE,3), forest$bold),
           boxsize = 0.25,
           col=fpColors(box=c("royalblue"), line=c("royalblue"), zero = "black"),
           
           colgap = unit(2, "mm"),
           

           graphwidth = unit(65, "mm"),
           lineheight = unit( 4.2, "mm"),
           
          
           graph.pos = 11,
           hrzl_lines = list("1" = gpar(lty=1, lwd=1,columns=c(1:12)),
                             "2" = gpar(lty=1, columns=c(2:5, 7:10, 12)), 
                             "4" = gpar(lty=1, lwd=1, columns=1:12),
                             "40" = gpar(lty=1, lwd=1, columns=11)),
           lwd.zero = gpar(lwd=1),
           clip=c(0.1,3.5), 
           xticks = c( 0.5, 1, 1.5, 3, 6),
           grid = structure( c(1.5, 3), gp = gpar(lty =2, lwd=1, col = "grey")),
           
           xlog=TRUE)

dev.off() 


a <- list(gpar( fontfamily = "Arial", cex = c(0.9)),
          gpar( fontfamily = "Arial", cex = c(1.9)))



a <- c(rep(TRUE,3), rep(FALSE,36))




