install.packages("forestplot")
library(forestplot)
forest <- 
  survial_summary %>% 
  ungroup() %>% 
  # mutate( outcome = substring( variable, 1, str_locate(aa$variable, "[.]")[,1]-1)) %>% 

  
  add_row( variable = c(unique(survial_summary$outcome_label))) %>% 
  
  mutate( outcome_label = case_when( is.na(outcome_label) ~  variable,
                                     TRUE ~ outcome_label)) %>% 
  mutate( stage = case_when( grepl("current", variable) ~ "current",
                             grepl("recent", variable) ~ "recent",
                             grepl("past", variable) ~ "past",
                             TRUE ~ "space")) %>% 
  mutate( label = case_when( stage == "current" ~ "     Curent",
                                stage == "recent" ~ "     Recent",
                                stage == "past" ~ "     Past",
                                TRUE ~ variable)) %>% 

  mutate(stage = factor( stage, levels = c( "space","current","recent","past"))) %>% 
  filter( outcome_label != "all_cause_mortality") %>% 
  
  arrange( outcome_label, stage)



# Cochrane data from the 'rmeta'-package
graph_data <- 
  structure(list(
    mean  = c(NA, NA, NA, as.numeric(forest$estimate)), 
    lower = c(NA, NA, NA, as.numeric(forest$lower)),
    upper = c(NA, NA, NA, as.numeric(forest$upper))),
    .Names = c("mean", "lower", "upper"), 
    row.names = c(NA, -35L), 
    class = "data.frame")



tabletext<-cbind( 
  c("Serious", "adverse outcomes", "", forest$label),
  c("Tramadol", "Cohort size", "(n)", forest$n.y),
  c("", "Follow up", "(mean, pys)", forest$mean_follow.y),
  c("", "Incidence", "(1000 pys)", forest$rate.y),
  c(rep(NA, 35)),
  c("Codeine", "Cohort size", "(n)", forest$n.x),
  c("", "Follow up", "(mean, pys)",forest$mean_follow.x),
  c("", "Incidence", "(1000 pys)", forest$rate.x),
  c("Tramadol vs Codeine", "Hazard ratio", "(95% CI)", forest$CI))




forestplot(tabletext, 
           graph_data,
           new_page = TRUE,
           
           txt_gp = fpTxtGp( label = gpar( fontfamily = "Arial"),
                             ticks = gpar( fontfamily = "Arial", cex = 0.6)),
           boxsize = 0.25,
           col=fpColors(box=c("royalblue"), line=c("royalblue")),
           
           colgap = unit(6, "mm"),
           

           graphwidth = unit(65, "mm"),
           lineheight = unit( 6, "mm"),
           
          
           graph.pos = 9,
           hrzl_lines = list("2" = gpar(lty=1, columns=c(2:4, 6:8)), 
                             "4" = gpar(lwd=1, columns=1:10)),
           

           
           clip=c(0.1,3.5), 
           xticks = c( 0.5, 1, 1.5, 3, 6),
           grid = structure( c(1.5, 3), gp = gpar(lty =2, lwd=1)),
           
             
           xlog=TRUE)

