library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

#createdataset

HealthRelVar <- mergedData %>% 
  dplyr::select(lessIll_ordered, dietaryKnowledge_ordered, appreciateHealthy_ordered, DGECriteriaNo)

#setthevariables

response= names(HealthRelVar)[1:3]
expl= names(HealthRelVar)[4]

response = purrr::set_names(response)
response

expl = set_names(expl)
expl

#createfunctionHEREGGPLOT

plotDGEOutcome = function(x, y){
    ggplot(HealthRelVar, aes_string(x = x, y = y) ) +
    geom_point() + 
    #geom_smooth(method = "loess", se = FALSE, color = "grey74") + 
    theme_bw()
}

#checkifthefunctionworks

plotDGEOutcome("DGECriteriaNo", "lessIll_ordered")

#mapthefunction

DGEHealthOutcome <- map(HealthRelVar, ~plotDGEOutcome("DGECriteriaNo", "lessIll_ordered"))

all_plots = map(response,
                ~map(expl, plotDGEOutcome, y = .x) )

#printtheplots

# all_plots$lessIll[1:2]
# all_plots$dietaryKnowledge[1:2]
# all_plots$appreciateHealthy[1:2]
all_plots$lessIll_ordered
all_plots$dietaryKnowledge_ordered
all_plots$appreciateHealthy_ordered


LessIll_plot <- plotDGEOutcome("DGECriteriaNo", "lessIll_ordered")
DietaryKnowledge_plot <- plotDGEOutcome("DGECriteriaNo", "dietaryKnowledge_ordered")
AppreciateHealthy_plot <- plotDGEOutcome("DGECriteriaNo", "appreciateHealthy_ordered")

Health_plots <- plot_grid(LessIll_plot, DietaryKnowledge_plot, AppreciateHealthy_plot, 
                                       ncol = 2, nrow = 2, align = "hv",
                                       labels = "AUTO",
                                       label_x = 0, label_y = 0, hjust = -3, vjust = 
                                         -1.5, label_fontface = "plain", label_size = 11)

saveRDS(Health_plots, "./ANALYSIS/GRAPHS/PAPER/Health_plots.Rds")

#againbutinpercentages #differentggplotfunction

response= names(HealthRelVar)[1:3]
expl= names(HealthRelVar)[4]

response = purrr::set_names(response)
response

expl = set_names(expl)
expl

#createfunction 


plotDGEOutcomeInPercent = function(x, y){
 #filter(HealthRelVar, !is.na(y)) %>%
  ggplot(HealthRelVar, aes_string(fill = y, order = y, x = x, y = y) ) +
    geom_col(position = position_stack(reverse = TRUE)) +
    theme_bw()  + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
  #+ scale_fill_discrete(na.translate=FALSE) 

}

#checkifitworks

plotDGEOutcomeInPercent("DGECriteriaNo", "lessIll_ordered")

#mapthefunction

DGEHealthOutcomePercent <- map(HealthRelVar, ~plotDGEOutcomeInPercent("DGECriteriaNo", "lessIll_ordered"))

all_percentagePlots = map(response,
                ~map(expl, plotDGEOutcomeInPercent, y = .x) )

#printtheplots

# all_percentagePlots$lessIll
# all_percentagePlots$dietaryKnowledge
# all_percentagePlots$appreciateHealthy
lessIll_DGE <- all_percentagePlots$lessIll_ordered
dietaryKnowledge_DGE <- all_percentagePlots$dietaryKnowledge_ordered
appreciateHealthy_DGE <- all_percentagePlots$appreciateHealthy_ordered

LessIll_percentageplot <- plotDGEOutcomeInPercent("DGECriteriaNo", "lessIll_ordered")
Dietary_percentageplot <- plotDGEOutcomeInPercent("DGECriteriaNo", "dietaryKnowledge_ordered")
Appreciate_percentageplot <- plotDGEOutcomeInPercent("DGECriteriaNo", "appreciateHealthy_ordered")

Health_percentageplots <- plot_grid(LessIll_percentageplot, Dietary_percentageplot, Appreciate_percentageplot, 
                          ncol = 2, nrow = 2, align = "hv",
                          labels = "AUTO",
                          label_x = 0, label_y = 0, hjust = -3, vjust = 
                            -1.5, label_fontface = "plain", label_size = 11)

saveRDS(Health_percentageplots, "./ANALYSIS/GRAPHS/PAPER/Health_percentageplots.Rds")