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

DGE_plots = map(all_plots, ~cowplot::plot_grid(plotlist = .x))
DGE_plots


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

saveRDS(lessIll_DGE, "./ANALYSIS/GRAPHS/PAPER/lessIll_DGE.Rds")
saveRDS(dietaryKnowledge_DGE, "./ANALYSIS/GRAPHS/PAPER/dietaryKnowledge_DGE.Rds")
saveRDS(appreciateHealthy_DGE, "./ANALYSIS/GRAPHS/PAPER/appreciateHealthy_DGE.Rds")


#option to save all plots with same expl in one 

response_percentageplots = map(all_percentagePlots, ~cowplot::plot_grid(plotlist = .x))
response_percentageplots