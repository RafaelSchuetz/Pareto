library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

#createdataset

HealthRelVar <- mergedData %>% 
  dplyr::select(lessIll, dietaryKnowledge, seasonalFoodstuff, organicFoodstuff, appreciateHealthy, tasksLunch, 
                lessIllOrdinal, dietaryKnowledgeOrdinal, seasonalFoodstuffOrdinal, appreciateHealthyOrdinal, tasksLunchOrdinal,DGECriteriaNo,)

#setthevariables

response= names(HealthRelVar)[1:11]
expl= names(HealthRelVar)[12]

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

plotDGEOutcome("DGECriteriaNo", "lessIll")

#mapthefunction

DGEHealthOutcome <- map(HealthRelVar, ~plotDGEOutcome("DGECriteriaNo", "lessIll"))

all_plots = map(response,
                ~map(expl, plotDGEOutcome, y = .x) )

#printtheplots

all_plots$lessIll[1:2]
all_plots$dietaryKnowledge[1:2]
all_plots$appreciateHealthy[1:2]
all_plots$lessIllOrdinal
all_plots$dietaryKnowledgeOrdinal
all_plots$appreciateHealthyOrdinal



#againbutinpercentages #differentggplotfunction

response= names(HealthRelVar)[1:11]
expl= names(HealthRelVar)[12]

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

plotDGEOutcomeInPercent("DGECriteriaNo", "lessIll")

#mapthefunction

DGEHealthOutcomePercent <- map(HealthRelVar, ~plotDGEOutcomeInPercent("DGECriteriaNo", "lessIll"))

all_percentagePlots = map(response,
                ~map(expl, plotDGEOutcomeInPercent, y = .x) )

#printtheplots

all_percentagePlots$lessIll
all_percentagePlots$dietaryKnowledge
all_percentagePlots$appreciateHealthy
lessIll_DGE <- all_percentagePlots$lessIllOrdinal
dietaryKnowledge_DGE <- all_percentagePlots$dietaryKnowledgeOrdinal
appreciateHealthy_DGE <- all_percentagePlots$appreciateHealthyOrdinal

saveRDS(lessIll_DGE, "./ANALYSIS/GRAPHS/PAPER/lessIll_DGE.Rds")
saveRDS(dietaryKnowledge_DGE, "./ANALYSIS/GRAPHS/PAPER/dietaryKnowledge_DGE.Rds")
saveRDS(appreciateHealthy_DGE, "./ANALYSIS/GRAPHS/PAPER/appreciateHealthy_DGE.Rds")
