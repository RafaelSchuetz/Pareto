library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

#createdataset

HealthRelVar <- mergedData %>% 
  select(lessIll, dietaryKnowledge, seasonalFoodstuff, organicFoodstuff, appreciateHealthy, tasksLunch, DGECriteriaNo)

#setthevariables

response= names(HealthRelVar)[1:6]
expl= names(HealthRelVar)[7]

response = set_names(response)
response

expl = set_names(expl)
expl

#createfunctionHEREGGPLOT

plotDGEOutcome = function(x, y){
  ggplot(HealthRelVar, aes_string(x = x, y = y) ) +
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "grey74") + 
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
all_plots$seasonalFoodstuff[1:2]
all_plots$organicFoodstuff[1:2]
all_plots$appreciateHealthy[1:2]
all_plots$tasksLunch[1:2]



#againbutinpercentages #differentggplotfunction

response= names(HealthRelVar)[1:6]
expl= names(HealthRelVar)[7]

response = set_names(response)
response

expl = set_names(expl)
expl

#createfunction 


plotDGEOutcomeInPercent = function(x, y){
 #filter(HealthRelVar, !is.na(y)) %>%
  ggplot(HealthRelVar, aes_string(fill = y, order = y, x = x, y = y) ) +
    geom_col(position = position_stack(reverse = TRUE)) +
    theme_bw()
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
all_percentagePlots$seasonalFoodstuff
all_percentagePlots$organicFoodstuff
all_percentagePlots$appreciateHealthy
all_percentagePlots$tasksLunch
