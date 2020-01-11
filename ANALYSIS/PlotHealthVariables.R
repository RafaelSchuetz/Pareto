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

