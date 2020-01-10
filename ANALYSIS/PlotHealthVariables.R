library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

HealthRelVar <- mergedData %>% 
  select(lessIll, dietaryKnowledge, seasonalFoodstuff, organicFoodstuff, appreciateHealthy, tasksLunch, DGECriteriaNo)

response= names(HealthRelVar)[1:6]
expl= names(HealthRelVar)[7]

response = set_names(response)


expl = set_names(expl)

plotDGEOutcome <- function(outcome, z){
  ggplot(z, aes(x = .data[[DGECriteriaNo]], y = .data[[outcome]]) ) +
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "grey74") + 
    theme_bw() + 
    labs(x= DGE,
         y= outcome)
}

DGEHealthOutcome <- map(HealthRelVar, ~plotDGEOutcome(.DGECriteriaNo, "lessIll"))



