##inspectinfluenceofvariablesovertime
#inpercentages

library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#setthevariables

response= names(mergedData)[5:119]
expl= names(mergedData)[4]

response = set_names(response)
response

expl = set_names(expl)
expl

#createfunctionHEREGGPLOT

plotOutcomeOverTime = function(x, y){
  ggplot(mergedData, aes_string(fill = y, x = x, y = y) ) +
    #geom_bar(position="stack", stat="identity") + 
    geom_col(position = position_stack(reverse = TRUE)) +  
    theme_bw() 
}

#checkifthefunctionworks

plotOutcomeOverTime("year", "lessIllOrdinal")

#mapthefunction

YearlyOutcome <- map(mergedData, ~plotOutcomeOverTime("year", "lessIllOrdinal"))

all_yearPlots = map(response,
                    ~map(expl, plotOutcomeOverTime, y = .x) )

#printtheplots #allvaribalescanbeplotted #examples

all_yearPlots$age[1:2]
all_yearPlots$eatersPerMealNo[1:2]
all_yearPlots$newKidsNo[1:2]
all_yearPlots$cateringNo[1:2]
all_yearPlots$mealsInInstitutionNo[1:2]
all_yearPlots$weeklyCooksOrdinal
all_yearPlots$monthlyCooksOrdinal
all_yearPlots$tasksLunchOrdinal
all_yearPlots$parentalDialogOrdinal
all_yearPlots$qualitySatisfiesOrdinal
all_yearPlots$trainingCompletedNo
all_yearPlots$trainingStartedNo
all_yearPlots$selfworthOrdinal
all_yearPlots$influenceHomeOrdinal
all_yearPlots$moreIndependentOrdinal
all_yearPlots$dayToDaySkillsOrdinal

plotOutcomeOverTime("tripsSubsidy", "tripsNo")




##plot Health Variables 

library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

#createdataset

HealthRelVar <- mergedData %>% 
  select(lessIllOrdinal, dietaryKnowledgeOrdinal, seasonalFoodstuffOrdinal, organicFoodstuffOrdinal, appreciateHealthyOrdinal, tasksLunchOrdinal, DGECriteriaNo)

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
    #geom_smooth(method = "loess", se = FALSE, color = "grey74") + 
    theme_bw()
}

#checkifthefunctionworks

plotDGEOutcome("DGECriteriaNo", "lessIllOrdinal")

#mapthefunction

DGEHealthOutcome <- map(HealthRelVar, ~plotDGEOutcome("DGECriteriaNo", "lessIllOrdinal"))

all_plots = map(response,
                ~map(expl, plotDGEOutcome, y = .x) )

#printtheplots

all_plots$lessIllOrdinal[1:2]
all_plots$dietaryKnowledgeOrdinal[1:2]
all_plots$seasonalFoodstuffOrdinal[1:2]
all_plots$organicFoodstuffOrdinal[1:2]
all_plots$appreciateHealthyOrdinal[1:2]
all_plots$tasksLunchOrdinal[1:2]



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

plotDGEOutcomeInPercent("DGECriteriaNo", "lessIllOrdinal")

#mapthefunction

DGEHealthOutcomePercent <- map(HealthRelVar, ~plotDGEOutcomeInPercent("DGECriteriaNo", "lessIllOrdinal"))

all_percentagePlots = map(response,
                          ~map(expl, plotDGEOutcomeInPercent, y = .x) )

#printtheplots

all_percentagePlots$lessIllOrdinal
all_percentagePlots$dietaryKnowledgeOrdinal
all_percentagePlots$seasonalFoodstuffOrdinal
all_percentagePlots$organicFoodstuffOrdinal
all_percentagePlots$appreciateHealthyOrdinal
all_percentagePlots$tasksLunchOrdinal
