#inspectinfluenceofvariablesovertime
#inpercentages

library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#setthevariables

response= names(mergedData)[5:213]
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

YearlyOutcome <- map(mergedData, ~plotOutcomeOverTime("year", "lessIll"))

all_yearPlots = map(response,
                ~map(expl, plotOutcomeOverTime, y = .x) )

#printtheplots #allvaribalescanbeplotted #examples
  
all_yearPlots$age[1:2]
all_yearPlots$eatersPerMealNo[1:2]
all_yearPlots$newKidsNo[1:2]
all_yearPlots$cateringNo[1:2]
all_yearPlots$mealsInInstitutionNo[1:2]
all_yearPlots$weeklyCooks
all_yearPlots$monthlyCooks
all_yearPlots$tasksLunch
all_yearPlots$parentalDialog
all_yearPlots$qualitySatisfies
all_yearPlots$trainingCompletedNo
all_yearPlots$trainingStartedNo
all_yearPlots$selfworth
all_yearPlots$influenceHome
all_yearPlots$moreIndependent
all_yearPlots$dayToDaySkills

all_yearPlots$dayToDaySkillsOrdinal
all_yearPlots$lessIllOrdinal


#plotforpresentation..explainsubsidyvalue

plotOutcomeOverTime("tripsSubsidy", "tripsNo")
plotOutcomeOverTime("realTripsSubsidy", "tripsNo")

#plots for paper 


####plots we need for the paper 

lessIll_Time<- all_yearPlots$lessIllOrdinal
AppreciateHealthy_Time<- all_yearPlots$appreciateHealthyOrdinal 
DietaryKnowledge_Time<- all_yearPlots$dietaryKnowledgeOrdinal 

summaryStatistics_HealthVariables <- plot_grid(lessIll_Time, AppreciateHealthy_Time, DietaryKnowledge_Time, 
                                       ncol = 1, nrow = 3, align = "v",
                                       labels = c("A", "B", "C"),
                                       label_x = 0, label_y = 0, hjust = -1.5, vjust = 
                                         -1.5, label_fontface = "plain", label_size = 11)

saveRDS(, "./ANALYSIS/GRAPHS/PAPER GRAPHS/.Rds")

all_yearPlots$selfworthOrdinal
all_yearPlots$dayToDaySkillsOrdinal 

saveRDS(, "./ANALYSIS/GRAPHS/PAPER GRAPHS/.Rds")

