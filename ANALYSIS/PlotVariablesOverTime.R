#inspectinfluenceofvariablesovertime
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

plotOutcomeOverTime("year", "lessIll")

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

#plotforpresentation..explainsubsidyvalue

plotOutcomeOverTime("tripsSubsidy", "tripsNo")
plotOutcomeOverTime("realTripsSubsidy", "tripsNo")

#plots for paper 
saveRDS(, "./ANALYSIS/GRAPHS/PAPER GRAPHS/.Rds")

