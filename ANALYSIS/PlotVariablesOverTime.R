#inspectinfluenceofvariablesovertime
#inpercentages

library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)
library(ggpubr)

#setthevariables

responset= names(mergedData)
# [5:213]
# expl= names(mergedData)[4]

responset = purrr::set_names(response)
responset

explt = purrr::set_names("year")
explt

#createfunctionHEREGGPLOT

plotOutcomeOverTime = function(x, y){
  ggplot(mergedData, aes_string(fill = y, x = x, y = y) ) +
    # geom_bar(position="stack", stat="identity") + 
    geom_col(position = position_stack(reverse = TRUE)) +  
    theme_bw()  + theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
}

# plotOutcomeOverTimeTest = function(x, y){
#   ggplot(mergedData, aes_string(fill = y, x = x, y = y) ) +
#     geom_bar(position = position_stack(reverse = TRUE), stat="identity") + 
#     # geom_col(position = position_stack(reverse = TRUE)) +  
#     theme_bw()
# }

#checkifthefunctionworks

test <-  plotOutcomeOverTime("year", "lessIllOrdinal")

# plotOutcomeOverTimeTest("year", "lessIllOrdinal")

#mapthefunction

# YearlyOutcome <- map(mergedData, ~plotOutcomeOverTime("year", "lessIll"))

all_yearPlots = map(responset,
                ~map(explt, plotOutcomeOverTime, y = .x) )

#printtheplots #allvaribalescanbeplotted #examples
  
# all_yearPlots$age[1:2]
# all_yearPlots$eatersPerMealNo[1:2]
# all_yearPlots$newKidsNo[1:2]
# all_yearPlots$cateringNo[1:2]
# all_yearPlots$mealsInInstitutionNo[1:2]
# all_yearPlots$weeklyCooks
# all_yearPlots$monthlyCooks
# all_yearPlots$tasksLunch
# all_yearPlots$parentalDialog
# all_yearPlots$qualitySatisfies
# all_yearPlots$trainingCompletedNo
# all_yearPlots$trainingStartedNo
# all_yearPlots$selfworth
# all_yearPlots$influenceHome
# all_yearPlots$moreIndependent
# all_yearPlots$dayToDaySkills
# all_yearPlots$dayToDaySkillsOrdinal
# all_yearPlots$lessIllOrdinal


#plotforpresentation..explainsubsidyvalue

plotOutcomeOverTime("tripsSubsidy", "tripsNo")
plotOutcomeOverTime("realTripsSubsidy", "tripsNo")

#plots for paper 


####plots we need for the paper 

lessIll_Time<- all_yearPlots$lessIllOrdinal
appreciateHealthy_Time<- all_yearPlots$appreciateHealthyOrdinal 
dietaryKnowledge_Time<- all_yearPlots$dietaryKnowledgeOrdinal

saveRDS(lessIll_Time, "./ANALYSIS/GRAPHS/PAPER/lessIll_Time.Rds")
saveRDS(AppreciateHealthy_Time, "./ANALYSIS/GRAPHS/PAPER/appreciateHealthy_time.Rds")
saveRDS(DietaryKnowledge_Time, "./ANALYSIS/GRAPHS/PAPER/dietaryKnowledge_Time.Rds")

selfworth_Time <- all_yearPlots$selfworthOrdinal
dayToDaySkills_Time <- all_yearPlots$dayToDaySkillsOrdinal

saveRDS(selfworth_Time, "./ANALYSIS/GRAPHS/PAPER/selfworth_Time.Rds")
saveRDS(dayToDaySkills_Time.lm, "./ANALYSIS/GRAPHS/PAPER/dayToDaySkills_Time.Rds")











