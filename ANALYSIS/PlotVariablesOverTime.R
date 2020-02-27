#inspectinfluenceofvariablesovertime
#inpercentages

#setthevariables

response= names(mergedData)
# [5:213]
# expl= names(mergedData)[4]

responset = purrr::set_names(responset)
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
#all_yearPlots$lessIll_ordered


#plotforpresentation..explainsubsidyvalue

plotOutcomeOverTime("tripsSubsidy", "tripsNo")
TripsSub_plot<- plotOutcomeOverTime("realTripsSubsidy", "tripsNo")
MealsSub_plot<- plotOutcomeOverTime("realSubsidy", "mealsNo")


####plots we need for the paper 

lessIll_Time<- plotOutcomeOverTime("year", "lessIll_ordered")
appreciateHealthy_Time<- plotOutcomeOverTime("year", "appreciateHealthy_ordered")
dietaryKnowledge_Time<- plotOutcomeOverTime("year", "dietaryKnowledge_ordered")

Health_Year <- plot_grid(lessIll_Time, dietaryKnowledge_Time, appreciateHealthy_Time, 
                          ncol = 1, nrow = 3, align = "v",
                          labels = "AUTO",
                          label_x = 0, label_y = 0, hjust = -3, vjust = 
                            -1.5, label_fontface = "plain", label_size = 11)


saveRDS(Health_Year, "./ANALYSIS/GRAPHS/PAPER/Health_Year.Rds")

selfworth_Time <- plotOutcomeOverTime("year", "selfworth_ordered")
dayToDaySkills_Time <- plotOutcomeOverTime("year", "dayToDaySkills_ordered")

Equality_Year <- plot_grid(selfworth_Time, dayToDaySkills_Time, 
                         ncol = 1, nrow = 2, align = "v",
                         labels = "AUTO",
                         label_x = 0, label_y = 0, hjust = -3, vjust = 
                           -1.5, label_fontface = "plain", label_size = 11)


saveRDS(Equality_Year, "./ANALYSIS/GRAPHS/PAPER/Equality_Year.Rds")











