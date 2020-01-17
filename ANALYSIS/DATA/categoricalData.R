# How to write your own function: https://adv-r.hadley.nz/functions.html
# How does map from package purrr work: https://r4ds.had.co.nz/iteration.html

# load packages
library(purrr)



mergedData <- as_tibble(mergedData)

#alle Variablen die 99 sind: Kategorische Daten und 99 bedeutet: keine Angabe
mergedData[mergedData == '99'] <- NA

# Coerce data type of ordinal variables falsely coded as "numeric" to "ordered"

# changeDataType <- function(x) {
#   categories <- c(NA, 0, 1, 2, 3, 4, 5)
#   if(all(unique(x) %in% categories)){
#     x <- as.ordered(x)
#   }
#   else {
#     x 
#   }
# }
# 
# mergedData <- map_dfr(mergedData, changeDataType) 

mergedData$participateMore=as.ordered(mergedData$participateMore)
mergedData$tasksLunch = as.ordered((mergedData$tasksLunch))
mergedData$monthlyCooks = as.ordered(mergedData$monthlyCooks)
mergedData$weeklyCooks = as.ordered(mergedData$weeklyCooks)
mergedData$shoppers = as.ordered(mergedData$shoppers)
mergedData$ownIdeas = as.ordered(mergedData$ownIdeas)
mergedData$easyDishes = as.ordered(mergedData$easyDishes)
mergedData$dietaryKnowledge = as.ordered(mergedData$dietaryKnowledge)
mergedData$appreciateHealthy = as.ordered((mergedData$appreciateHealthy))
mergedData$foodCulture = as.ordered(mergedData$foodCulture)
mergedData$influenceHome = as.ordered(mergedData$influenceHome)
mergedData$cookAtHome = as.ordered(mergedData$cookAtHome)
mergedData$askRecipes = as.ordered(mergedData$askRecipes)
mergedData$moreConcentrated = as.ordered(mergedData$moreConcentrated)
mergedData$moreBalanced = as.ordered(mergedData$moreBalanced)
mergedData$lessIll = as.ordered(mergedData$lessIll)
mergedData$dayToDaySkills = as.ordered(mergedData$dayToDaySkills)
mergedData$moreIndependent = as.ordered(mergedData$moreIndependent)
mergedData$betterTeamwork = as.ordered(mergedData$betterTeamwork)
mergedData$betterReading = as.ordered(mergedData$betterReading)
mergedData$betterGrades = as.ordered(mergedData$betterGrades)
mergedData$moreRegularSchoolVisits = as.ordered(mergedData$moreRegularSchoolVisits)
mergedData$selfworth = as.ordered(mergedData$selfworth)
mergedData$moreOpen = as.ordered(mergedData$moreOpen)
mergedData$moreConfidence = as.ordered(mergedData$moreConfidence)
mergedData$addressProblems = as.ordered(mergedData$addressProblems)
mergedData$proud = as.ordered(mergedData$proud)
mergedData$success = as.ordered(mergedData$success)
mergedData$enoughFood = as.ordered(mergedData$enoughFood)
mergedData$enoughStaffLunch = as.ordered(mergedData$enoughStaffLunch)
mergedData$enoughStaffActivities = as.ordered(mergedData$enoughStaffActivities)
mergedData$qualitySatisfies = as.ordered(mergedData$qualitySatisfies)
mergedData$regionalProducts = as.ordered(mergedData$regionalProducts)
mergedData$cultureReligion = as.ordered(mergedData$cultureReligion)
mergedData$unsweetenedDrinks = as.ordered(mergedData$unsweetenedDrinks)
mergedData$tripsSuggestions = as.ordered(mergedData$tripsSuggestions)
mergedData$tripsDecisions = as.ordered(mergedData$tripsDecisions)
mergedData$tripsOrganization = as.ordered(mergedData$tripsOrganization)
mergedData$tripsCostCalculation = as.ordered(mergedData$tripsCostCalculation)
mergedData$tripsBudget = as.ordered(mergedData$tripsBudget)
mergedData$tripsMoney = as.ordered(mergedData$tripsMoney)
mergedData$tripsReview = as.ordered(mergedData$tripsReview)
mergedData$tripsPublicTransport = as.ordered(mergedData$tripsPublicTransport)
mergedData$tripsMobility = as.ordered(mergedData$tripsMobility)
mergedData$tripsNewPlaces = as.ordered(mergedData$tripsNewPlaces)
mergedData$tripsNewCommunities = as.ordered(mergedData$tripsNewCommunities)
mergedData$tripsNewIdeas = as.ordered(mergedData$tripsNewIdeas)
mergedData$tripsAdditionalActivities = as.ordered(mergedData$tripsAdditionalActivities)
mergedData$tripsSpecificSkills = as.ordered(mergedData$tripsSpecificSkills)
mergedData$tripsDayToDaySkills = as.ordered(mergedData$tripsDayToDaySkills)
mergedData$tripsSuccess = as.ordered(mergedData$tripsSuccess)
mergedData$tripsSelfEfficacy = as.ordered(mergedData$tripsSelfEfficacy)
mergedData$tripsSelfworth = as.ordered(mergedData$tripsSelfworth)
mergedData$tripsSocialSkills = as.ordered(mergedData$tripsSocialSkills)
mergedData$tripsFrustrationTolerance = as.ordered(mergedData$tripsFrustrationTolerance)
mergedData$tripsCHILDRENSuggestions = as.ordered(mergedData$tripsCHILDRENSuggestions)
mergedData$betterNumbers = as.ordered(mergedData$betterNumbers)
mergedData$addressProblems = as.ordered(mergedData$addressProblems)
mergedData$parentalDialog = as.ordered(mergedData$parentalDialog)
mergedData$friendlyEnvironment = as.ordered(mergedData$friendlyEnvironment)
mergedData$menu = as.ordered(mergedData$menu)
mergedData$menuCycle = as.ordered(mergedData$menuCycle)
mergedData$tripsReached = as.ordered(mergedData$tripsReached)
mergedData$tripsKnowledge = as.ordered(mergedData$tripsKnowledge)
mergedData$tripsBehavior = as.ordered(mergedData$tripsBehavior)
mergedData$selfworth = as.ordered(mergedData$selfworth)
mergedData$claimBTP = as.ordered(mergedData$claimBTP)
mergedData$benefitBTP = as.ordered(mergedData$benefitBTP)
mergedData$stayLonger = as.ordered(mergedData$stayLonger)
mergedData$suggestionsForCHILDREN = as.ordered(mergedData$suggestionsForCHILDREN)
mergedData$organicFoodstuff = as.ordered(mergedData$organicFoodstuff)
mergedData$seasonalFoodstuff = as.ordered(mergedData$seasonalFoodstuff)
mergedData$noSchoolLunch = as.ordered(mergedData$noSchoolLunch)
mergedData$expensiveSchoolLunch = as.ordered(mergedData$expensiveSchoolLunch) 



# df <- data.frame(x = 1:5, y = sample(5))
# eval_tidy(expr(x + y), df)
# 
# df <- data.frame(x = 1:5, y = sample(5))
# eval_tidy(expr(x + y), df)
# eval_tidy(x + y, df)
# 
# x 




