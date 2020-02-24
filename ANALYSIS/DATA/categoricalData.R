# scale returns values below -1


# How to write your own function: https://adv-r.hadley.nz/functions.html
# How does map from package purrr work: https://r4ds.had.co.nz/iteration.html
# How to mutate columns selectively with automatic new names: https://dplyr.tidyverse.org/reference/mutate_all.html

# load packages
library(purrr)
library(tidyselect)
library(stringr)


mergedData <- as_tibble(mergedData)

#alle Variablen die 99 sind: Kategorische Daten und 99 bedeutet: keine Angabe
mergedData[mergedData == '99'] <- NA

categories <- c(NA, 0, 1, 2, 3, 4)

pretendsToBeMetric <- function(x) {
  return(all(unique(x) %in% categories))
}

weight <- function(expr) {
  name <- quo_name(enquo(expr))
  if(stringr::str_detect(name, "trips")) {
    return(mergedData$tripsKidsNo*0.25*(!! expr))
    }
  else {
    return(mergedData$eatersPerMealNo*0.25*(!! expr))
  }
}

# enquo(mergedData$ownIdeas)
# 
# weight(mergedData$ownIdeas)
# 
# testWeighted <- weightOutcome(lessIll, mergedData, eatersPerMealNo)

mergedData <- mergedData %>% 
  mutate_if(pretendsToBeMetric, list(scaled = scale, ordered = as.ordered, weighted = weight))

# pretendsToBeMetric(mergedData$eatersPerMealNo)
# 
# all(unique(mergedData$lessIll) %in% categories)

# function to create list of names of ordinal variables falsely coded as numeric

# metricPretenders <- mergedData %>% 
#   select_if(pretendsToBeMetric)
# 
# metricPretendersNames <- names(metricPretenders)

# Coerce data type of ordinal variables falsely coded as "numeric" to "ordered"

# mergedData <- map_dfr(mergedData, changeDataType) 



# mergedData$participateMoreOrdinal=as.ordered(mergedData$participateMore)
# mergedData$tasksLunchOrdinal = as.ordered((mergedData$tasksLunch))
# mergedData$monthlyCooksOrdinal = as.ordered(mergedData$monthlyCooks)
# mergedData$weeklyCooksOrdinal = as.ordered(mergedData$weeklyCooks)
# mergedData$shoppersOrdinal = as.ordered(mergedData$shoppers)
# mergedData$ownIdeasOrdinal = as.ordered(mergedData$ownIdeas)
# mergedData$easyDishesOrdinal = as.ordered(mergedData$easyDishes)
# mergedData$dietaryKnowledgeOrdinal = as.ordered(mergedData$dietaryKnowledge)
# mergedData$appreciateHealthyOrdinal = as.ordered((mergedData$appreciateHealthy))
# mergedData$foodCultureOrdinal = as.ordered(mergedData$foodCulture)
# mergedData$influenceHomeOrdinal = as.ordered(mergedData$influenceHome)
# mergedData$cookAtHomeOrdinal = as.ordered(mergedData$cookAtHome)
# mergedData$askRecipesOrdinal = as.ordered(mergedData$askRecipes)
# mergedData$moreConcentratedOrdinal = as.ordered(mergedData$moreConcentrated)
# mergedData$moreBalancedOrdinal = as.ordered(mergedData$moreBalanced)
# mergedData$lessIllOrdinal = as.ordered(mergedData$lessIll)
# mergedData$dayToDaySkillsOrdinal = as.ordered(mergedData$dayToDaySkills)
# mergedData$moreIndependentOrdinal = as.ordered(mergedData$moreIndependent)
# mergedData$betterTeamworkOrdinal = as.ordered(mergedData$betterTeamwork)
# mergedData$betterReadingOrdinal = as.ordered(mergedData$betterReading)
# mergedData$betterGradesOrdinal = as.ordered(mergedData$betterGrades)
# mergedData$moreRegularSchoolVisitsOrdinal = as.ordered(mergedData$moreRegularSchoolVisits)
# mergedData$selfworthOrdinal = as.ordered(mergedData$selfworth)
# mergedData$moreOpenOrdinal = as.ordered(mergedData$moreOpen)
# mergedData$moreConfidenceOrdinal = as.ordered(mergedData$moreConfidence)
# mergedData$addressProblemsOrdinal = as.ordered(mergedData$addressProblems)
# mergedData$proudOrdinal = as.ordered(mergedData$proud)
# mergedData$successOrdinal = as.ordered(mergedData$success)
# mergedData$enoughFoodOrdinal = as.ordered(mergedData$enoughFood)
# mergedData$enoughStaffLunchOrdinal = as.ordered(mergedData$enoughStaffLunch)
# mergedData$enoughStaffActivitiesOrdinal = as.ordered(mergedData$enoughStaffActivities)
# mergedData$qualitySatisfiesOrdinal = as.ordered(mergedData$qualitySatisfies)
# mergedData$regionalProductsOrdinal = as.ordered(mergedData$regionalProducts)
# mergedData$cultureReligionOrdinal = as.ordered(mergedData$cultureReligion)
# mergedData$unsweetenedDrinksOrdinal = as.ordered(mergedData$unsweetenedDrinks)
# mergedData$tripsSuggestionsOrdinal = as.ordered(mergedData$tripsSuggestions)
# mergedData$tripsDecisionsOrdinal = as.ordered(mergedData$tripsDecisions)
# mergedData$tripsOrganizationOrdinal = as.ordered(mergedData$tripsOrganization)
# mergedData$tripsCostCalculationOrdinal = as.ordered(mergedData$tripsCostCalculation)
# mergedData$tripsBudgetOrdinal = as.ordered(mergedData$tripsBudget)
# mergedData$tripsMoneyOrdinal = as.ordered(mergedData$tripsMoney)
# mergedData$tripsReviewOrdinal = as.ordered(mergedData$tripsReview)
# mergedData$tripsPublicTransportOrdinal = as.ordered(mergedData$tripsPublicTransport)
# mergedData$tripsMobilityOrdinal = as.ordered(mergedData$tripsMobility)
# mergedData$tripsNewPlacesOrdinal = as.ordered(mergedData$tripsNewPlaces)
# mergedData$tripsNewCommunitiesOrdinal = as.ordered(mergedData$tripsNewCommunities)
# mergedData$tripsNewIdeasOrdinal = as.ordered(mergedData$tripsNewIdeas)
# mergedData$tripsAdditionalActivitiesOrdinal = as.ordered(mergedData$tripsAdditionalActivities)
# mergedData$tripsSpecificSkillsOrdinal = as.ordered(mergedData$tripsSpecificSkills)
# mergedData$tripsDayToDaySkillsOrdinal = as.ordered(mergedData$tripsDayToDaySkills)
# mergedData$tripsSuccessOrdinal = as.ordered(mergedData$tripsSuccess)
# mergedData$tripsSelfEfficacyOrdinal = as.ordered(mergedData$tripsSelfEfficacy)
# mergedData$tripsSelfworthOrdinal = as.ordered(mergedData$tripsSelfworth)
# mergedData$tripsSocialSkillsOrdinal = as.ordered(mergedData$tripsSocialSkills)
# mergedData$tripsFrustrationToleranceOrdinal = as.ordered(mergedData$tripsFrustrationTolerance)
# mergedData$tripsCHILDRENSuggestionsOrdinal = as.ordered(mergedData$tripsCHILDRENSuggestions)
# mergedData$betterNumbersOrdinal = as.ordered(mergedData$betterNumbers)
# mergedData$addressProblemsOrdinal = as.ordered(mergedData$addressProblems)
# mergedData$parentalDialogOrdinal = as.ordered(mergedData$parentalDialog)
# mergedData$friendlyEnvironmentOrdinal = as.ordered(mergedData$friendlyEnvironment)
# mergedData$menuOrdinal = as.ordered(mergedData$menu)
# mergedData$menuCycleOrdinal = as.ordered(mergedData$menuCycle)
# mergedData$tripsReachedOrdinal = as.ordered(mergedData$tripsReached)
# mergedData$tripsKnowledgeOrdinal = as.ordered(mergedData$tripsKnowledge)
# mergedData$tripsBehaviorOrdinal = as.ordered(mergedData$tripsBehavior)
# mergedData$selfworthOrdinal = as.ordered(mergedData$selfworth)
# mergedData$claimBTPOrdinal = as.ordered(mergedData$claimBTP)
# mergedData$benefitBTPOrdinal = as.ordered(mergedData$benefitBTP)
# mergedData$stayLongerOrdinal = as.ordered(mergedData$stayLonger)
# mergedData$suggestionsForCHILDRENOrdinal = as.ordered(mergedData$suggestionsForCHILDREN)
# mergedData$organicFoodstuffOrdinal = as.ordered(mergedData$organicFoodstuff)
# mergedData$seasonalFoodstuffOrdinal = as.ordered(mergedData$seasonalFoodstuff)
# mergedData$noSchoolLunchOrdinal = as.ordered(mergedData$noSchoolLunch)
# mergedData$expensiveSchoolLunchOrdinal = as.ordered(mergedData$expensiveSchoolLunch) 



# df <- data.frame(x = 1:5, y = sample(5))
# eval_tidy(expr(x + y), df)
# 
# df <- data.frame(x = 1:5, y = sample(5))
# eval_tidy(expr(x + y), df)
# eval_tidy(x + y, df)
# 
# x 




