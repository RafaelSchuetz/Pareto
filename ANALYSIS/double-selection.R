# use package hdm to perform double selection: OLS regression after feature selection
# https://insightr.wordpress.com/2017/08/04/the-package-hdm-for-double-selection-inference-with-a-simple-example/

library(hdm)
library(tidyselect)

outcomesMeals <- c("participateMore",
                   "tasksLunch",
                   "monthlyCooks",
                   "weeklyCooks",
                   "shoppers",
                   "ownIdeas",
                   "stayLonger",
                   "easyDishes",
                   "dietaryKnowledge",
                   "appreciateHealthy",
                   "foodCulture",
                   "influenceHome",
                   "cookAtHome",
                   "askRecipes",
                   "moreConcentrated",
                   "moreBalanced",
                   "lessIll",
                   "dayToDaySkills",
                   "moreIndependent",
                   "betterTeamwork",
                   "betterReading",
                   "betterNumbers",
                   "betterGrades",
                   "moreRegularSchoolVisits",
                   "selfworth",
                   "moreOpen",
                   "moreConfidence",
                   "addressProblems",
                   "proud")

outcomesTrips <- c("tripsSuggestions",
                   "tripsDecision",
                   "tripsOrganization",
                   "tripsCostCalculations",
                   "tripsBudget",
                   "tripsMoney",
                   "tripsReview",
                   "tripsPublicTransport",
                   "tripsMobility",
                   "tripsNewPlaces",
                   "tripsNewCommunities",
                   "tripsNewIdeas",
                   "tripsAdditionalAcivities",
                   "tripsSpecificSkills",
                   "tripsDayToDaySkills",
                   "tripsSuccess",
                   "tripsSelfEfficacy",
                   "tripsSelfworth", 
                   "tripsSocialSkills",
                   "tripsFrustrationTolerance",
                   "tripsCHILDRENSuggestionsOrdinal",
                   "tripsReachedOrdinal",
                   "tripsKnowledgeOrdinal",
                   "tripsBehaviorOrdinal")

# this function returns all years in which a variable was recorded

availableYears <- function(dataSet, variableName) {
  if(is_string(variableName)) {
    variable <- dataSet[, variableName]
  }
  else {
    variable <- variableName
  }
  availableObservations <- dataSet %>% 
    filter(!is.na(variable))
  availableYears <- unique(availableObservations$year)
  return(availableYears)
}

# This function says whether "firstVariableName" was recorded in all years in which "secondVariableName" was recorded

compareAvailableYears <- function(firstVariableName, secondVariableName, dataSet) {
  if(availableYears(dataSet, secondVariableName) %in% availableYears(dataSet, firstVariableName)){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# This function returns the names of all variables in "dataSet" that were recorded in all years in which "variableName" was recorded

availableVariables <- function(dataSet, variableName) {
  comparisonAvailableYears <- map_dfc(dataSet, compareAvailableYears, variableName, dataSet) %>% 
    select_if(is_true)
  return(names(comparisonAvailableYears))
}


# select rows with year in which DGECriteriaNo was recorded

datasetMode <- mergedDataImputeMode %>% 
  filter(year %in% c(2018, 2017, 2016, 2014)) %>%
  dplyr::select(!tidyselect::contains('scaled'))
#   

datasetInterpolation <- mergedDataImputeInterpolation %>% 
  filter(year %in% c(2018, 2017, 2016, 2014)) %>% 
  dplyr::select(!tidyselect::contains('scaled'))



# rlassoEffect performs double selection

# loop for regressions with varying outcome and features

flexibleRegression <- function(response, predictor, dataset) {
  x <- as.matrix(dataset[, !(names(dataset) %in% response)])
  y <- as.matrix(dataset[, response])
  d <- as.matrix(dataset[, predictor])
  rlassoEffect(x, y, d)
}

DSSelfworthRealSubsidy <- flexibleRegression("selfworth", "realSubsidy", mergedDataImputeAll)

DSDayToDaySkillsRealSubsidy <- flexibleRegression("dayToDaySkills", "realSubsidy", mergedDataImputeAll)

DSMealsNoRealSubsidy <- flexibleRegression("mealsNo", "realSubsidy", mergedDataImputeAll)

DSTripsSelfworthRealTripsSubsidy <- flexibleRegression("tripsSelfworth", "realTripsSubsidy", mergedDataImputeAll)

DSTripsDayToDaySkillsRealTripsSubsidy <- flexibleRegression("dayToDaySkills", "realTripsSubsidy", mergedDataImputeAll)

DSTripsNoRealTripsSubsidy <- flexibleRegression("tripsNo", "realTripsSubsidy", mergedDataImputeAll)

DSLessIllDGECriteriaNo <- flexibleRegression("lessIll", "DGECriteriaNo", mergedDataImputeAll)

DSDietaryKnowledgeDGECriteriaNo <- flexibleRegression("dietaryKnowledge", "DGECriteriaNo", mergedDataImputeAll)

DSAppreciateHealthyDGECriteriaNo <- flexibleRegression("appreciateHealthy", "DGECriteriaNo", mergedDataImputeAll)



