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

# availableObservations1 <- mergedData %>% 
#   filter(!is.na(mergedData[, "id"]))
# 
# availableObservations2 <- mergedData %>% 
#   filter(!is.na(mergedData$id))
# 
# availableYears(mergedData, mergedData$kidsTotalNo)
# compareAvailableYears("kidsTotalNo", "DGECriteriaNo", mergedData)

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

# availableVariables(mergedData, "subsidy")
# comparisonAvailableYears <- map_dfc(mergedData, compareAvailableYears, "subsidy", mergedData) 
# 
# comparisonAvailableYears %>% 
#   select_if(is_true)
# 
# identical(mergedData$id), mergedData[,"id"])
# this function selects all variables of a data set that were recorded in all year in which a given variable was recorded
# availableVariables <- function(dataset, variableName) {
#   variable <- dataset[, variableName]
#   availableObservations <- dataset %>% 
#     filter(!is.na(variable))
#   availableYears <- unique(availableObservations$year)
#   availableDataset <- dataset %>% 
#     filter(year %in% availableYears) 
#   availableDataset <- availableDataset[, availableYears(availableDataset) == availableYears]
#   return(str(availableDataset))
# }
# 
# availableVariables(mergedData, "DGECriteriaNo")


# select rows with year in which DGECriteriaNo was recorded

datasetMode <- mergedDataImputeMode %>% 
  filter(year %in% c(2018, 2017, 2016, 2014)) %>%
  dplyr::select(!tidyselect::contains('scaled'))
#   
# names(.) %in% outcomesMeals
datasetInterpolation <- mergedDataImputeInterpolation %>% 
  filter(year %in% c(2018, 2017, 2016, 2014)) %>% 
  dplyr::select(!tidyselect::contains('scaled'))

# NAsPerVariableMergedData <- mergedData %>%
#   summarise_all(list(~ sum(is.na(.)))) %>%
#   arrange(.)
# 
# datasetNewName <- dataset %>% 
#   rename(X3 = realSubsidy)
# 
# fmNewName = paste("participateMore ~", paste(colnames(select(datasetNewName, -"participateMore")), collapse="+"))
# fmNewName = as.formula(fmNewName)
# 
# fmOldName = paste("participateMore ~", paste(colnames(select(dataset, -"participateMore")), collapse="+"))
# fmOldName = as.formula(fmOldName)

#dataset <- join(dataset, )

# rlassoEffect performs double selection

# xOldName <- as.matrix(dataset %>% 
#   select(., -lessIll))
# yOldName <- as.matrix(dataset$lessIll)
# dOldName <- as.matrix(dataset$DGECriteriaNo)
# 
# DSOldName = rlassoEffect(xOldName, yOldName, dOldName)
# 
# summary(DSOldName)
# DSNewName = rlassoEffects(fmNewName, I = ~ DGECriteriaNo + dayToDaySkills + X3, data=datasetNewName)
# DSOldName = rlassoEffects(fmOldName, I = ~ DGECriteriaNo + dayToDaySkills + realSubsidy, data=dataset)

# loop for regressions with varying outcome and features

flexibleRegression <- function(z, dataset) {
  drops <- z
  x <- as.matrix(dataset[, !(names(dataset) %in% drops)])
  y <- as.matrix(dataset[, z])
  d <- as.matrix(dataset$DGECriteriaNo)
  rlassoEffect(x, y, d)
}

DSflexibleTest <- flexibleRegression("selfworth", datasetMode)

saveRDS(DSflexibleTest, "./ANALYSIS/Tables/DSflexibleTest.Rds")

stargazer(DSflexibleTest)
# DSflexibleTest2 <- flexibleRegression("dayToySkills")

# testDS2 <- map(names(dataset), flexibleRegression)

# doubleSelectionRegressions <- map(names(dataset2), flexibleRegression, datasetInterpolation)

# lasso.effect = rlassoEffects(as.matrix(dataset), lessIll, index=3)
# 
# library(hdm); library(ggplot2)
# set.seed(1)
# n = 38 #sample size
# p = 20 # number of variables
# s = 3 # nubmer of non-zero variables
# X = matrix(rnorm(n*p), ncol=p)
# #X[1,1] = NA
# colnames(X) <- paste("X", 1:p, sep="")
# beta = c(rep(3,s), rep(0,p-s))
# y = 1 + X%*%beta + rnorm(n)
# data = data.frame(cbind(y,X))
# data <- data %>% 
# mutate(X1 = dataset$DGECriteriaNo, X2 = dataset$realSubsidy, X3 = dataset$lessIll, X4 = dataset$dayToDaySkills)
# dataOrdinal <- data %>% 
#   rename(DGECriteriaNo = X1, realSubsidy = X2, lessIll = X3) # , dayToDaySkills = X4
# dataVerbal <- data %>% 
#   rename(DGECriteriaNo = X1, realSubsidy = X2, lessIll = X3, dayToDaySkills = X4) 
# colnames(dataOrdinal)[1] <- "y"
# colnames(dataVerbal)[1] <- "y"
# fmOrdinal = paste("y ~", paste(colnames(subset(dataOrdinal, select = -y)), collapse="+"))
# fmOrdinal = as.formula(fmOrdinal) 
# fmVerbal = paste("y ~", paste(colnames(subset(dataVerbal, select = -y)), collapse="+"))
# fmVerbal = as.formula(fmVerbal)  
# #lasso.effect = rlassoEffects(X, y, index=c(1,2,3))
# lasso.effect = rlassoEffects(fmOrdinal, I = ~ DGECriteriaNo + realSubsidy + lessIll + X4, data=dataOrdinal)
# lasso.effect = rlassoEffects(fmVerbal, I = ~ DGECriteriaNo + realSubsidy + lessIll + dayToDaySkills, data=dataVerbal)
# print(lasso.effect)
# summary(lasso.effect)
# confint(lasso.effect)
# plot(lasso.effect)

###model: OLS
##influence of DGEcriterium on health relevant variables

#DGEandLessIll

#DGEandAppreciateHealthy 

#DGEandDietaryKnowledge

#expandedModelLessIll





##approximating Chance Equality with the following proxies 

#dayTodaySkillsAndSubsidy 

#selfworthAndSubsidy

