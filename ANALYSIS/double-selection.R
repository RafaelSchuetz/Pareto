# use package hdm to perform double selection: OLS regression after feature selection
# https://insightr.wordpress.com/2017/08/04/the-package-hdm-for-double-selection-inference-with-a-simple-example/

library(hdm)
library(tidyselect)

# select rows with year in which DGECriteriaNo was recorded

datasetMode <- mergedDataImputeMode %>% 
  filter(year %in% c(2018, 2017, 2016, 2014)) %>%
  dplyr::select(!tidyselect::contains('scaled'))

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



