# Names of variables used in models

modelVariablesNames <- c('lessIll',
                    'appreciateHealthy',
                    'dietaryKnowledge',
                    'DGECriteriaNo',
                    'selfworth',
                    'dayToDaySkills',
                    'tripsSelfworth',
                    'tripsDayToDaySkills',
                    'realSubsidyPerBeneficiary',
                    'realTripsSubsidyPerBeneficiary'
                    )

# outcomes not directly related to beneficiaries 

generalOutcomes <- c(
  'enoughFood',
  'enoughStaffLunch',
  'enoughStaffActivities',
  'qualitySatisfies',
  'regionalProducts',
  'cultureReligion',
  'unsweetenedDrinks',
  'DGECriteriaBinary',
  'recipesBinary',
  'tripsCHILDRENSuggestions',
  'suggestionsForCHILDREN',
  'organicFoodstuff',
  'seasonalFoodstuff',
  'parentalDialog',
  'friendlyEnvironment',
  'menu',
  'menuCycle',
  'trips',
  'claimBTP',
  'benefitBTP',
  'noSchoolLunch',
  'expensiveSchoolLunch'
)

generalOutcomes_scaled <- purrr::modify(generalOutcomes, paste0, '_scaled')

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
  if(all(availableYears(dataSet, secondVariableName) %in% availableYears(dataSet, firstVariableName))){
    return(TRUE)
  }
  else {
    return(FALSE)
  }
}

# all(availableYears(mergedData, "regionalProducts") %in% availableYears(mergedData, 'qualitySatisfies')) 
# 
# compareAvailableYears("regionalProducts", 'qualitySatisfies', mergedData)
# compareAvailableYears("regionalProducts", 'claimBTP', mergedData)

# This function returns the names of all variables in "dataSet" that were recorded in all years in which "variableName" was recorded

availableVariables <- function(dataSet, variableName) {
  comparisonAvailableYears <- map_dfc(dataSet, compareAvailableYears, variableName, dataSet) %>% 
    select_if(is_true)
  return(names(comparisonAvailableYears))
}

alwaysRecordedVariablesMeals <- availableVariables(mergedData, "subsidy")
alwaysRecordedVariablesTrips <- availableVariables(mergedData, 'tripsDayToDaySkills_scaled')

