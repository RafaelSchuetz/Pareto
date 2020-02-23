library(tidyimpute)
library(imputeMissings)
library(zoo)

# impute missing values in all variables where there are fewer missing values than in DGECriteriaNo

## impute mode

mergedDataImputeMode <- mergedData[, colSums(is.na(mergedData)) <= sum(is.na(mergedData$DGECriteriaNo))] %>% 
  select_if(is.numeric) %>%    
  impute_most_freq()

## impute linear interpolation by group

## https://stackoverflow.com/questions/33696795/r-interpolation-of-nas-by-group

# [, colSums(is.na(mergedData)) <= sum(is.na(mergedData$DGECriteriaNo))]

mergedDataImputeInterpolation <- mergedData %>% 
  select_if(is.numeric) %>%
  group_by(id) %>% 
  mutate_all(na.approx, na.rm = FALSE) %>% 
  ungroup()

selectionInterpolation <- mergedDataImputeInterpolation[, c("id", "year", outcomesMeals)]

#   NAsPerVariableMergedDataImputeInterpolation <- mergedDataImputeInterpolation %>%
#   summarise_all(list(~ sum(is.na(.)))) %>%
#   arrange(.)

NAsPerVariableMergedDataImputeInterpolation <- mergedDataImputeInterpolation %>%
  purrr::map_dfc(~sum(is.na(.))) %>% 
  arrange(.)

NAsPerVariableMergedDataImputeInterpolation

NAsPerVariableMergedData <- mergedData %>% 
  select_if(is.numeric) %>% 
  map(~sum(is.na(.)))
# %>%
  # mutate(ppentInterp = na.approx(ppent, na.rm = FALSE))

# mergedDataImputeInterpolation <- ungroup(purrr::modify(mergedDataImputeInterpolation, na.approx, na.rm = FALSE))

# mergedDataImputeTest <- mergedDataImputeInterpolation %>% 
  # purrr::modify_at("tasksLunch", na.approx, na.rm = FALSE)

# mergedDataImputeTest <- mergedDataImputeInterpolation %>% 
#   mutate_all(na.approx, na.rm = FALSE)
# 
# mergedDataImputeMode <- mergedData[, colSums(is.na(mergedData)) <= sum(is.na(mergedData$DGECriteriaNo))]  
# 
# impute(mergedDataImputeMode)

