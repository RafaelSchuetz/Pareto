library(tidyimpute)
library(imputeMissings)
library(zoo)

# impute missing values in all variables where there are less missing values than in DGECriteriaNo

## impute mode

mergedDataImputeMode <- mergedData[, colSums(is.na(mergedData)) <= sum(is.na(mergedData$DGECriteriaNo))] %>% 
  select_if(is.numeric) %>%    
  impute_most_freq()

## impute linear interpolation

### order by year and id

# [, colSums(is.na(mergedData)) <= sum(is.na(mergedData$DGECriteriaNo))]

mergedDataImputeInterpolation <- mergedData %>% 
  select_if(is.numeric) %>%
  arrange(id, year) %>% 
  group_by(id) %>% 
  mutate_all(na.approx, na.rm = FALSE) %>% 
  ungroup()

NAsPerVariableMergedDataImputeInterpolation <- mergedDataImputeInterpolation %>%
  summarise_all(list(~ sum(is.na(.)))) %>%
  arrange(.)

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

