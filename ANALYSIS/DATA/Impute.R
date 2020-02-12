library(tidyimpute)
library(imputeMissings)

mergedDataImputeMode <- mergedData[, colSums(is.na(mergedData)) <= sum(is.na(mergedData$DGECriteriaNo))] %>% 
  #select_if(is.numeric) %>%    
  impute_most_freq()

mergedDataImputeMode <- mergedData[, colSums(is.na(mergedData)) <= sum(is.na(mergedData$DGECriteriaNo))]  

impute(mergedDataImputeMode)

