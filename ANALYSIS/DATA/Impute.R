library(tidyimpute)

mergedDataImputeMode <- mergedData %>% 
  select_if(is.numeric) %>% 
  impute_most_freq()
