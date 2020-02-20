#https://www.r-bloggers.com/how-to-remove-outliers-in-r/

library(dplyr)

#remove outliers

#mealsNo

mergedData_mealsNoexcludedOutliers <- quantile(mergedData$mealsNo, probs=c(.25, .75), na.rm = TRUE)

iqr_mealsNo <- IQR(mergedData$mealsNo, na.rm = TRUE)

up_mealsNo <-  mergedData_mealsNoexcludedOutliers[2]+1.5*iqr_mealsNo # Upper Range  
low_mealsNo<- mergedData_mealsNoexcludedOutliers[1]-1.5*iqr_mealsNo # Lower Range

outlier_ID <- filter(mergedData, mergedData$mealsNo <= low_mealsNo | mergedData$mealsNo >= up_mealsNo) %>%
  select(id) %>% unique() %>% as.vector()

mealsNoEliminated <- mergedData %>% 
  filter(!(mergedData$id %in% outlier_ID))