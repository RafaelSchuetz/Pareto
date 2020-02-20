#https://www.r-bloggers.com/how-to-remove-outliers-in-r/

#remove outliers

#mealsNo

mergedData_mealsNoexcludedOutliers <- quantile(mergedData$mealsNo, probs=c(.25, .75), na.rm = TRUE)

iqr_mealsNo <- IQR(mergedData$mealsNo, na.rm = TRUE)

up_mealsNo <-  Q[2]+1.5*iqr_mealsNo # Upper Range  
low_mealsNo<- Q[1]-1.5*iqr_mealsNo # Lower Range

mealsNoEliminated<- subset(mergedData, mergedData$mealsNo > low_mealsNo & mergedData$mealsNo < up_mealsNo)

