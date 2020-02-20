library(Scale)

#standardized coefficents

#mergedData

mergedData$mealsNo_scaled<-scale(mergedData$mealsNo)
mergedData$realSubsidy_scaled<-scale(mergedData$realSubsidy)
mergedData$tripsNo_scaled<-scale(mergedData$tripsNo)
mergedData$realTripsSubsidy_scaled<-scale(mergedData$realTripsSubsidy)
mergedData$lessIll_scaled<-scale(mergedData$lessIll)
mergedData$DGECriteriaNo_scaled<-scale(mergedData$DGECriteriaNo)
mergedData$regionalProducts_scaled<-scale(mergedData$regionalProducts)
mergedData$dietaryKnowledge_scaled<-scale(mergedData$dietaryKnowledge)
mergedData$selfworth_scaled<-scale(mergedData$selfworth)
mergedData$dayToDaySkills_scaled<-scale(mergedData$dayToDaySkills)

#excluded Outliers

#mealsNo

mealsNoEliminated$mealsNo_scaled<-scale(mealsNoEliminated$mealsNo)
mealsNoEliminated$realSubsidy_scaled<-scale(mealsNoEliminated$realSubsidy)
mealsNoEliminated$tripsNo_scaled<-scale(mealsNoEliminated$tripsNo)
mealsNoEliminated$realTripsSubsidy_scaled<-scale(mealsNoEliminated$realTripsSubsidy)
mealsNoEliminated$lessIll_scaled<-scale(mealsNoEliminated$lessIll)
mealsNoEliminated$DGECriteriaNo_scaled<-scale(mealsNoEliminated$DGECriteriaNo)
mealsNoEliminated$regionalProducts_scaled<-scale(mealsNoEliminated$regionalProducts)
mealsNoEliminated$dietaryKnowledge_scaled<-scale(mealsNoEliminated$dietaryKnowledge)
mealsNoEliminated$selfworth_scaled<-scale(mealsNoEliminated$selfworth)
mealsNoEliminated$dayToDaySkills_scaled<-scale(mealsNoEliminated$dayToDaySkills)

#tripsNo

mergedDataImputeMode$mealsNo_scaled<-scale(mergedDataImputeMode$mealsNo)
mergedDataImputeMode$realSubsidy_scaled<-scale(mergedDataImputeMode$realSubsidy)
mergedDataImputeMode$tripsNo_scaled<-scale(mergedDataImputeMode$tripsNo)
mergedDataImputeMode$realTripsSubsidy_scaled<-scale(mergedDataImputeMode$realTripsSubsidy)
mergedDataImputeMode$lessIll_scaled<-scale(mergedDataImputeMode$lessIll)
mergedDataImputeMode$DGECriteriaNo_scaled<-scale(mergedDataImputeMode$DGECriteriaNo)
mergedDataImputeMode$regionalProducts_scaled<-scale(mergedDataImputeMode$regionalProducts)
mergedDataImputeMode$dietaryKnowledge_scaled<-scale(mergedDataImputeMode$dietaryKnowledge)
mergedDataImputeMode$selfworth_scaled<-scale(mergedDataImputeMode$selfworth)
mergedDataImputeMode$dayToDaySkills_scaled<-scale(mergedDataImputeMode$dayToDaySkills)
