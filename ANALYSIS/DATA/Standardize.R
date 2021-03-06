# library(Scale)

# function for standardizing

standardizeVariable <- function(expr, df) {
  expr <- enquo(expr)
  name <- paste0(quo_name(expr), "_scaled")
  mutate(df,
         !! name := scale(!! expr)
  )
}


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

tripsNoEliminated$mealsNo_scaled<-scale(tripsNoEliminated$mealsNo)
tripsNoEliminated$realSubsidy_scaled<-scale(tripsNoEliminated$realSubsidy)
tripsNoEliminated$tripsNo_scaled<-scale(tripsNoEliminated$tripsNo)
tripsNoEliminated$realTripsSubsidy_scaled<-scale(tripsNoEliminated$realTripsSubsidy)
tripsNoEliminated$lessIll_scaled<-scale(tripsNoEliminated$lessIll)
tripsNoEliminated$DGECriteriaNo_scaled<-scale(tripsNoEliminated$DGECriteriaNo)
tripsNoEliminated$regionalProducts_scaled<-scale(tripsNoEliminated$regionalProducts)
tripsNoEliminated$dietaryKnowledge_scaled<-scale(tripsNoEliminated$dietaryKnowledge)
tripsNoEliminated$selfworth_scaled<-scale(tripsNoEliminated$selfworth)
tripsNoEliminated$dayToDaySkills_scaled<-scale(tripsNoEliminated$dayToDaySkills)

#mergedDataImputeMode

# mergedDataImputeMode$mealsNo_scaled<-scale(mergedDataImputeMode$mealsNo)
# mergedDataImputeMode$realSubsidy_scaled<-scale(mergedDataImputeMode$realSubsidy)
# mergedDataImputeMode$tripsNo_scaled<-scale(mergedDataImputeMode$tripsNo)
# mergedDataImputeMode$realTripsSubsidy_scaled<-scale(mergedDataImputeMode$realTripsSubsidy)
mergedDataImputeInterpolation$lessIll_scaled<-scale(mergedDataImputeInterpolation$lessIll)
# mergedDataImputeMode$DGECriteriaNo_scaled<-scale(mergedDataImputeMode$DGECriteriaNo)
mergedDataImputeInterpolation$regionalProducts_scaled<-scale(mergedDataImputeInterpolation$regionalProducts)
# mergedDataImputeMode$dietaryKnowledge_scaled<-scale(mergedDataImputeMode$dietaryKnowledge)
# mergedDataImputeMode$selfworth_scaled<-scale(mergedDataImputeMode$selfworth)
mergedDataImputeInterpolation$dayToDaySkills_scaled<-scale(mergedDataImputeInterpolation$dayToDaySkills)
