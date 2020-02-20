library(QuantPsyc)
library(Scale)

#standardized coefficents function 

lm.beta
function (MOD) 
{
  b <- summary(MOD)$coef[-1, 1]
  sx <- sd(MOD$model[-1])
  sy <- sd(MOD$model[1])
  beta <- b * sx/sy
  return(beta)
}

#other way: standardized coefficents

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
