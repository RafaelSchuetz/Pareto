
#OLS

##subsidy 
#dataset: mergedData

mealsNo_sub.lm <- lm(mealsNo ~ realSubsidy, data = mergedData)
summary(mealsNo_sub.lm)

tripsNo_sub.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedData)
summary(tripsNo_sub.lm)

#datset: mergedDataImputeMode

mealsNo_sub_IM.lm <- lm(mealsNo ~ realSubsidy, data = mergedDataImputeMode)
summary(mealsNo_sub.lm)

tripsNo_sub_IM.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedDataImputeMode)
summary(tripsNo_sub.lm)


##healthinfluence 

#dataset: mergedData

lessIll_DGE.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedData)
summary(lessIll_DGE.lm)

Expand_LessIll.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll.lm)

dietaryKnowledge_DGE.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

appreciateHealthy_DGE.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

#dataset: mergedDataImputeMode
lessIll_DGE_IM.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedDataImputeMode)
summary(lessIll_DGE_IM.lm)

Expand_LessIll_IM.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedDataImputeMode)
summary(Expand_LessIll_IM.lm)

dietaryKnowledge_DGE_IM.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedDataImputeMode)
summary(dietaryKnowledge_DGE_IM.lm)

appreciateHealthy_DGE_IM.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedDataImputeMode)
summary(dietaryKnowledge_DGE_IM.lm)

##chance equality
#dataset: mergedData 

selfworth.lm <- lm(selfworth ~ realSubsidy, data = mergedData)
summary(selfworth.lm)

dayToDaySkills.lm <- lm(dayToDaySkills ~ realSubsidy, data = mergedData)
summary(dayToDaySkills.lm)

#dataset: mergedDataImputeMode

selfworth_IM.lm <- lm(selfworth ~ realSubsidy, data = mergedDataImputeMode)
summary(selfworth_IM.lm)

dayToDaySkills_IM.lm <- lm(dayToDaySkills ~ realSubsidy, data = mergedDataImputeMode)
summary(dayToDaySkills_IM.lm)



