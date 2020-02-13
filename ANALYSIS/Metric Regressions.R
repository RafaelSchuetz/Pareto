#OLS regressions & their standardized coefficents

library(QuantPsyc)

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

##subsidy 
#dataset: mergedData

mealsNo_sub.lm <- lm(mealsNo ~ realSubsidy, data = mergedData)
summary(mealsNo_sub.lm)

lm.beta(mealsNo_sub.lm) #standardized

tripsNo_sub.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedData)
summary(tripsNo_sub.lm)

lm.beta(tripsNo_sub.lm) #standardized

#datset: mergedDataImputeMode

mealsNo_sub_IM.lm <- lm(mealsNo ~ realSubsidy, data = mergedDataImputeMode)
summary(mealsNo_sub.lm)

lm.beta(mealsNo_sub_IM.lm) #standardized

tripsNo_sub_IM.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedDataImputeMode)
summary(tripsNo_sub.lm)

lm.beta(tripsNo_sub_IM.lm) #standardized


##healthinfluence 

#dataset: mergedData

lessIll_DGE.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedData)
summary(lessIll_DGE.lm)

lm.beta(lessIll_DGE.lm) #standardized

Expand_LessIll.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll.lm)

lm.beta(Expand_LessIll.lm) #standardized

dietaryKnowledge_DGE.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

lm.beta(dietaryKnowledge_DGE.lm) #standardized

appreciateHealthy_DGE.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

lm.beta(appreciateHealthy_DGE.lm) #standardized

#dataset: mergedDataImputeMode
lessIll_DGE_IM.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedDataImputeMode)
summary(lessIll_DGE_IM.lm)

lm.beta(lessIll_DGE_IM.lm) #standardized

Expand_LessIll_IM.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedDataImputeMode)
summary(Expand_LessIll_IM.lm)

lm.beta(Expand_LessIll_IM.lm) #standardized

dietaryKnowledge_DGE_IM.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedDataImputeMode)
summary(dietaryKnowledge_DGE_IM.lm)

lm.beta(dietaryKnowledge_DGE_IM.lm) #standardized

appreciateHealthy_DGE_IM.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedDataImputeMode)
summary(dietaryKnowledge_DGE_IM.lm)

lm.beta(appreciateHealthy_DGE_IM.lm) #standardized

###chance equality
##dataset: mergedData 

selfworth.lm <- lm(selfworth ~ realSubsidy, data = mergedData)
summary(selfworth.lm)

lm.beta(selfworth.lm) #standardized

dayToDaySkills.lm <- lm(dayToDaySkills ~ realSubsidy, data = mergedData)
summary(dayToDaySkills.lm)

lm.beta(dayToDaySkills.lm) #standardized
 
##dataset: mergedDataImputeMode

selfworth_IM.lm <- lm(selfworth ~ realSubsidy, data = mergedDataImputeMode)
summary(selfworth_IM.lm)

lm.beta(selfworth_IM.lm) #standardized

dayToDaySkills_IM.lm <- lm(dayToDaySkills ~ realSubsidy, data = mergedDataImputeMode)
summary(dayToDaySkills_IM.lm)

lm.beta(dayToDaySkills_IM.lm) #standardized


