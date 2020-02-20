#OLS regressions 

library(QuantPsyc)
library(Scale)


##subsidy 
#dataset: mergedData

mealsNo_sub.lm <- lm(mealsNo ~ realSubsidy, data = mergedData)
summary(mealsNo_sub.lm)

mealsNo_sub_scaled.lm <- lm(mealsNo_scaled ~ realSubsidy_scaled, data = mergedData)
summary(mealsNo_sub_scaled.lm) #standardized

tripsNo_sub.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedData)
summary(tripsNo_sub.lm)

tripsNo_sub_scaled.lm <- lm(tripsNo_scaled ~ realTripsSubsidy_scaled, data = mergedData)
summary(tripsNo_sub_scaled.lm) #standardized

#dataset with excluded outliers: mealsNo / tripsNo

mealsNo_sub_ex.lm <- lm(mealsNo ~ realSubsidy, data = mealsNoEliminated)
summary(mealsNo_sub_ex.lm)

mealsNo_sub_ex_scaled.lm <- lm(mealsNo_scaled ~ realSubsidy_scaled, data = mealsNoEliminated)
summary(mealsNo_sub_ex_scaled.lm) #standardized


# #datset: mergedDataImputeMode
# 
# 
# mealsNo_sub_IM.lm <- lm(mealsNo ~ realSubsidy, data = mergedDataImputeMode)
# summary(mealsNo_sub.lm)
# 
# mealsNo_sub_scaled_IM.lm <- lm(mealsNo_scaled ~ realSubsidy_scaled, data = mergedDataImputeMode)
# summary(mealsNo_sub_scaled_IM.lm) #standardized
# 
# tripsNo_sub_IM.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedDataImputeMode)
# summary(tripsNo_sub.lm)
# 
# tripsNo_sub_scaled_IM.lm <- lm(tripsNo_scaled ~ realTripsSubsidy_scaled, data = mergedDataImputeMode)
# summary(tripsNo_sub_scaled_IM.lm) #standardized


##healthinfluence 

#dataset: mergedData


lessIll_DGE.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedData)
summary(lessIll_DGE.lm)

lessIll_DGE_scaled.lm <- lm(lessIll_scaled ~ DGECriteriaNo_scaled, data = mergedData)
summary(lessIll_DGE.lm) #standardized

Expand_LessIll.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll.lm)

Expand_LessIll_scaled.lm = lm(lessIll_scaled ~ DGECriteriaNo_scaled + regionalProducts_scaled + yearsSupportSince + realSubsidy_scaled + state, data = mergedData)
summary(Expand_LessIll_scaled.lm) #standardized

dietaryKnowledge_DGE.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

dietaryKnowledge_DGE_scaled.lm <- lm(dietaryKnowledge_scaled ~ DGECriteriaNo_scaled, data = mergedData)
summary(dietaryKnowledge_DGE_scaled.lm) #standardized

appreciateHealthy_DGE.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

appreciateHealthy_DGE_scaled.lm <- lm(appreciateHealthy_scaled ~ DGECriteriaNo_scaled, data = mergedData)
summary(dietaryKnowledge_DGE_scaled.lm) #standardized


# #dataset: mergedDataImputeMode
# 
# lessIll_DGE_IM.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedDataImputeMode)
# summary(lessIll_DGE_IM.lm)
# 
# lessIll_DGE_IM_scaled.lm <- lm(lessIll_scaled ~ DGECriteriaNo_scaled, data = mergedDataImputeMode)
# summary(lessIll_DGE_IM_scaled.lm) #standardized
# 
# Expand_LessIll_IM.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedDataImputeMode)
# summary(Expand_LessIll_IM.lm)
# 
# Expand_LessIll_IM_scaled.lm = lm(lessIll_scaled ~ DGECriteriaNo_scaled + regionalProducts_scaled + yearsSupportSince + realSubsidy_scaled + state, data = mergedDataImputeMode)
# summary(Expand_LessIll_IM_scaled.lm) #standardized
# 
# dietaryKnowledge_DGE_IM.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedDataImputeMode)
# summary(dietaryKnowledge_DGE_IM.lm)
# 
# dietaryKnowledge_DGE_IM_scaled.lm <- lm(dietaryKnowledge_scaled ~ DGECriteriaNo_scaled, data = mergedDataImputeMode)
# summary(dietaryKnowledge_DGE_IM_scaled.lm) #standardized
# 
# appreciateHealthy_DGE_IM.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedDataImputeMode)
# summary(dietaryKnowledge_DGE_IM.lm)
# 
# appreciateHealthy_DGE_IM_scaled.lm <- lm(appreciateHealthy_scaled ~ DGECriteriaNo_scaled, data = mergedDataImputeMode)
# summary(dietaryKnowledge_DGE_IM_scaled.lm) #standardized

###chance equality
##dataset: mergedData 

selfworth.lm <- lm(selfworth ~ realSubsidy, data = mergedData)
summary(selfworth.lm)

selfworth_scaled.lm <- lm(selfworth_scaled ~ realSubsidy_scaled, data = mergedData)
summary(selfworth_scaled.lm) #standardized

dayToDaySkills.lm <- lm(dayToDaySkills ~ realSubsidy, data = mergedData)
summary(dayToDaySkills.lm)

dayToDaySkills_scaled.lm <- lm(dayToDaySkills_scaled ~ realSubsidy_scaled, data = mergedData)
summary(dayToDaySkills_scaled.lm)
 
# ##dataset: mergedDataImputeMode
# 
# selfworth_IM.lm <- lm(selfworth ~ realSubsidy, data = mergedDataImputeMode)
# summary(selfworth_IM.lm)
# 
# selfworth_IM_scaled.lm <- lm(selfworth_scaled ~ realSubsidy_scaled, data = mergedDataImputeMode)
# summary(selfworth_IM_scaled.lm) #standardized
# 
# dayToDaySkills_IM.lm <- lm(dayToDaySkills ~ realSubsidy, data = mergedDataImputeMode)
# summary(dayToDaySkills_IM.lm)
# 
# dayToDaySkills_IM_scaled.lm <- lm(dayToDaySkills_scaled ~ realSubsidy_scaled, data = mergedDataImputeMode)
# summary(dayToDaySkills_IM_scaled.lm)
# 

