#OLS regressions 

library(estimatr)

#NOCHKEINEMODELLEMITOUTLIERN ERSTELLEN

##subsidy 
#dataset: mergedData

mealsNo_sub.lm <- lm_robust(mealsNo ~ realSubsidy, data = mergedData)
summary(mealsNo_sub.lm)

saveRDS(mealsNo_sub.lm,"./ANALYSIS/Tables/mealsNo_sub.lm.Rds")

tripsNo_sub.lm <- lm_robust(tripsNo ~ realTripsSubsidy, data = mergedData)
summary(tripsNo_sub.lm)

saveRDS(tripsNo_sub.lm,"./ANALYSIS/Tables/tripsNo_sub.lm.Rds")


#dataset with excluded outliers: mealsNo / tripsNo

mealsNo_sub_ex.lm <- lm_robust(mealsNo ~ realSubsidy, data = mealsNoEliminated)
summary(mealsNo_sub_ex.lm)

saveRDS(mealsNo_sub_ex.lm,"./ANALYSIS/Tables/mealsNo_sub_ex.lm.Rds")

tripsNo_sub_ex.lm <- lm_robust(tripsNo ~ realTripsSubsidy, data = tripsNoEliminated)
summary(tripsNo_sub_ex.lm)

saveRDS(tripsNo_sub_ex.lm,"./ANALYSIS/Tables/tripsNo_sub_ex.lm.Rds")



# #datset: mergedDataImputeAll
# 
# 
# mealsNo_sub_IM.lm <- lm_robust(mealsNo ~ realSubsidy, data = mergedDataImputeAll)
# summary(mealsNo_sub.lm)
# 
# tripsNo_sub_IM.lm <- lm_robust(tripsNo ~ realTripsSubsidy, data = mergedDataImputeAll)
# summary(tripsNo_sub.lm)
# 



##healthinfluence 

#dataset: mergedData


lessIll_DGE.lm <- lm_robust(lessIll ~ DGECriteriaNo, data = mergedData)
summary(lessIll_DGE.lm)

saveRDS(lessIll_DGE.lm,"./ANALYSIS/Tables/lessIll_DGE.lm.Rds")

lessIll_DGE_scaled.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNo_scaled, data = mergedData)
summary(lessIll_DGE.lm) #standardized

saveRDS(lessIll_DGE_scaled.lm,"./ANALYSIS/Tables/lessIll_DGE_scaled.lm.Rds")


Expand_LessIll.lm = lm_robust(lessIll_weighted ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll.lm)

saveRDS(Expand_LessIll.lm,"./ANALYSIS/Tables/Expand_LessIll.lm.Rds")

Expand_LessIll_scaled.lm = lm_robust(lessIll_scaled ~ DGECriteriaNo_scaled + regionalProducts_scaled + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll_scaled.lm) #standardized

saveRDS(Expand_LessIll_scaled.lm,"./ANALYSIS/Tables/Expand_LessIll_scaled.Rds")

dietaryKnowledge_DGE.lm <- lm_robust(dietaryKnowledge ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

saveRDS(dietaryKnowledge_DGE.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE.lm.Rds")

dietaryKnowledge_DGE_scaled.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNo_scaled, data = mergedData)
summary(dietaryKnowledge_DGE_scaled.lm) #standardized

saveRDS(dietaryKnowledge_DGE_scaled.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_scaled.Rds")

appreciateHealthy_DGE.lm <- lm_robust(appreciateHealthy ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

saveRDS(appreciateHealthy_DGE.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE.Rds")

appreciateHealthy_DGE_scaled.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNo_scaled, data = mergedData)
summary(dietaryKnowledge_DGE_scaled.lm) #standardized

saveRDS(appreciateHealthy_DGE_scaled.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_scaled.Rds")


# #dataset: mergedDataImputeAll
# 
# lessIll_DGE_IM.lm <- lm_robust(lessIll ~ DGECriteriaNo, data = mergedDataImputeAll)
# summary(lessIll_DGE_IM.lm)
# 
# lessIll_DGE_IM_scaled.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNo_scaled, data = mergedDataImputeAll)
# summary(lessIll_DGE_IM_scaled.lm) #standardized
# 
# Expand_LessIll_IM.lm = lm_robust(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedDataImputeAll)
# summary(Expand_LessIll_IM.lm)
# 
# Expand_LessIll_IM_scaled.lm = lm_robust(lessIll_scaled ~ DGECriteriaNo_scaled + regionalProducts_scaled + yearsSupportSince + realSubsidy + state, data = mergedDataImputeAll)
# summary(Expand_LessIll_IM_scaled.lm) #standardized
# 
# dietaryKnowledge_DGE_IM.lm <- lm_robust(dietaryKnowledge ~ DGECriteriaNo, data = mergedDataImputeAll)
# summary(dietaryKnowledge_DGE_IM.lm)
# 
# dietaryKnowledge_DGE_IM_scaled.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNo_scaled, data = mergedDataImputeAll)
# summary(dietaryKnowledge_DGE_IM_scaled.lm) #standardized
# 
# appreciateHealthy_DGE_IM.lm <- lm_robust(appreciateHealthy ~ DGECriteriaNo, data = mergedDataImputeAll)
# summary(dietaryKnowledge_DGE_IM.lm)
# 
# appreciateHealthy_DGE_IM_scaled.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNo_scaled, data = mergedDataImputeAll)
# summary(dietaryKnowledge_DGE_IM_scaled.lm) #standardized

###chance equality
##dataset: mergedData 

selfworth.lm <- lm_robust(selfworth_weighted ~ realSubsidy, data = mergedData)
summary(selfworth.lm)

saveRDS(selfworth.lm,"./ANALYSIS/Tables/selfworth.lm.Rds")

selfworth_scaled.lm <- lm_robust(selfworth_scaled ~ realSubsidy, data = mergedData)
summary(selfworth_scaled.lm) #standardized

saveRDS(selfworth_scaled.lm,"./ANALYSIS/Tables/selfworth_scaled.Rds")

dayToDaySkills.lm <- lm_robust(dayToDaySkills_weighted ~ realSubsidy, data = mergedData)
summary(dayToDaySkills.lm)

saveRDS(dayToDaySkills.lm,"./ANALYSIS/Tables/dayToDaySkills.lm.Rds")

dayToDaySkills_scaled.lm <- lm_robust(dayToDaySkills_scaled ~ realSubsidy, data = mergedData)
summary(dayToDaySkills_scaled.lm) #standardized

saveRDS(dayToDaySkills_scaled.lm,"./ANALYSIS/Tables/dayToDaySkills_scaled.lm.Rds")
 
# ##dataset: mergedDataImputeAll
# 
# selfworth_IM.lm <- lm_robust(selfworth ~ realSubsidy, data = mergedDataImputeAll)
# summary(selfworth_IM.lm)
# 
# selfworth_IM_scaled.lm <- lm_robust(selfworth_scaled ~ realSubsidy_scaled, data = mergedDataImputeAll)
# summary(selfworth_IM_scaled.lm) #standardized
# 
# dayToDaySkills_IM.lm <- lm_robust(dayToDaySkills ~ realSubsidy, data = mergedDataImputeAll)
# summary(dayToDaySkills_IM.lm)
# 
# dayToDaySkills_IM_scaled.lm <- lm_robust(dayToDaySkills_scaled ~ realSubsidy_scaled, data = mergedDataImputeAll)
# summary(dayToDaySkills_IM_scaled.lm)
# 



