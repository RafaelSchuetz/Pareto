#OLS regressions 

library(estimatr)
library(robustbase)
library(broom)
library(sandwich)
library(lmtest)
library(modelr)
library(texreg)


##subsidy 
#dataset: mergedData

mealsNo_sub.lm <- lm_robust(mealsNo ~ realSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(mealsNo_sub.lm) 



saveRDS(mealsNo_sub.lm,"./ANALYSIS/Tables/mealsNo_sub.lm.Rds")


tripsNo_sub.lm <- lm_robust(tripsNo ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(tripsNo_sub.lm)

texreg(tripsNo_sub.lm)

saveRDS(tripsNo_sub.lm,"./ANALYSIS/Tables/tripsNo_sub.lm.Rds")


#dataset with excluded outliers: mealsNo / tripsNo

mealsNo_sub_ex.lm <- lm(mealsNo ~ realSubsidy, data = mealsNoOutliers)%>%
  extract.lm_robust(include.ci = FALSE)
summary(mealsNo_sub_ex.lm)

texreg(mealsNo_sub_ex.lm)

saveRDS(mealsNo_sub_ex.lm,"./ANALYSIS/Tables/mealsNo_sub_ex.lm.Rds")


tripsNo_sub_ex.lm <- lm(tripsNo ~ realTripsSubsidy, data = tripsNoOutliers)
summary(tripsNo_sub_ex.lm)

saveRDS(tripsNo_sub_ex.lm,"./ANALYSIS/Tables/tripsNo_sub_ex.lm.Rds")



#datset: mergedDataImputeInterpolation

mealsNo_sub_IM.lm <- lm(mealsNo ~ realSubsidy, data = mergedDataImputeInterpolation)
summary(mealsNo_sub.lm)

saveRDS(mealsNo_sub_IM.lm,"./ANALYSIS/Tables/mealsNo_sub_IM.lm.Rds")

tripsNo_sub_IM.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedDataImputeInterpolation)
summary(tripsNo_sub.lm)

saveRDS(tripsNo_sub_IM.lm,"./ANALYSIS/Tables/tripsNo_sub_IM.lm.Rds")
 



##healthinfluence 

#dataset: mergedData


lessIll_DGE.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedData)
summary(lessIll_DGE.lm)

saveRDS(lessIll_DGE.lm,"./ANALYSIS/Tables/lessIll_DGE.lm.Rds")

lessIll_DGE_scaled.lm <- lm(lessIll_scaled ~ DGECriteriaNoScaled, data = mergedData)
summary(lessIll_DGE_scaled.lm) #standardized

saveRDS(lessIll_DGE_scaled.lm,"./ANALYSIS/Tables/lessIll_DGE_scaled.lm.Rds")

Expand_LessIll.lm = lm(lessIll_weighted ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll.lm)

saveRDS(Expand_LessIll.lm,"./ANALYSIS/Tables/Expand_LessIll.lm.Rds")

Expand_LessIll_scaled.lm = lm(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll_scaled.lm) #standardized

saveRDS(Expand_LessIll_scaled.lm,"./ANALYSIS/Tables/Expand_LessIll_scaled.Rds")

dietaryKnowledge_DGE.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

saveRDS(dietaryKnowledge_DGE.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE.lm.Rds")

dietaryKnowledge_DGE_scaled.lm <- lm(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, data = mergedData)
summary(dietaryKnowledge_DGE_scaled.lm) #standardized

saveRDS(dietaryKnowledge_DGE_scaled.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_scaled.Rds")

appreciateHealthy_DGE.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedData)
summary(appreciateHealthy_DGE.lm)

saveRDS(appreciateHealthy_DGE.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE.Rds")

appreciateHealthy_DGE_scaled.lm <- lm(appreciateHealthy_scaled ~ DGECriteriaNoScaled, data = mergedData)
summary(appreciateHealthy_DGE_scaled.lm) #standardized

saveRDS(appreciateHealthy_DGE_scaled.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_scaled.Rds")


#dataset: mergedDataImputeInterpolation
 
lessIll_DGE_IM.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
summary(lessIll_DGE_IM.lm)

saveRDS(lessIll_DGE_IM.lm,"./ANALYSIS/Tables/lessIll_DGE_IM.lm.Rds")

lessIll_DGE_IM_scaled.lm <- lm(lessIll_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)
summary(lessIll_DGE_IM_scaled.lm) #standardized

saveRDS(lessIll_DGE_IM_scaled.lm,"./ANALYSIS/Tables/lessIll_DGE_IM_scaled.lm.Rds")

Expand_LessIll_IM.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy, data = mergedDataImputeInterpolation)
summary(Expand_LessIll_IM.lm)

saveRDS(Expand_LessIll_IM.lm,"./ANALYSIS/Tables/Expand_LessIll_IM.lm.Rds")
# 
Expand_LessIll_IM_scaled.lm = lm(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidy, data = mergedDataImputeInterpolation)
summary(Expand_LessIll_IM_scaled.lm) #standardized

saveRDS(Expand_LessIll_IM_scaled.lm,"./ANALYSIS/Tables/Expand_LessIll_IM_scaled.lm.Rds")

dietaryKnowledge_DGE_IM.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
summary(dietaryKnowledge_DGE_IM.lm)

saveRDS(dietaryKnowledge_DGE_IM.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_IM.lm.Rds")

dietaryKnowledge_DGE_IM_scaled.lm <- lm(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)
summary(dietaryKnowledge_DGE_IM_scaled.lm) #standardized

saveRDS(dietaryKnowledge_DGE_IM_scaled.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_IM_scaled.lm.Rds")

appreciateHealthy_DGE_IM.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
summary(appreciateHealthy_DGE_IM.lm)

saveRDS(appreciateHealthy_DGE_IM.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_IM.lm.Rds")

appreciateHealthy_DGE_IM_scaled.lm <- lm(appreciateHealthy_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)
summary(appreciateHealthy_DGE_IM_scaled.lm) #standardized

saveRDS(appreciateHealthy_DGE_IM_scaled.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_IM_scaled.lm.Rds")


###chance equality
##dataset: mergedData 

#lunch

selfworth.lm <- lm(selfworth_weighted ~ realSubsidy, data = mergedData)
summary(selfworth.lm)

saveRDS(selfworth.lm,"./ANALYSIS/Tables/selfworth.lm.Rds")

selfworth_scaled.lm <- lm(selfworth_scaled ~ realSubsidy, data = mergedData)
summary(selfworth_scaled.lm) #standardized

saveRDS(selfworth_scaled.lm,"./ANALYSIS/Tables/selfworth_scaled.Rds")

dayToDaySkills.lm <- lm(dayToDaySkills_weighted ~ realSubsidy, data = mergedData)
summary(dayToDaySkills.lm)

saveRDS(dayToDaySkills.lm,"./ANALYSIS/Tables/dayToDaySkills.lm.Rds")

dayToDaySkills_scaled.lm <- lm(dayToDaySkills_scaled ~ realSubsidy, data = mergedData)
summary(dayToDaySkills_scaled.lm) #standardized

saveRDS(dayToDaySkills_scaled.lm,"./ANALYSIS/Tables/dayToDaySkills_scaled.lm.Rds")
 
#trips
selfworthTrips.lm <- lm(tripsSelfworth_weighted ~ realTripsSubsidy, data = mergedData)
summary(selfworthTrips.lm)

saveRDS(selfworthTrips.lm,"./ANALYSIS/Tables/selfworthTrips.lm.Rds")

selfworthTrips_scaled.lm <- lm(tripsSelfworth_scaled ~ realTripsSubsidy, data = mergedData)
summary(selfworthTrips_scaled.lm) #standardized

saveRDS(selfworthTrips_scaled.lm,"./ANALYSIS/Tables/selfworthTrips_scaled.Rds")

dayToDaySkillsTrips.lm <- lm(tripsDayToDaySkills_weighted ~ realTripsSubsidy, data = mergedData)
summary(dayToDaySkillsTrips.lm)

saveRDS(dayToDaySkillsTrips.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips.lm.Rds")

dayToDaySkillsTrips_scaled.lm <- lm(tripsDayToDaySkills_scaled ~ realTripsSubsidy, data = mergedData)
summary(dayToDaySkillsTrips_scaled.lm) #standardized

saveRDS(dayToDaySkillsTrips_scaled.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_scaled.lm.Rds")


#dataset: mergedDataImputeInterpolation
#lunch
selfworth_IM.lm <- lm(selfworth ~ realSubsidy, data = mergedDataImputeInterpolation)
summary(selfworth_IM.lm)

saveRDS(selfworth_IM.lm,"./ANALYSIS/Tables/selfworth_IM.lm.Rds")

selfworth_IM_scaled.lm <- lm(selfworth_scaled ~ realSubsidy, data = mergedDataImputeInterpolation)
summary(selfworth_IM_scaled.lm) #standardized

saveRDS(selfworth_IM_scaled.lm,"./ANALYSIS/Tables/selfworth_IM_scaled.lm.Rds")
# 
dayToDaySkills_IM.lm <- lm(dayToDaySkills ~ realSubsidy, data = mergedDataImputeInterpolation)
summary(dayToDaySkills_IM.lm)

saveRDS(dayToDaySkills_IM.lm,"./ANALYSIS/Tables/dayToDaySkills_IM.lm.Rds")
# 
dayToDaySkills_IM_scaled.lm <- lm(dayToDaySkills_scaled ~ realSubsidy, data = mergedDataImputeInterpolation)
summary(dayToDaySkills_IM_scaled.lm) #standardized

saveRDS(dayToDaySkills_IM_scaled.lm,"./ANALYSIS/Tables/dayToDaySkills_IM_scaled.lm.Rds")
# 
#trips 
selfworthTrips_IM.lm <- lm(tripsSelfworth ~ realTripsSubsidy, data = mergedDataImputeInterpolation)
summary(selfworthTrips_IM.lm)

saveRDS(selfworthTrips_IM.lm,"./ANALYSIS/Tables/selfworthTrips_IM.lm.Rds")

selfworthTrips_IM_scaled.lm <- lm(tripsSelfworth_scaled ~ realTripsSubsidy, data = mergedDataImputeInterpolation)
summary(selfworthTrips_IM_scaled.lm) #standardized

saveRDS(selfworthTrips_IM_scaled.lm,"./ANALYSIS/Tables/selfworthTrips_IM_scaled.lm.Rds")
# 
dayToDaySkillsTrips_IM.lm <- lm(tripsDayToDaySkills ~ realTripsSubsidy, data = mergedDataImputeInterpolation)
summary(dayToDaySkillsTrips_IM.lm)

saveRDS(dayToDaySkillsTrips_IM.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_IM.lm.Rds")
# 
dayToDaySkillsTrips_IM_scaled.lm <- lm(tripsDayToDaySkills_scaled ~ realTripsSubsidy, data = mergedDataImputeInterpolation)
summary(dayToDaySkillsTrips_IM_scaled.lm) #standardized

saveRDS(dayToDaySkillsTrips_IM_scaled.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_IM_scaled.lm.Rds")
# 

