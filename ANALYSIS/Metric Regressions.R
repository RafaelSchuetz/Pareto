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

texreg(mealsNo_sub.lm)

saveRDS(mealsNo_sub.lm,"./ANALYSIS/Tables/mealsNo_sub.lm.Rds")


tripsNo_sub.lm <- lm_robust(tripsNo ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(tripsNo_sub.lm)

texreg(tripsNo_sub.lm)

saveRDS(tripsNo_sub.lm,"./ANALYSIS/Tables/tripsNo_sub.lm.Rds")


#dataset with excluded outliers: mealsNo / tripsNo

mealsNo_sub_ex.lm <- lm_robust(mealsNo ~ realSubsidy, data = mealsNoOutliers)%>%
  extract.lm_robust(include.ci = FALSE)
summary(mealsNo_sub_ex.lm)

texreg(mealsNo_sub_ex.lm)

saveRDS(mealsNo_sub_ex.lm,"./ANALYSIS/Tables/mealsNo_sub_ex.lm.Rds")


tripsNo_sub_ex.lm <- lm_robust(tripsNo ~ realTripsSubsidy, data = tripsNoOutliers)%>%
  extract.lm_robust(include.ci = FALSE)
summary(tripsNo_sub_ex.lm)

texreg(tripsNo_sub_ex.lm)

saveRDS(tripsNo_sub_ex.lm,"./ANALYSIS/Tables/tripsNo_sub_ex.lm.Rds")



#datset: mergedDataImputeInterpolation

mealsNo_sub_IM.lm <- lm_robust(mealsNo ~ realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(mealsNo_sub_IM.lm)

texreg(mealsNo_sub_IM.lm)

saveRDS(mealsNo_sub_IM.lm,"./ANALYSIS/Tables/mealsNo_sub_IM.lm.Rds")

tripsNo_sub_IM.lm <- lm_robust(tripsNo ~ realTripsSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(tripsNo_sub_IM.lm)

texreg(mealsNo_sub_IM.lm)

saveRDS(tripsNo_sub_IM.lm,"./ANALYSIS/Tables/tripsNo_sub_IM.lm.Rds")
 



##healthinfluence 

#dataset: mergedData


lessIll_DGE.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedData)
summary(lessIll_DGE.lm)

saveRDS(lessIll_DGE.lm,"./ANALYSIS/Tables/lessIll_DGE.lm.Rds")


lessIll_DGE_scaled.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNoScaled, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(lessIll_DGE_scaled.lm) #standardized

texreg(lessIll_DGE_scaled.lm)

saveRDS(lessIll_DGE_scaled.lm,"./ANALYSIS/Tables/lessIll_DGE_scaled.lm.Rds")

Expand_LessIll.lm = lm(lessIll_weighted ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedData)
summary(Expand_LessIll.lm)

saveRDS(Expand_LessIll.lm,"./ANALYSIS/Tables/Expand_LessIll.lm.Rds")

Expand_LessIll_scaled.lm = lm_robust(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidy + state, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(Expand_LessIll_scaled.lm) #standardized

texreg(Expand_LessIll_scaled.lm)

saveRDS(Expand_LessIll_scaled.lm,"./ANALYSIS/Tables/Expand_LessIll_scaled.Rds")

dietaryKnowledge_DGE.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData)
summary(dietaryKnowledge_DGE.lm)

saveRDS(dietaryKnowledge_DGE.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE.lm.Rds")

dietaryKnowledge_DGE_scaled.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dietaryKnowledge_DGE_scaled.lm) #standardized

texreg(dietaryKnowledge_DGE_scaled.lm)

saveRDS(dietaryKnowledge_DGE_scaled.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_scaled.Rds")

appreciateHealthy_DGE.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedData)
summary(appreciateHealthy_DGE.lm)

saveRDS(appreciateHealthy_DGE.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE.Rds")

appreciateHealthy_DGE_scaled.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNoScaled, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(appreciateHealthy_DGE_scaled.lm) #standardized

texreg(appreciateHealthy_DGE_scaled.lm)

saveRDS(appreciateHealthy_DGE_scaled.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_scaled.Rds")


#dataset: mergedDataImputeInterpolation
 
lessIll_DGE_IM.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
summary(lessIll_DGE_IM.lm)

saveRDS(lessIll_DGE_IM.lm,"./ANALYSIS/Tables/lessIll_DGE_IM.lm.Rds")

lessIll_DGE_IM_scaled.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(lessIll_DGE_IM_scaled.lm) #standardized

texreg(lessIll_DGE_IM_scaled.lm)

saveRDS(lessIll_DGE_IM_scaled.lm,"./ANALYSIS/Tables/lessIll_DGE_IM_scaled.lm.Rds")

Expand_LessIll_IM.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy, data = mergedDataImputeInterpolation)
summary(Expand_LessIll_IM.lm)

saveRDS(Expand_LessIll_IM.lm,"./ANALYSIS/Tables/Expand_LessIll_IM.lm.Rds")
# 
Expand_LessIll_IM_scaled.lm = lm_robust(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(Expand_LessIll_IM_scaled.lm) #standardized

texreg(Expand_LessIll_IM_scaled.lm)

saveRDS(Expand_LessIll_IM_scaled.lm,"./ANALYSIS/Tables/Expand_LessIll_IM_scaled.lm.Rds")

dietaryKnowledge_DGE_IM.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
summary(dietaryKnowledge_DGE_IM.lm)

saveRDS(dietaryKnowledge_DGE_IM.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_IM.lm.Rds")

dietaryKnowledge_DGE_IM_scaled.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dietaryKnowledge_DGE_IM_scaled.lm) #standardized

texreg(dietaryKnowledge_DGE_IM_scaled.lm)

saveRDS(dietaryKnowledge_DGE_IM_scaled.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_IM_scaled.lm.Rds")

appreciateHealthy_DGE_IM.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
summary(appreciateHealthy_DGE_IM.lm)

saveRDS(appreciateHealthy_DGE_IM.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_IM.lm.Rds")

appreciateHealthy_DGE_IM_scaled.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(appreciateHealthy_DGE_IM_scaled.lm) #standardized

texreg(appreciateHealthy_DGE_IM_scaled.lm)

saveRDS(appreciateHealthy_DGE_IM_scaled.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_IM_scaled.lm.Rds")


###chance equality
##dataset: mergedData 

#lunch

selfworth.lm <- lm_robust(selfworth_weighted ~ realSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworth.lm)

texreg(selfworth.lm)

saveRDS(selfworth.lm,"./ANALYSIS/Tables/selfworth.lm.Rds")

selfworth_scaled.lm <- lm_robust(selfworth_scaled ~ realSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworth_scaled.lm) #standardized

texreg(selfworth_scaled.lm)

saveRDS(selfworth_scaled.lm,"./ANALYSIS/Tables/selfworth_scaled.Rds")

dayToDaySkills.lm <- lm_robust(dayToDaySkills_weighted ~ realSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkills.lm)

texreg(dayToDaySkills.lm)

saveRDS(dayToDaySkills.lm,"./ANALYSIS/Tables/dayToDaySkills.lm.Rds")

dayToDaySkills_scaled.lm <- lm_robust(dayToDaySkills_scaled ~ realSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkills_scaled.lm) #standardized

texreg(dayToDaySkills_scaled.lm)

saveRDS(dayToDaySkills_scaled.lm,"./ANALYSIS/Tables/dayToDaySkills_scaled.lm.Rds")
 
#trips
selfworthTrips.lm <- lm_robust(tripsSelfworth_weighted ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworthTrips.lm)

texreg(selfworthTrips.lm)

saveRDS(selfworthTrips.lm,"./ANALYSIS/Tables/selfworthTrips.lm.Rds")

selfworthTrips_scaled.lm <- lm_robust(tripsSelfworth_scaled ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworthTrips_scaled.lm) #standardized

texreg(selfworthTrips_scaled.lm)

saveRDS(selfworthTrips_scaled.lm,"./ANALYSIS/Tables/selfworthTrips_scaled.Rds")

dayToDaySkillsTrips.lm <- lm_robust(tripsDayToDaySkills_weighted ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkillsTrips.lm)

texreg(dayToDaySkillsTrips.lm)

saveRDS(dayToDaySkillsTrips.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips.lm.Rds")

dayToDaySkillsTrips_scaled.lm <- lm_robust(tripsDayToDaySkills_scaled ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkillsTrips_scaled.lm) #standardized

texreg(dayToDaySkillsTrips_scaled.lm)

saveRDS(dayToDaySkillsTrips_scaled.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_scaled.lm.Rds")


#dataset: mergedDataImputeInterpolation
#lunch
selfworth_IM.lm <- lm_robust(selfworth_weighted ~ realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworth_IM.lm)

texreg(selfworth_IM.lm)

saveRDS(selfworth_IM.lm,"./ANALYSIS/Tables/selfworth_IM.lm.Rds")

selfworth_IM_scaled.lm <- lm_robust(selfworth_scaled ~ realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworth_IM_scaled.lm) #standardized

texreg(selfworth_IM_scaled.lm)

saveRDS(selfworth_IM_scaled.lm,"./ANALYSIS/Tables/selfworth_IM_scaled.lm.Rds")
# 
dayToDaySkills_IM.lm <- lm_robust(dayToDaySkills_weighted ~ realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkills_IM.lm)

texreg(dayToDaySkills_IM.lm)

saveRDS(dayToDaySkills_IM.lm,"./ANALYSIS/Tables/dayToDaySkills_IM.lm.Rds")
# 
dayToDaySkills_IM_scaled.lm <- lm_robust(dayToDaySkills_scaled ~ realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkills_IM_scaled.lm) #standardized

texreg(dayToDaySkills_IM_scaled.lm)

saveRDS(dayToDaySkills_IM_scaled.lm,"./ANALYSIS/Tables/dayToDaySkills_IM_scaled.lm.Rds")
# 
#trips 
selfworthTrips_IM.lm <- lm_robust(tripsSelfworth_weighted ~ realTripsSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworthTrips_IM.lm)

texreg(selfworthTrips_IM.lm)

saveRDS(selfworthTrips_IM.lm,"./ANALYSIS/Tables/selfworthTrips_IM.lm.Rds")

selfworthTrips_IM_scaled.lm <- lm_robust(tripsSelfworth_scaled ~ realTripsSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworthTrips_IM_scaled.lm) #standardized

texreg(selfworth_IM_scaled.lm)

saveRDS(selfworthTrips_IM_scaled.lm,"./ANALYSIS/Tables/selfworthTrips_IM_scaled.lm.Rds")
# 
dayToDaySkillsTrips_IM.lm <- lm_robust(tripsDayToDaySkills_weighted ~ realTripsSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkillsTrips_IM.lm)

texreg(dayToDaySkillsTrips_IM.lm)

saveRDS(dayToDaySkillsTrips_IM.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_IM.lm.Rds")
# 
dayToDaySkillsTrips_IM_scaled.lm <- lm_robust(tripsDayToDaySkills_scaled ~ realTripsSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkillsTrips_IM_scaled.lm) #standardized

texreg(dayToDaySkillsTrips_IM_scaled.lm)

saveRDS(dayToDaySkillsTrips_IM_scaled.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_IM_scaled.lm.Rds")
# 

