#health regressions
library(estimatr)
library(texreg)

##healthinfluence 

#dataset: mergedData

# lessIll_DGE.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedData)
# summary(lessIll_DGE.lm)
# 
# saveRDS(lessIll_DGE.lm,"./ANALYSIS/Tables/lessIll_DGE.lm.Rds")

lessIll_DGE_scaled.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNoScaled, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(lessIll_DGE_scaled.lm) #standardized

texreg(lessIll_DGE_scaled.lm)

saveRDS(lessIll_DGE_scaled.lm,"./ANALYSIS/Tables/lessIll_DGE_scaled.lm.Rds")
#

lessIll_DGE_scaled_weighted.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNoScaled, weights = eatersPerMealNo, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(lessIll_DGE_scaled.lm) #standardized

texreg(lessIll_DGE_scaled_weighted.lm)

saveRDS(lessIll_DGE_scaled_weighted.lm,"./ANALYSIS/Tables/lessIll_DGE_scaled_weighted.lm.Rds")
#

# Expand_LessIll.lm = lm(lessIll_weighted ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy + state, data = mergedData)
# summary(Expand_LessIll.lm)
# 
# saveRDS(Expand_LessIll.lm,"./ANALYSIS/Tables/Expand_LessIll.lm.Rds")

Expand_LessIll_scaled.lm = lm_robust(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidyPerBeneficiary + state, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(Expand_LessIll_scaled.lm) #standardized

texreg(Expand_LessIll_scaled.lm)

saveRDS(Expand_LessIll_scaled.lm,"./ANALYSIS/Tables/Expand_LessIll_scaled.Rds")
#

Expand_LessIll_scaled_weighted.lm = lm_robust(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidyPerBeneficiary + state, weight = eatersPerMealNo, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(Expand_LessIll_scaled_weighted.lm) #standardized

texreg(Expand_LessIll_scaled_weighted.lm)

saveRDS(Expand_LessIll_scaled_weighted.lm,"./ANALYSIS/Tables/Expand_LessIll_scaled_weighted.Rds")

#
# dietaryKnowledge_DGE.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData)
# summary(dietaryKnowledge_DGE.lm)
# 
# saveRDS(dietaryKnowledge_DGE.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE.lm.Rds")

dietaryKnowledge_DGE_scaled.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dietaryKnowledge_DGE_scaled.lm) #standardized

texreg(dietaryKnowledge_DGE_scaled.lm)

saveRDS(dietaryKnowledge_DGE_scaled.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_scaled.Rds")
#
dietaryKnowledge_DGE_scaled_weighted.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, weights = eatersPerMealNo, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dietaryKnowledge_DGE_scaled_weighted.lm) #standardized

texreg(dietaryKnowledge_DGE_scaled_weighted.lm)

saveRDS(dietaryKnowledge_DGE_scaled_weighted.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_scaled_weighted.Rds")
#
# appreciateHealthy_DGE.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedData)
# summary(appreciateHealthy_DGE.lm)

appreciateHealthy_DGE_scaled.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNoScaled, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(appreciateHealthy_DGE_scaled.lm) #standardized

texreg(appreciateHealthy_DGE_scaled.lm)

saveRDS(appreciateHealthy_DGE_scaled.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_scaled.Rds")
#
appreciateHealthy_DGE_scaled_weighted.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNoScaled, weights = eatersPerMealNo, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(appreciateHealthy_DGE_scaled.lm) #standardized

texreg(appreciateHealthy_DGE_scaled_weighted.lm)

saveRDS(appreciateHealthy_DGE_scaled_weighted.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_scaled_weighted.Rds")
#



#dataset: mergedDataImputeInterpolation

# lessIll_DGE_IM.lm <- lm(lessIll ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
# summary(lessIll_DGE_IM.lm)
# 
# saveRDS(lessIll_DGE_IM.lm,"./ANALYSIS/Tables/lessIll_DGE_IM.lm.Rds")

lessIll_DGE_IM_scaled.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(lessIll_DGE_IM_scaled.lm) #standardized

texreg(lessIll_DGE_IM_scaled.lm)

saveRDS(lessIll_DGE_IM_scaled.lm,"./ANALYSIS/Tables/lessIll_DGE_IM_scaled.lm.Rds")
#
lessIll_DGE_IM_scaled_weighted.lm <- lm_robust(lessIll_scaled ~ DGECriteriaNoScaled, weights = eatersPerMealNo, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(lessIll_DGE_IM_scaled_weighted.lm) #standardized

texreg(lessIll_DGE_IM_scaled_weighted.lm)

saveRDS(lessIll_DGE_IM_scaled_weighted.lm,"./ANALYSIS/Tables/lessIll_DGE_IM_scaled_weighted.lm.Rds")
#
# Expand_LessIll_IM.lm = lm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + realSubsidy, data = mergedDataImputeInterpolation)
# summary(Expand_LessIll_IM.lm)
# 
# saveRDS(Expand_LessIll_IM.lm,"./ANALYSIS/Tables/Expand_LessIll_IM.lm.Rds")
# 
Expand_LessIll_IM_scaled.lm = lm_robust(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(Expand_LessIll_IM_scaled.lm) #standardized

texreg(Expand_LessIll_IM_scaled.lm)

saveRDS(Expand_LessIll_IM_scaled.lm,"./ANALYSIS/Tables/Expand_LessIll_IM_scaled.lm.Rds")
#

Expand_LessIll_IM_scaled_weighted.lm = lm_robust(lessIll_scaled ~ DGECriteriaNoScaled + regionalProducts_scaled + yearsSupportSince + realSubsidyPerBeneficiary, weights = eatersPerMealNo, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(Expand_LessIll_IM_scaled_weighted.lm) #standardized

texreg(Expand_LessIll_IM_scaled_weighted.lm)

saveRDS(Expand_LessIll_IM_scaled_weighted.lm,"./ANALYSIS/Tables/Expand_LessIll_IM_scaled_weighted.lm.Rds")
#

# dietaryKnowledge_DGE_IM.lm <- lm(dietaryKnowledge ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
# summary(dietaryKnowledge_DGE_IM.lm)
# 
# saveRDS(dietaryKnowledge_DGE_IM.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_IM.lm.Rds")

dietaryKnowledge_DGE_IM_scaled.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dietaryKnowledge_DGE_IM_scaled.lm) #standardized

texreg(dietaryKnowledge_DGE_IM_scaled.lm)

saveRDS(dietaryKnowledge_DGE_IM_scaled.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_IM_scaled.lm.Rds")
#
dietaryKnowledge_DGE_IM_scaled_weighted.lm <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNoScaled, weights = eatersPerMealNo, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dietaryKnowledge_DGE_IM_scaled_weighted.lm) #standardized

texreg(dietaryKnowledge_DGE_IM_scaled_weighted.lm)

saveRDS(dietaryKnowledge_DGE_IM_scaled_weighted.lm,"./ANALYSIS/Tables/dietaryKnowledge_DGE_IM_scaled_weighted.lm.Rds")
#
#
# appreciateHealthy_DGE_IM.lm <- lm(appreciateHealthy ~ DGECriteriaNo, data = mergedDataImputeInterpolation)
# summary(appreciateHealthy_DGE_IM.lm)
# 
# saveRDS(appreciateHealthy_DGE_IM.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_IM.lm.Rds")

appreciateHealthy_DGE_IM_scaled.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNoScaled, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(appreciateHealthy_DGE_IM_scaled.lm) #standardized

texreg(appreciateHealthy_DGE_IM_scaled.lm)

saveRDS(appreciateHealthy_DGE_IM_scaled.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_IM_scaled.lm.Rds")
#
appreciateHealthy_DGE_IM_scaled_weighted.lm <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNoScaled, weights = eatersPerMealNo, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(appreciateHealthy_DGE_IM_scaled_weighted.lm) #standardized

texreg(appreciateHealthy_DGE_IM_scaled_weighted.lm)

saveRDS(appreciateHealthy_DGE_IM_scaled_weighted.lm,"./ANALYSIS/Tables/appreciateHealthy_DGE_IM_scaled_weighted.lm.Rds")
#