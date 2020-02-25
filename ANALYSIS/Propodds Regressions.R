
library(VGAM)
library(ordinal)
library(MASS)
library(texreg)



###inspectunfluenceofDGEcriterium
##proportionalOddsModel: Cumulative Link #cumulative odds model

#dataset: mergedData

#DGE_lessIll.vglm.propOdds = vglm(lessIllOrdinal ~  DGECriteriaNo, na.action = na.fail, data = mergedData, family = propodds)
#summary(DGE_lessIll.vglm.propOdds)

#DGE_lessIll.vglm.propOdds = vglm(lessIllOrdinal ~  DGECriteriaNo, data = mergedData, family = propodds)
#summary(DGE_lessIll.vglm.propOdds)

DGE_lessIll.cumulative.parallel = vglm(lessIll_ordered ~  DGECriteriaNo, data = mergedData, family = cumulative(parallel = TRUE))
summary(DGE_lessIll.cumulative.parallel)

saveRDS(DGE_lessIll.cumulative.parallel,"./ANALYSIS/Tables/DGE_lessIll.cumulative.parallel.Rds")
# 

#DGE_lessIll.vglm.cumulative.notParallel = vglm(lessIllOrdinal ~  DGECriteriaNo, data = mergedData, family = cumulative(parallel = FALSE))
#summary(DGE_lessIll.vglm.cumulative.notParallel)

#DGE_dietaryKnowledge.vglm = vglm(dietaryKnowledgeOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
#summary(DGE_dietaryKnowledge.vglm)

DGE_dietaryKnowledge.cumulative.parallel = vglm(dietaryKnowledge_ordered ~ DGECriteriaNo, data = mergedData, family = cumulative(parallel = TRUE))
summary(DGE_dietaryKnowledge.cumulative.parallel)

saveRDS(DGE_dietaryKnowledge.cumulative.parallel,"./ANALYSIS/Tables/DGE_dietaryKnowledge.cumulative.parallel.Rds")

#DGE_seasonalFoodstuff.vglm = vglm(seasonalFoodstuffOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
#summary(DGE_seasonalFoodstuff.vglm)

#DGE_organicFoodstuff.vglm = vglm(organicFoodstuffOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
#summary(DGE_organicFoodstuff.vglm)

DGE_appreciateHealthy.cumulative.parallel = vglm(appreciateHealthy_ordered ~ DGECriteriaNo, data = mergedData, family = cumulative(parallel = TRUE))
summary(DGE_appreciateHealthy.cumulative.parallel)

saveRDS(DGE_appreciateHealthy.cumulative.parallel,"./ANALYSIS/Tables/DGE_appreciateHealthy.cumulative.parallel.Rds")

#DGE_tasksLunch.vglm = vglm(tasksLunchOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
#summary(DGE_tasksLunch.vglm)

#dataset: mergedDataImputeInterpolation

DGE_lessIll_IM.cumulative.parallel = vglm(lessIll_ordered ~  DGECriteriaNo, data = mergedDataImputeInterpolation, family = cumulative(parallel = TRUE))
summary(DGE_lessIll_IM.cumulative.parallel)

saveRDS(DGE_lessIll_IM.cumulative.parallel,"./ANALYSIS/Tables/DGE_lessIll_IM.cumulative.parallel.Rds")

DGE_lessIll_IM.vglm.cumulative.notParallel = vglm(lessIll_ordered ~  DGECriteriaNo, data = mergedDataImputeInterpolation, family = cumulative(parallel = FALSE))
summary(DGE_lessIll_IM.vglm.cumulative.notParallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")

DGE_dietaryKnowledge_IM.cumulative.parallel = vglm(dietaryKnowledge_ordered ~ DGECriteriaNo, data = mergedDataImputeInterpolation, family = cumulative(parallel = TRUE))
summary(DGE_dietaryKnowledge_IM.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")

DGE_appreciateHealthy_IM.cumulative.parallel = vglm(appreciateHealthy_ordered ~ DGECriteriaNo, data = mergedDataImputeInterpolation, family = cumulative(parallel = TRUE))
summary(DGE_appreciateHealthy_IM.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")



##expandRegressionLessIll
#dataset: mergedData

#DGE_lessIll.vglm = vglm(lessIllOrdinal ~  DGECriteriaNo, data = mergedData, family = propodds)
#summary(DGE_lessIll.vglm)

#LessIll.vglm = vglm(lessIllOrdinal ~  DGECriteriaNo + regionalProductsOrdinal + yearsSupportSince + realSubsidyDifferenceLunch + state, data = mergedData, family = propodds)
#summary(LessIll.vglm)

#Expand_LessIll.vglm = vglm(lessIllOrdinal ~ DGECriteriaNo + regionalProductsOrdinal + yearsSupportSince + realSubsidy + state, data = mergedData, family = propodds)
#summary(Expand_LessIll.vglm)

Expand_LessIll.cumulative.parallel = vglm(lessIll_ordered ~ DGECriteriaNo + regionalProducts_ordered + yearsSupportSince + realSubsidy + state, data = mergedData, family = cumulative(parallel = TRUE))
summary(Expand_LessIll.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")

#LessIll_SubsidyDifference.vglm = vglm(lessIllOrdinal ~ realSubsidyDifferenceLunch, data = mergedData, family = propodds)
#summary(LessIll_SubsidyDifference.vglm) 

#LessIll_Subsidy.vglm = vglm(lessIllOrdinal ~ realSubsidy, data = mergedData, family = propodds)
#summary(LessIll_Subsidy.vglm)

#LessIll_Support.vglm = vglm(lessIllOrdinal ~ yearsSupportSince, data = mergedData, family = propodds)
#summary(LessIll_Support.vglm)

#dataset: mergedDataImputeInterpolation

Expand_LessIll_IM.cumulative.parallel = vglm(lessIll_ordered ~ DGECriteriaNo + regionalProducts_ordered + yearsSupportSince + realSubsidy + state, data = mergedDataImputeInterpolation, family = cumulative(parallel = TRUE))
summary(Expand_LessIll_IM.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")



###approximatingChanceEquality 
##dayToDaySkills
#data: mergedData

#dayToDaySkills_subsidy.vglm = vglm(dayToDaySkillsOrdinal ~ realSubsidy, data = mergedData, family = propodds)
#summary(dayToDaySkills_subsidy.vglm)

dayToDaySkills_subsidy.cumulative.parallel = vglm(dayToDaySkills_ordered ~ realSubsidy, data = mergedData, family = cumulative(parallel = TRUE))
summary(dayToDaySkills_subsidy.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")

#dayToDaySkills_Support.vglm = vglm(dayToDaySkillsOrdinal ~ yearsSupportSince, data = mergedData, family = propodds)
#summary(dayToDaySkills_Support.vglm)

#dayToDaySkills_monthlyCooks.vglm = vglm(dayToDaySkillsOrdinal ~ monthlyCooksOrdinal, data = mergedData, family = propodds)
#summary(dayToDaySkills_monthlyCooks.vglm)

#dayToDaySkills.vglm = vglm(dayToDaySkillsOrdinal ~ yearsSupportSince + realSubsidyDifferenceLunch + realSubsidy + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state , data = mergedData, family = propodds)
#summary(dayToDaySkills.vglm)

#dayToDaySkills_shoppers.vglm = vglm(dayToDaySkillsOrdinal ~ shoppersOrdinal, data = mergedData, family = propodds)
#summary(dayToDaySkills_shoppers.vglm)

#dataset: mergedDataImputeInterpolation

dayToDaySkills_subsidy_IM.cumulative.parallel = vglm(dayToDaySkills_ordered ~ realSubsidy, data = mergedDataImputeInterpolation, family = cumulative(parallel = TRUE))
summary(dayToDaySkills_subsidy_IM.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")

##selfworth
#dataset: mergedData

#selfworth_subsidy.vglm = vglm(selfworthOrdinal ~ realSubsidy, data = mergedData, family = propodds)
#summary(selfworth_subsidy.vglm)

selfworth_subsidy.cumulative.parallel = vglm(selfworth_ordered ~ realSubsidy, data = mergedData, family = cumulative(parallel = TRUE))
summary(selfworth_subsidy.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")

#selfworth_support.vglm = vglm(selfworthOrdinal ~ yearsSupportSince, data = mergedData, family = propodds)
#summary(selfworth_support.vglm)

#selfworth_monthlyCooks.vglm = vglm(selfworthOrdinal ~ monthlyCooksOrdinal, data = mergedData, family = propodds)
#summary(selfworth_monthlyCooks.vglm)

#selfworth_shoppers.vglm = vglm(selfworthOrdinal ~ shoppersOrdinal, data = mergedData, family = propodds)
#summary(selfworth_shoppers.vglm)

#selfworth_expand.vglm = vglm(selfworthOrdinal ~ yearsSupportSince + realSubsidyDifferenceLunch + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
#summary(selfworth_expand.vglm)

#selfworth_Expand.vglm = vglm(selfworthOrdinal ~ yearsSupportSince + realSubsidy + realSubsidyDifferenceLunch + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
#summary(selfworth_Expand.vglm)

#ExpandSelfworth.vglm = vglm(selfworthOrdinal ~ participateMoreOrdinal + enoughFoodOrdinal + yearsSupportSince + realSubsidy + weeklyCooksOrdinal, data = mergedData, family = propodds)
#summary(ExpandSelfworth.vglm)

#dataset: mergedDataImputeInterpolation

selfworth_subsidy_IM.cumulative.parallel = vglm(selfworth_ordered ~ realSubsidy, data = mergedDataImputeInterpolation, family = cumulative(parallel = TRUE))
summary(selfworth_subsidy_IM.cumulative.parallel)

saveRDS(,"./ANALYSIS/Tables/.Rds")

#other

#influenceHome.vglm = vglm(influenceHomeOrdinal ~  yearsSupportSince + realSubsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
#summary(influenceHome.vglm)

#moreIndependent.vglm = vglm(moreIndependentOrdinal ~  yearsSupportSince + realSubsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
#summary(moreIndependent.vglm)


