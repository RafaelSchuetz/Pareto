
library(VGAM)
library(ordinal)
library(MASS)

###inspectunfluenceofDGEcriterium
##proportionalOddsModel: Cumulative Link 

DGE_lessIll.vglm.propOdds = vglm(lessIllOrdinal ~  DGECriteriaNo, na.action = na.fail, data = mergedData, family = propodds)
summary(DGE_lessIll.vglm.propOdds)

DGE_lessIll.vglm.propOdds = vglm(lessIllOrdinal ~  DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_lessIll.vglm.propOdds)

DGE_lessIll.cumulative.parallel = vglm(lessIllOrdinal ~  DGECriteriaNo, data = mergedData, family = cumulative(parallel = TRUE))
summary(DGE_lessIll.cumulative.parallel)

DGE_lessIll.vglm.cumulative.notParallel = vglm(lessIllOrdinal ~  DGECriteriaNo, data = mergedData, family = cumulative(parallel = FALSE))
summary(DGE_lessIll.vglm.cumulative.notParallel)

DGE_dietaryKnowledge.vglm = vglm(dietaryKnowledgeOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_dietaryKnowledge.vglm)

DGE_dietaryKnowledge.cumulative.parallel = vglm(dietaryKnowledgeOrdinal ~ DGECriteriaNo, data = mergedData, family = cumulative(parallel = TRUE))
summary(DGE_dietaryKnowledge.cumulative.parallel)

DGE_seasonalFoodstuff.vglm = vglm(seasonalFoodstuffOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_seasonalFoodstuff.vglm)

DGE_organicFoodstuff.vglm = vglm(organicFoodstuffOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_organicFoodstuff.vglm)

DGE_appreciateHealthy.cumulative.parallel = vglm(appreciateHealthyOrdinal ~ DGECriteriaNo, data = mergedData, family = cumulative(parallel = TRUE))
summary(DGE_appreciateHealthy.cumulative.parallel)

DGE_tasksLunch.vglm = vglm(tasksLunchOrdinal ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_tasksLunch.vglm)

## cumulative odds model


#expandRegressionLessIll

DGE_lessIll.vglm = vglm(lessIllOrdinal ~  DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_lessIll.vglm)

LessIll.vglm = vglm(lessIllOrdinal ~  DGECriteriaNo + regionalProductsOrdinal + yearsSupportSince + realSubsidyDifferenceLunch + state, data = mergedData, family = propodds)
summary(LessIll.vglm)

Expand_LessIll.vglm = vglm(lessIllOrdinal ~ DGECriteriaNo + regionalProductsOrdinal + yearsSupportSince + realSubsidy + state, data = mergedData, family = propodds)
summary(Expand_LessIll.vglm)

Expand_LessIll.cumulative.parallel = vglm(lessIllOrdinal ~ DGECriteriaNo + regionalProductsOrdinal + yearsSupportSince + realSubsidy + state, data = mergedData, family = cumulative(parallel = TRUE))
summary(Expand_LessIll.cumulative.parallel)



LessIll_SubsidyDifference.vglm = vglm(lessIllOrdinal ~ realSubsidyDifferenceLunch, data = mergedData, family = propodds)
summary(LessIll_SubsidyDifference.vglm) 

LessIll_Subsidy.vglm = vglm(lessIllOrdinal ~ realSubsidy, data = mergedData, family = propodds)
summary(LessIll_Subsidy.vglm)

LessIll_Support.vglm = vglm(lessIllOrdinal ~ yearsSupportSince, data = mergedData, family = propodds)
summary(LessIll_Support.vglm)


##residualplots??

#approximatingChanceEquality #dayToDaySkills

dayToDaySkills_subsidy.vglm = vglm(dayToDaySkillsOrdinal ~ realSubsidy, data = mergedData, family = propodds)
summary(dayToDaySkills_subsidy.vglm)

dayToDaySkills_subsidy.cumulative.parallel = vglm(dayToDaySkillsOrdinal ~ realSubsidy, data = mergedData, family = cumulative(parallel = TRUE))
summary(dayToDaySkills_subsidy.cumulative.parallel)

dayToDaySkills_Support.vglm = vglm(dayToDaySkillsOrdinal ~ yearsSupportSince, data = mergedData, family = propodds)
summary(dayToDaySkills_Support.vglm)

dayToDaySkills_monthlyCooks.vglm = vglm(dayToDaySkillsOrdinal ~ monthlyCooksOrdinal, data = mergedData, family = propodds)
summary(dayToDaySkills_monthlyCooks.vglm)

dayToDaySkills.vglm = vglm(dayToDaySkillsOrdinal ~ yearsSupportSince + realSubsidyDifferenceLunch + realSubsidy + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state , data = mergedData, family = propodds)
summary(dayToDaySkills.vglm)

dayToDaySkills_shoppers.vglm = vglm(dayToDaySkillsOrdinal ~ shoppersOrdinal, data = mergedData, family = propodds)
summary(dayToDaySkills_shoppers.vglm)

#selfworth

selfworth_subsidy.vglm = vglm(selfworthOrdinal ~ realSubsidy, data = mergedData, family = propodds)
summary(selfworth_subsidy.vglm)

selfworth_subsidy.cumulative.parallel = vglm(selfworthOrdinal ~ realSubsidy, data = mergedData, family = cumulative(parallel = TRUE))
summary(selfworth_subsidy.cumulative.parallel)

selfworth_support.vglm = vglm(selfworthOrdinal ~ yearsSupportSince, data = mergedData, family = propodds)
summary(selfworth_support.vglm)

selfworth_monthlyCooks.vglm = vglm(selfworthOrdinal ~ monthlyCooksOrdinal, data = mergedData, family = propodds)
summary(selfworth_monthlyCooks.vglm)

selfworth_shoppers.vglm = vglm(selfworthOrdinal ~ shoppersOrdinal, data = mergedData, family = propodds)
summary(selfworth_shoppers.vglm)

selfworth_expand.vglm = vglm(selfworthOrdinal ~ yearsSupportSince + realSubsidyDifferenceLunch + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
summary(selfworth_expand.vglm)

selfworth_Expand.vglm = vglm(selfworthOrdinal ~ yearsSupportSince + realSubsidy + realSubsidyDifferenceLunch + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
summary(selfworth_Expand.vglm)

ExpandSelfworth.vglm = vglm(selfworthOrdinal ~ participateMoreOrdinal + enoughFoodOrdinal + yearsSupportSince + realSubsidy + weeklyCooksOrdinal, data = mergedData, family = propodds)
summary(ExpandSelfworth.vglm)


#other

influenceHome.vglm = vglm(influenceHomeOrdinal ~  yearsSupportSince + realSubsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
summary(influenceHome.vglm)

moreIndependent.vglm = vglm(moreIndependentOrdinal ~  yearsSupportSince + realSubsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunchOrdinal + weeklyCooksOrdinal + snacksNo + shoppersOrdinal + state, data = mergedData, family = propodds)
summary(moreIndependent.vglm)


#metrischeregression 

mealsNo_sub.lm <- lm(mealsNo ~ realSubsidy, data = mergedData)
summary(mealsNo_sub.lm)

tripsNo_sub.lm <- lm(tripsNo ~ realTripsSubsidy, data = mergedData)
summary(tripsNo_sub.lm)



