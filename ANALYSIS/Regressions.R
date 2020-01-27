
library(VGAM)
library(ordinal)
library(MASS)

###inspectunfluenceofDGEcriterium
##proportionalOddsModel 

DGE_lessIll.vglm = vglm(lessIll ~  DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_lessIll.vglm)

DGE_dietaryKnowledge.vglm = vglm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_dietaryKnowledge.vglm)

DGE_seasonalFoodstuff.vglm = vglm(seasonalFoodstuff ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_seasonalFoodstuff.vglm)

DGE_organicFoodstuff.vglm = vglm(organicFoodstuff ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_organicFoodstuff.vglm)

DGE_appreciateHealthy.vglm = vglm(appreciateHealthy ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_appreciateHealthy.vglm)

DGE_tasksLunch.vglm = vglm(tasksLunch ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_tasksLunch.vglm)

#expandRegressionLessIll

DGE_lessIll.vglm = vglm(lessIll ~  DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_lessIll.vglm)

LessIll.vglm = vglm(lessIll ~  DGECriteriaNo + regionalProducts + yearsSupportSince + subsidyDifferenceLunch + state, data = mergedData, family = propodds)
summary(LessIll.vglm)

Expand_LessIll.vglm = vglm(lessIll ~ DGECriteriaNo + regionalProducts + yearsSupportSince + subsidy + state, data = mergedData, family = propodds)
summary(Expand_LessIll.vglm)




LessIll_SubsidyDifference.vglm = vglm(lessIll ~ subsidyDifferenceLunch, data = mergedData, family = propodds)
summary(LessIll_SubsidyDifference.vglm) 

LessIll_Subsidy.vglm = vglm(lessIll ~ subsidy, data = mergedData, family = propodds)
summary(LessIll_Subsidy.vglm)

LessIll_Support.vglm = vglm(lessIll ~ yearsSupportSince, data = mergedData, family = propodds)
summary(LessIll_Support.vglm)


##residualplots??

#approximatingChanceEquality #dayToDaySkills

dayToDaySkills_subsidy.vglm = vglm(dayToDaySkills ~ subsidy, data = mergedData, family = propodds)
summary(dayToDaySkills_subsidy.vglm)

dayToDaySkills_Support.vglm = vglm(dayToDaySkills ~ yearsSupportSince, data = mergedData, family = propodds)
summary(dayToDaySkills_Support.vglm)

dayToDaySkills_monthlyCooks.vglm = vglm(dayToDaySkills ~ monthlyCooks, data = mergedData, family = propodds)
summary(dayToDaySkills_monthlyCooks.vglm)

dayToDaySkills.vglm = vglm(dayToDaySkills ~ yearsSupportSince + subsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state , data = mergedData, family = propodds)
summary(dayToDaySkills.vglm)

dayToDaySkills_shoppers.vglm = vglm(dayToDaySkills ~ shoppers, data = mergedData, family = propodds)
summary(dayToDaySkills_shoppers.vglm)

#selfworth

selfworth_subsidy.vglm = vglm(selfworth ~ subsidy, data = mergedData, family = propodds)
summary(selfworth_subsidy.vglm)

selfworth_support.vglm = vglm(selfworth ~ yearsSupportSince, data = mergedData, family = propodds)
summary(selfworth_support.vglm)

selfworth_monthlyCooks.vglm = vglm(selfworth ~ monthlyCooks, data = mergedData, family = propodds)
summary(selfworth_monthlyCooks.vglm)

selfworth_shoppers.vglm = vglm(selfworth ~ shoppers, data = mergedData, family = propodds)
summary(selfworth_shoppers.vglm)

selfworth_expand.vglm = vglm(selfworth ~ yearsSupportSince + subsidyDifferenceLunch + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state, data = mergedData, family = propodds)
summary(selfworth_expand.vglm)

selfworth_Expand.vglm = vglm(selfworth ~ yearsSupportSince + subsidy + subsidyDifferenceLunch + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state, data = mergedData, family = propodds)
summary(selfworth_Expand.vglm)

ExpandSelfworth.vglm = vglm(selfworth ~ participateMore + enoughFood + yearsSupportSince + subsidy + weeklyCooks, data = mergedData, family = propodds)
summary(ExpandSelfworth.vglm)


#other

influenceHome.vglm = vglm(influenceHome ~  yearsSupportSince + subsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state, data = mergedData, family = propodds)
summary(influenceHome.vglm)

moreIndependent.vglm = vglm(moreIndependent ~  yearsSupportSince + subsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state, data = mergedData, family = propodds)
summary(moreIndependent.vglm)


#metrischeregression 

mealsNo_sub.lm <- lm(mealsNo ~ subsidy, data = mergedData)
summary(mealsNo_sub.lm)

tripsNo_sub.lm <- lm(tripsNo ~ tripsSubsidy, data = mergedData)
summary(tripsNo_sub.lm)



