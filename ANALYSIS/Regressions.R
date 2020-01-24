
library(VGAM)
library(ordinal)

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

#expandRegressionDGElessIll

LessIll.vglm = vglm(lessIll ~  DGECriteriaNo + regionalProducts + yearsSupportSince + subsidyDifferenceLunch + state, data = mergedData, family = propodds)
summary(LessIll.vglm)

##residualplots
##interpretationofpropodds??
##darstellung

#approximatingChanceEquality

selfworth.vglm = vglm(selfworth ~ yearsSupportSince + subsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state , data = mergedData, family = propodds)
summary(selfworth.vglm)

ExpandSelfworth.vglm = vglm(selfworth ~ participateMore + enoughFood + yearsSupportSince + subsidy + subsidyDifferenceLunch + weeklyCooks, data = mergedData, family = propodds)
summary(ExpandSelfworth.vglm)

dayToDaySkills.vglm = vglm(dayToDaySkills ~ yearsSupportSince + subsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state , data = mergedData, family = propodds)
summary(dayToDaySkills.vglm)

influenceHome.vglm = vglm(influenceHome ~  yearsSupportSince + subsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state, data = mergedData, family = propodds)
summary(influenceHome.vglm)

moreIndependent.vglm = vglm(moreIndependent ~  yearsSupportSince + subsidyDifferenceLunch + subsidy + mealsNo + enoughStaffLunch + weeklyCooks + snacksNo + shoppers + state, data = mergedData, family = propodds)
summary(moreIndependent.vglm)






#cumulativelinkmodelsdon'tworkbecausetheyneedafactorasoutcome 


