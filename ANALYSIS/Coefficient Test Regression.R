#coeftest regressions for robust standard errors

##mealsno and tripsno

#mergedData
mealsNo_sub.lm <- coeftest(mealsNo_sub.lm, vcov. = vcovHC(mealsNo_sub.lm, type = 'HC1'))
summary(mealsNo_sub.lm)

tripsNo_sub.lm <- coeftest(tripsNo_sub.lm, vcov. = vcovHC(tripsNo_sub.lm, type = 'HC1'))
summary(tripsNo_sub.lm)

#excludedOutliers
mealsNo_sub_ex.lm <- coeftest(mealsNo_sub_ex.lm, vcov. = vcovHC(mealsNo_sub_ex.lm, type = 'HC1'))
summary(mealsNo_sub_ex.lm)

tripsNo_sub_ex.lm <- coeftest(tripsNo_sub_ex.lm, vcov. = vcovHC(tripsNo_sub_ex.lm, type = 'HC1'))
summary(tripsNo_sub_ex.lm)

#impute

mealsNo_sub_IM.lm <- coeftest(mealsNo_sub_IM.lm, vcov. = vcovHC(mealsNo_sub_IM.lm, type = 'HC1'))
summary(mealsNo_sub_IM.lm)

tripsNo_sub_IM.lm <- coeftest(tripsNo_sub_IM.lm, vcov. = vcovHC(tripsNo_sub_IM.lm, type = 'HC1'))
summary(tripsNo_sub_IM.lm)

#health influence

#mergedData
lessIll_DGE.lm <- coeftest(lessIll_DGE.lm, vcov. = vcovHC(lessIll_DGE.lm, type = 'HC1'))
summary(lessIll_DGE.lm)

lessIll_DGE_Scaled.lm <- coeftest(lessIll_DGE_scaled.lm, vcov. = vcovHC(lessIll_DGE_scaled.lm, type = 'HC1'))
summary(lessIll_DGE_scaled.lm)

Expand_LessIll.lm <- coeftest(Expand_LessIll.lm, vcov. = vcovHC(Expand_LessIll.lm, type = 'HC1'))
summary(Expand_LessIll.lm)

Expand_LessIll_scaled.lm <- coeftest(Expand_LessIll_scaled.lm, vcov. = vcovHC(Expand_LessIll_scaled.lm, type = 'HC1'))
summary(Expand_LessIll_scaled.lm)

dietaryKnowledge_DGE.lm <- coeftest(dietaryKnowledge_DGE.lm, vcov. = vcovHC(dietaryKnowledge_DGE.lm, type = 'HC1'))
summary(dietaryKnowledge_DGE.lm)

dietaryKnowledge_DGE_scaled.lm <- coeftest(dietaryKnowledge_DGE_scaled.lm, vcov. = vcovHC(dietaryKnowledge_DGE_scaled.lm, type = 'HC1'))
summary(dietaryKnowledge_DGE_scaled.lm)

appreciateHealthy_DGE.lm <- coeftest(appreciateHealthy_DGE.lm, vcov. = vcovHC(appreciateHealthy_DGE.lm, type = 'HC1'))
summary(appreciateHealthy_DGE.lm)

appreciateHealthy_DGE_scaled.lm <- coeftest(appreciateHealthy_DGE_scaled.lm, vcov. = vcovHC(appreciateHealthy_DGE_scaled.lm, type = 'HC1'))
summary(appreciateHealthy_DGE_scaled.lm)

#impute 

lessIll_DGE_IM.lm <- coeftest(lessIll_DGE_IM.lm, vcov. = vcovHC(lessIll_DGE_IM.lm, type = 'HC1'))
summary(lessIll_DGE_IM.lm)

lessIll_DGE_IM_scaled.lm <- coeftest(lessIll_DGE_IM_scaled.lm, vcov. = vcovHC(lessIll_DGE_IM_scaled.lm, type = 'HC1'))
summary(lessIll_DGE_IM_scaled.lm)

Expand_LessIll_IM.lm <- coeftest(Expand_LessIll_IM.lm, vcov. = vcovHC(Expand_LessIll_IM.lm, type = 'HC1'))
summary(Expand_LessIll_IM.lm)

Expand_LessIll_IM_scaled.lm <- coeftest(Expand_LessIll_IM_scaled.lm, vcov. = vcovHC(Expand_LessIll_IM_scaled.lm, type = 'HC1'))
summary(Expand_LessIll_IM_scaled.lm)

dietaryKnowledge_DGE_IM.lm <- coeftest(dietaryKnowledge_DGE_IM.lm, vcov. = vcovHC(dietaryKnowledge_DGE_IM.lm, type = 'HC1'))
summary(dietaryKnowledge_DGE_IM.lm)

dietaryKnowledge_DGE_IM_scaled.lm <- coeftest(dietaryKnowledge_DGE_IM_scaled.lm, vcov. = vcovHC(dietaryKnowledge_DGE_IM_scaled.lm, type = 'HC1'))
summary(dietaryKnowledge_DGE_IM_scaled.lm)

appreciateHealthy_DGE_IM.lm <- coeftest(appreciateHealthy_DGE_IM.lm, vcov. = vcovHC(appreciateHealthy_DGE_IM.lm, type = 'HC1'))
summary(appreciateHealthy_DGE_IM.lm)

appreciateHealthy_DGE_IM_scaled.lm <- coeftest(appreciateHealthy_DGE_IM_scaled.lm, vcov. = vcovHC(appreciateHealthy_DGE_IM_scaled.lm, type = 'HC1'))
summary(appreciateHealthy_DGE_IM_scaled.lm)

#equality of opportunities 
#lunch
selfworth.lm <- coeftest(selfworth.lm, vcov. = vcovHC(selfworth.lm, type = 'HC1'))
summary(selfworth.lm)

selfworth_scaled.lm <- coeftest(selfworth_scaled.lm, vcov. = vcovHC(selfworth_scaled.lm, type = 'HC1'))
summary(selfworth_scaled.lm)

dayToDaySkills.lm <- coeftest(dayToDaySkills.lm, vcov. = vcovHC(dayToDaySkills.lm, type = 'HC1'))
summary(dayToDaySkills.lm)

dayToDaySkills_scaled.lm <- coeftest(dayToDaySkills_scaled.lm, vcov. = vcovHC(dayToDaySkills_scaled.lm, type = 'HC1'))
summary(dayToDaySkills_scaled.lm)

#trips

selfworthTrips.lm <- coeftest(selfworthTrips.lm, vcov. = vcovHC(selfworthTrips.lm, type = 'HC1'))
summary(selfworthTrips.lm)

selfworthTrips_scaled.lm <- coeftest(selfworthTrips_scaled.lm, vcov. = vcovHC(selfworthTrips_scaled.lm, type = 'HC1'))
summary(selfworthTrips_scaled.lm)

dayToDaySkillsTrips.lm <- coeftest(dayToDaySkillsTrips.lm, vcov. = vcovHC(dayToDaySkillsTrips.lm, type = 'HC1'))
summary(dayToDaySkillsTrips.lm)

dayToDaySkillsTrips_scaled.lm <- coeftest(dayToDaySkillsTrips_scaled.lm, vcov. = vcovHC(dayToDaySkillsTrips_scaled.lm, type = 'HC1'))
summary(dayToDaySkillsTrips_scaled.lm)

#impute
#lunch

selfworth_IM.lm <- coeftest(selfworth_IM.lm, vcov. = vcovHC(selfworth_IM.lm, type = 'HC1'))
summary(selfworth_IM.lm)

selfworth_IM_scaled.lm <- coeftest(selfworth_IM_scaled.lm, vcov. = vcovHC(selfworth_IM_scaled.lm, type = 'HC1'))
summary(selfworth_IM_scaled.lm) 

dayToDaySkills_IM.lm <- coeftest(dayToDaySkills_IM.lm, vcov. = vcovHC(dayToDaySkills_IM.lm, type = 'HC1'))
summary(dayToDaySkills_IM.lm)

dayToDaySkills_IM_scaled.lm <- coeftest(dayToDaySkills_IM_scaled.lm, vcov. = vcovHC(dayToDaySkills_IM_scaled.lm, type = 'HC1'))
summary(dayToDaySkills_IM_scaled.lm)

#trips

selfworthTrips_IM.lm <- coeftest(selfworthTrips_IM.lm, vcov. = vcovHC(selfworthTrips_IM.lm, type = 'HC1'))
summary(selfworthTrips_IM.lm)

selfworthTrips_IM_scaled.lm <- coeftest(selfworthTrips_IM_scaled.lm, vcov. = vcovHC(selfworthTrips_IM_scaled.lm, type = 'HC1'))
summary(selfworthTrips_IM_scaled.lm) 

dayToDaySkillsTrips_IM.lm <- coeftest(dayToDaySkillsTrips_IM.lm, vcov. = vcovHC(dayToDaySkillsTrips_IM.lm, type = 'HC1'))
summary(dayToDaySkillsTrips_IM.lm)

dayToDaySkillsTrips_IM_scaled.lm <- coeftest(dayToDaySkillsTrips_IM_scaled.lm, vcov. = vcovHC(dayToDaySkillsTrips_IM_scaled.lm, type = 'HC1'))
summary(dayToDaySkillsTrips_IM_scaled.lm)