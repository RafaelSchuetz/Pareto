#selfworth and daytodayskills 

#selfworth meals 

factorselfworthmeals <- lm_robust(selfworth_scaled ~ realSubsidyPerBeneficiary + ML1 + ML2 + ML3, selfworth_RealSubsidyPerBeneficiary_Factors)%>%
  extract.lm_robust(include.ci = FALSE)

saveRDS(factorselfworthmeals,"./ANALYSIS/Tables/factorselfworthmeals.Rds")

#dayToDaySkills meals 

factordaytodayskillsmeals <- lm_robust(dayToDaySkills_scaled ~ realSubsidyPerBeneficiary + ML1 + ML2 + ML3, dayToDaySkills_RealSubsidyPerBeneficiary_Factors)%>%
  extract.lm_robust(include.ci = FALSE)

saveRDS(factordaytodayskillsmeals,"./ANALYSIS/Tables/factordaytodayskillsmeals.Rds")

#dayToDaySkills trips

factordaytodayskillstrips <- lm_robust(tripsDayToDaySkills_scaled ~ realTripsSubsidyPerBeneficiary + ML1 + ML2 + ML3 + ML4, tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary_Factors)%>%
  extract.lm_robust(include.ci = FALSE)

saveRDS(factordaytodayskillstrips,"./ANALYSIS/Tables/factordaytodayskillstrips.Rds")

#health outcome 
#lessIll
lessIllfactor <- lm_robust(lessIll_scaled ~ DGECriteriaNoScaled + ML1 + ML2, lessIll_DGECriteriaNo_Factors)%>%
  extract.lm_robust(include.ci = FALSE)

saveRDS(lessIllfactor,"./ANALYSIS/Tables/lessIllfactor.Rds")

#dietaryKnowledge 

dietaryknowledgefactor <- lm_robust(dietaryKnowledge_scaled ~ DGECriteriaNoScaled + ML1 + ML2 + ML3, dietaryKnowledge_DGECriteriaNo_Factors)%>%
  extract.lm_robust(include.ci = FALSE)

saveRDS(dietaryknowledgefactor,"./ANALYSIS/Tables/dietaryknowledgefactor.Rds")

#appreciateHealthy

appreciatehealthyfactor <- lm_robust(appreciateHealthy_scaled ~ DGECriteriaNoScaled + ML1 + ML2 + ML3, appreciateHealthy_DGECriteriaNo_Factors)%>%
  extract.lm_robust(include.ci = FALSE)

saveRDS(appreciatehealthyfactor,"./ANALYSIS/Tables/appreciatehealthyfactor.Rds")
