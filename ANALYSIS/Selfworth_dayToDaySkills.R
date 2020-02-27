#equality of opportunities regressions

##dataset: mergedData 

#lunch

selfworth.lm <- lm_robust(selfworth_weighted ~ realSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworth.lm)

texreg(selfworth.lm)

saveRDS(selfworth.lm,"./ANALYSIS/Tables/selfworth.lm.Rds")
#

selfworth_scaled.lm <- lm_robust(selfworth_scaled ~ realSubsidyPerBeneficiary, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworth_scaled.lm) #standardized

texreg(selfworth_scaled.lm)

saveRDS(selfworth_scaled.lm,"./ANALYSIS/Tables/selfworth_scaled.Rds")
#

dayToDaySkills.lm <- lm_robust(dayToDaySkills_weighted ~ realSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkills.lm)

texreg(dayToDaySkills.lm)

saveRDS(dayToDaySkills.lm,"./ANALYSIS/Tables/dayToDaySkills.lm.Rds")
#

dayToDaySkills_scaled.lm <- lm_robust(dayToDaySkills_scaled ~ realSubsidyPerBeneficiary, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkills_scaled.lm) #standardized

texreg(dayToDaySkills_scaled.lm)

saveRDS(dayToDaySkills_scaled.lm,"./ANALYSIS/Tables/dayToDaySkills_scaled.lm.Rds")
#

#trips
selfworthTrips.lm <- lm_robust(tripsSelfworth_weighted ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworthTrips.lm)

texreg(selfworthTrips.lm)

saveRDS(selfworthTrips.lm,"./ANALYSIS/Tables/selfworthTrips.lm.Rds")
#

selfworthTrips_scaled.lm <- lm_robust(tripsSelfworth_scaled ~ realTripsSubsidyPerBeneficiary, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworthTrips_scaled.lm) #standardized

texreg(selfworthTrips_scaled.lm)

saveRDS(selfworthTrips_scaled.lm,"./ANALYSIS/Tables/selfworthTrips_scaled.Rds")
#

dayToDaySkillsTrips.lm <- lm_robust(tripsDayToDaySkills_weighted ~ realTripsSubsidy, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkillsTrips.lm)

texreg(dayToDaySkillsTrips.lm)

saveRDS(dayToDaySkillsTrips.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips.lm.Rds")
#

dayToDaySkillsTrips_scaled.lm <- lm_robust(tripsDayToDaySkills_scaled ~ realTripsSubsidyPerBeneficiary, data = mergedData)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkillsTrips_scaled.lm) #standardized

texreg(dayToDaySkillsTrips_scaled.lm)

saveRDS(dayToDaySkillsTrips_scaled.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_scaled.lm.Rds")
#

#dataset: mergedDataImputeInterpolation
#lunch
selfworth_IM.lm <- lm_robust(selfworth_weighted ~ realSubsidy, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(selfworth_IM.lm)

texreg(selfworth_IM.lm)

saveRDS(selfworth_IM.lm,"./ANALYSIS/Tables/selfworth_IM.lm.Rds")
#

selfworth_IM_scaled.lm <- lm_robust(selfworth_scaled ~ realSubsidyPerBeneficiary, data = mergedDataImputeInterpolation)%>%
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
dayToDaySkills_IM_scaled.lm <- lm_robust(dayToDaySkills_scaled ~ realSubsidyPerBeneficiary, data = mergedDataImputeInterpolation)%>%
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
#

selfworthTrips_IM_scaled.lm <- lm_robust(tripsSelfworth_scaled ~ realTripsSubsidyPerBeneficiary, data = mergedDataImputeInterpolation)%>%
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
dayToDaySkillsTrips_IM_scaled.lm <- lm_robust(tripsDayToDaySkills_scaled ~ realTripsSubsidyPerBeneficiary, data = mergedDataImputeInterpolation)%>%
  extract.lm_robust(include.ci = FALSE)
summary(dayToDaySkillsTrips_IM_scaled.lm) #standardized

texreg(dayToDaySkillsTrips_IM_scaled.lm)

saveRDS(dayToDaySkillsTrips_IM_scaled.lm,"./ANALYSIS/Tables/dayToDaySkillsTrips_IM_scaled.lm.Rds")
# 

