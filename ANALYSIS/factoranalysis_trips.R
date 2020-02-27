#Faktoranalyse
# Factor analysis for ordinal data:
# http://dwoll.de/rexrepos/posts/multFApoly.html

#zuerst aus dem Datensatz mergedData nur die Variablen auswählen die auch tatsächlich dafür
#verwendet werden können
#z.B. kann ID der Einrichtung nicht verwendet werden 
#deshalb muss ich nur die relevanten Variablen auswählen

# --- create two data sets for factor analysis: one for Meals, one for Trips

additionalVariablesTrips <- 'realTripsSubsidyPerBeneficiary'

dfFATripsPlus <- mergedDataImputeInterpolation %>% 
  dplyr::select(-any_of(generalOutcomes_scaled)) %>% 
  dplyr::select(c(any_of(alwaysRecordedVariablesTrips), additionalVariablesTrips)) %>% 
  dplyr::select(c(tidyselect::contains('_scaled') & tidyselect::contains('trips'), additionalVariablesTrips)) %>%
  drop_na

dfFATrips <- dfFATripsPlus %>% 
  dplyr::select(-'realTripsSubsidyPerBeneficiary')

nobsFATrips <- nrow(dfFATrips)

#generalcoorelationmatrix 

correlationMatrixTrips <- hetcor(dfFATrips, ML=FALSE, use = "pairwise.complete.obs")

##
correlationMatrixTripsLong <- data.frame(variable1=rownames(correlationMatrixTrips$correlations)[row(correlationMatrixTrips$correlations)[upper.tri(correlationMatrixTrips$correlations)]],
                                         variable2=colnames(correlationMatrixTrips$correlations)[col(correlationMatrixTrips$correlations)[upper.tri(correlationMatrixTrips$correlations)]],
                                         correlation=correlationMatrixTrips$correlations[upper.tri(correlationMatrixTrips$correlations)])
highCorrelationsTrips <- correlationMatrixTripsLong %>%  arrange(desc(correlation)) %>% filter(abs(correlation)>0.3)

##
# --- Factor analysis for later regression: tripsDayToDaySkills_scaled vs. realTripsSubsidyPerBeneficiary

dfFA_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary <- dfFATrips %>% 
  dplyr::select(-'tripsDayToDaySkills_scaled')

tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary <- dfFATripsPlus %>% 
  dplyr::select(c('tripsDayToDaySkills_scaled', 'realTripsSubsidyPerBeneficiary'))

correlationMatrix_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary <- hetcor(dfFA_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary, ML=TRUE, use = "pairwise.complete.obs")

numberFactors_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary <- fa.parallel(correlationMatrix_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary$correlations, fm = 'ml', fa = 'fa', n.obs = nobsFATrips)

FA_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary <- fa(dfFA_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary, nfactors = numberFactors_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary$nfact, scores = 'Bartlett', n.obs = nobsFATrips, rotate = "varimax", fm = "ml")

scores_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary <- data.frame(FA_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary$scores)

tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary_Factors <- cbind.data.frame(scores_tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary, tripsDayToDaySkills_RealTripsSubsidyPerBeneficiary)

