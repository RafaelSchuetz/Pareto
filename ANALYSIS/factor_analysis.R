#Faktoranalyse
# Factor analysis for ordinal data:
# http://dwoll.de/rexrepos/posts/multFApoly.html

#zuerst aus dem Datensatz mergedData nur die Variablen auswählen die auch tatsächlich dafür
#verwendet werden können
#z.B. kann ID der Einrichtung nicht verwendet werden 
#deshalb muss ich nur die relevanten Variablen auswählen

# --- create two data sets for factor analysis: one for Meals, one for Trips

additionalVariablesMeals <- c('DGECriteriaNoScaled', 'realSubsidyPerBeneficiary')

dfFAMealsPlus <- mergedDataImputeInterpolation %>% 
  dplyr::select(-any_of(generalOutcomes_scaled)) %>% 
  dplyr::select(c(any_of(alwaysRecordedVariablesMeals), additionalVariablesMeals)) %>% 
  dplyr::select(c(tidyselect::contains('_scaled') & !tidyselect::contains('trips'), additionalVariablesMeals)) %>%
  drop_na
         
dfFAMeals <- dfFAMealsPlus %>% 
  dplyr::select(-'realSubsidyPerBeneficiary')

nobsFAMeals <- nrow(dfFAMeals)

# General correlation matrix Meals

# with ML=TRUE, hetcor() takes very long to compute
# https://john-uebersax.com/stat/tetra.htm
# latent correlations better name than polychoric correlations
# pairwise.complete.obs. considered dangerous
# https://www.r-bloggers.com/pairwise-complete-correlation-considered-dangerous/

correlationMatrixMeals <- hetcor(dfFAMeals, ML=FALSE, use = "pairwise.complete.obs")

# hetcor() creates many warnings, either "In log(P) : NaNs wurden erzeugt" or "In polychor(x, y, ML = ML, std.err = std.err) : 1 column with zero marginal removed"

# select all correlations above a threshold
## transform correlation matrix
## https://stackoverflow.com/questions/28035001/transform-correlation-matrix-into-dataframe-with-records-for-each-row-column-pai
## print cases with correlations above a threshold
## https://stackoverflow.com/questions/49510472/print-cases-with-correlations-above-a-threshold

correlationMatrixMealsLong <- data.frame(variable1=rownames(correlationMatrixMeals$correlations)[row(correlationMatrixMeals$correlations)[upper.tri(correlationMatrixMeals$correlations)]],
                                         variable2=colnames(correlationMatrixMeals$correlations)[col(correlationMatrixMeals$correlations)[upper.tri(correlationMatrixMeals$correlations)]],
                                         correlation=correlationMatrixMeals$correlations[upper.tri(correlationMatrixMeals$correlations)])
highCorrelationsMeals <- correlationMatrixMealsLong %>%  arrange(desc(correlation)) %>% filter(abs(correlation)>0.5)

# --- Factor analysis for later regression: selfworth_scaled vs. realSubsidyPerBeneficiary

dfFA_Selfworth_RealSubsidyPerBeneficiary <- dfFAMeals %>% 
  dplyr::select(-'selfworth_scaled')

selfworth_RealSubsidyPerBeneficiary <- dfFAMealsPlus %>% 
  dplyr::select(c('selfworth_scaled', 'realSubsidyPerBeneficiary'))

correlationMatrix_Selfworth_RealSubsidyPerBeneficiary <- hetcor(dfFA_Selfworth_RealSubsidyPerBeneficiary, ML=TRUE, use = "pairwise.complete.obs")

numberFactors_Selfworth_RealSubsidyPerBeneficiary <- fa.parallel(correlationMatrix_Selfworth_RealSubsidyPerBeneficiary$correlations, fm = 'ml', fa = 'fa', n.obs = nobsFAMeals)

FA_Selfworth_RealSubsidyPerBeneficiary <- fa(dfFA_Selfworth_RealSubsidyPerBeneficiary, nfactors = numberFactors_Selfworth_RealSubsidyPerBeneficiary$nfact, scores = 'Bartlett', n.obs = nobsFAMeals, rotate = "varimax", fm = "ml")

scores_Selfworth_RealSubsidyPerBeneficiary <- data.frame(FA_Selfworth_RealSubsidyPerBeneficiary$scores)

selfworth_RealSubsidyPerBeneficiary_Factors <- cbind.data.frame(scores_Selfworth_RealSubsidyPerBeneficiary, selfworth_RealSubsidyPerBeneficiary)

# --- Factor analysis for later regression: dayToDaySkills_scaled vs. realSubsidyPerBeneficiary

dfFA_dayToDaySkills_RealSubsidyPerBeneficiary <- dfFAMeals %>% 
  dplyr::select(-'dayToDaySkills_scaled')

dayToDaySkills_RealSubsidyPerBeneficiary <- dfFAMealsPlus %>% 
  dplyr::select(c('dayToDaySkills_scaled', 'realSubsidyPerBeneficiary'))

correlationMatrix_dayToDaySkills_RealSubsidyPerBeneficiary <- hetcor(dfFA_dayToDaySkills_RealSubsidyPerBeneficiary, ML=TRUE, use = "pairwise.complete.obs")

numberFactors_dayToDaySkills_RealSubsidyPerBeneficiary <- fa.parallel(correlationMatrix_dayToDaySkills_RealSubsidyPerBeneficiary$correlations, fm = 'ml', fa = 'fa', n.obs = nobsFAMeals)

FA_dayToDaySkills_RealSubsidyPerBeneficiary <- fa(dfFA_dayToDaySkills_RealSubsidyPerBeneficiary, nfactors = numberFactors_dayToDaySkills_RealSubsidyPerBeneficiary$nfact, scores = 'Bartlett', n.obs = nobsFAMeals, rotate = "varimax", fm = "ml")

scores_dayToDaySkills_RealSubsidyPerBeneficiary <- data.frame(FA_dayToDaySkills_RealSubsidyPerBeneficiary$scores)

dayToDaySkills_RealSubsidyPerBeneficiary_Factors <- cbind.data.frame(scores_dayToDaySkills_RealSubsidyPerBeneficiary, dayToDaySkills_RealSubsidyPerBeneficiary)

# Factor analysis: lessIll_scaled vs. DGECriteriaNoScaled

dfFA_lessIll_DGECriteriaNo <- dfFAMeals %>% 
  dplyr::select(-c('lessIll_scaled', 'DGECriteriaNoScaled'))

lessIll_DGECriteriaNo <- dfFAMealsPlus %>% 
  dplyr::select(c('lessIll_scaled', 'DGECriteriaNoScaled'))

correlationMatrix_lessIll_DGECriteriaNo <- hetcor(dfFA_lessIll_DGECriteriaNo, ML=TRUE, use = "pairwise.complete.obs")

numberFactors_lessIll_DGECriteriaNo <- fa.parallel(correlationMatrix_lessIll_DGECriteriaNo$correlations, fm = 'ml', fa = 'fa', n.obs = nobsFAMeals)

FA_lessIll_DGECriteriaNo <- fa(dfFA_lessIll_DGECriteriaNo, nfactors = numberFactors_lessIll_DGECriteriaNo$nfact, scores = 'Bartlett', n.obs = nobsFAMeals, rotate = "varimax", fm = "ml")

scores_lessIll_DGECriteriaNo <- data.frame(FA_lessIll_DGECriteriaNo$scores)

lessIll_DGECriteriaNo_Factors <- cbind.data.frame(scores_lessIll_DGECriteriaNo, lessIll_DGECriteriaNo)

#factor analysis dietary knowledge_scaled vs DGECriteriaNoScaled

dfFA_dietaryKnowledge_DGECriteriaNo <- dfFAMeals %>% 
  dplyr::select(-c('dietaryKnowledge_scaled', 'DGECriteriaNoScaled'))

dietaryKnowledge_DGECriteriaNo <- dfFAMealsPlus %>% 
  dplyr::select(c('dietaryKnowledge_scaled', 'DGECriteriaNoScaled'))

correlationMatrix_dietaryKnowledge_DGECriteriaNo <- hetcor(dfFA_dietaryKnowledge_DGECriteriaNo, ML=TRUE, use = "pairwise.complete.obs")

numberFactors_dietaryKnowledge_DGECriteriaNo <- fa.parallel(correlationMatrix_dietaryKnowledge_DGECriteriaNo$correlations, fm = 'ml', fa = 'fa', n.obs = nobsFAMeals)

FA_dietaryKnowledge_DGECriteriaNo <- fa(dfFA_dietaryKnowledge_DGECriteriaNo, nfactors = numberFactors_dietaryKnowledge_DGECriteriaNo$nfact, scores = 'Bartlett', n.obs = nobsFAMeals, rotate = "varimax", fm = "ml")

scores_dietaryKnowledge_DGECriteriaNo <- data.frame(FA_dietaryKnowledge_DGECriteriaNo$scores)

dietaryKnowledge_DGECriteriaNo_Factors <- cbind.data.frame(scores_dietaryKnowledge_DGECriteriaNo, dietaryKnowledge_DGECriteriaNo)

# factor analysis appreciateHealthy_scaled vs. DGECriteriaNoScaled

dfFA_appreciateHealthy_DGECriteriaNo <- dfFAMeals %>% 
  dplyr::select(-c('appreciateHealthy_scaled', 'DGECriteriaNoScaled'))

appreciateHealthy_DGECriteriaNo <- dfFAMealsPlus %>% 
  dplyr::select(c('appreciateHealthy_scaled', 'DGECriteriaNoScaled'))

correlationMatrix_appreciateHealthy_DGECriteriaNo <- hetcor(dfFA_appreciateHealthy_DGECriteriaNo, ML=TRUE, use = "pairwise.complete.obs")

numberFactors_appreciateHealthy_DGECriteriaNo <- fa.parallel(correlationMatrix_appreciateHealthy_DGECriteriaNo$correlations, fm = 'ml', fa = 'fa', n.obs = nobsFAMeals)

FA_appreciateHealthy_DGECriteriaNo <- fa(dfFA_appreciateHealthy_DGECriteriaNo, nfactors = numberFactors_appreciateHealthy_DGECriteriaNo$nfact, scores = 'Bartlett', n.obs = nobsFAMeals, rotate = "varimax", fm = "ml")

scores_appreciateHealthy_DGECriteriaNo <- data.frame(FA_appreciateHealthy_DGECriteriaNo$scores)

appreciateHealthy_DGECriteriaNo_Factors <- cbind.data.frame(scores_appreciateHealthy_DGECriteriaNo, appreciateHealthy_DGECriteriaNo)
