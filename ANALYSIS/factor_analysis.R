#Faktoranalyse
# Factor analysis for ordinal data:
# http://dwoll.de/rexrepos/posts/multFApoly.html

#zuerst aus dem Datensatz mergedData nur die Variablen auswählen die auch tatsächlich dafür
#verwendet werden können
#z.B. kann ID der Einrichtung nicht verwendet werden 
#deshalb muss ich nur die relevanten Variablen auswählen

#relevante packages laden
library(tidyverse)
library(polycor)
library(psych)
library(lavaan)

# --- create two data sets for factor analysis: one for Meals, one for Trips

additionalVariablesMeals <- c('DGECriteriaNoScaled', 'realSubsidyPerBeneficiary')

dfFAMealsPlus <- mergedDataImputeInterpolation %>% 
  dplyr::select(-any_of(generalOutcomes_scaled)) %>% 
  dplyr::select(c(any_of(alwaysRecordedVariablesMeals), additionalVariablesMeals)) %>% 
  dplyr::select(c(tidyselect::contains('_scaled') & !tidyselect::contains('trips'), additionalVariablesMeals)) %>%
  drop_na
         
dfFAMeals <- dfFAMealsPlus %>% 
  dplyr::select(-'realSubsidyPerBeneficiary')

nobsFA <- nrow(dfFAMeals)

# General correlation matrix Meals

# with ML=TRUE, hetcor() takes very long to compute
# https://john-uebersax.com/stat/tetra.htm
# latent correlations better name than polychoric correlations
# pairwise.complete.obs. considered dangerous
# https://www.r-bloggers.com/pairwise-complete-correlation-considered-dangerous/

correlationMatrixMeals <- hetcor(ordinalVariablesMealsFA, ML=FALSE, use = "pairwise.complete.obs")

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

selfWorth_RealSubsidyPerBeneficiary <- dfFAMealsPlus %>% 
  dplyr::select(c('selfworth_scaled', 'realSubsidyPerBeneficiary'))

correlationMatrix_Selfworth_RealSubsidyPerBeneficiary <- hetcor(dfFA_Selfworth_RealSubsidyPerBeneficiary, ML=TRUE, use = "pairwise.complete.obs")

numberFactors_Selfworth_RealSubsidyPerBeneficiary <- fa.parallel(correlationMatrix_Selfworth_RealSubsidyPerBeneficiary$correlations, fm = 'ml', fa = 'fa', n.obs = nobsFA)

FA_Selfworth_RealSubsidyPerBeneficiary <- fa(dfFA_Selfworth_RealSubsidyPerBeneficiary, nfactors = numberFactors_Selfworth_RealSubsidyPerBeneficiary$nfact, scores = 'Bartlett', n.obs = nobsFA, rotate = "varimax", fm = "ml")

scores_Selfworth_RealSubsidyPerBeneficiary <- data.frame(FA_Selfworth_RealSubsidyPerBeneficiary$scores)

selfWorth_RealSubsidyPerBeneficiary_Factors <- cbind.data.frame(scores_Selfworth_RealSubsidyPerBeneficiary, selfWorth_RealSubsidyPerBeneficiary)

# --- Factor analysis for later regression: lessIll_scaled vs. DGECriteriaNoScaled

DGECriteriaNolessIll <- c('DGECriteriaNoScaled', 'lessIll_scaled')


ordinalVariablesDGECriteriaNolessIll <- dfFAMeals %>% 
  dplyr::select(-DGECriteriaNolessIll)

lessIll_DGECriteriaNo <- ordinalVariablesMeals %>% 
  dplyr::select(DGECriteriaNolessIll) %>% 
  data.frame()

# with ML=TRUE, hetcor() takes very long to compute
# https://john-uebersax.com/stat/tetra.htm
# latent correlations better name than polychoric correlations
# pairwise.complete.obs. considered dangerous
# https://www.r-bloggers.com/pairwise-complete-correlation-considered-dangerous/

correlationMatrixMeals <- hetcor(ordinalVariablesMealsFA, ML=FALSE, use = "pairwise.complete.obs")

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

# use fa.parallel to estimate optimal number of factors
# https://www.promptcloud.com/blog/exploratory-factor-analysis-in-r/

numberFactorsMeals <- fa.parallel(correlationMatrixMeals$correlations, fm = 'ml', fa = 'fa', n.obs = nrow(ordinalVariablesMealsFA))

# fa.parallel(correlationMatrixMeals$correlations, fm = 'ml', fa = 'fa', n.obs = 300) suggests that the number of factors =  9

# perform exploratory factor analysis on these variables 

names(ordinalVariablesMealsFA)

# this is the factor analysis

factorAnalysisMeals <- fa(ordinalVariablesMealsFA, nfactors = numberFactorsMeals$nfact, scores = "regression", n.obs = nrow(ordinalVariablesMealsFA), rotate = "varimax", fm = "ml")

# show factor loadings

loadings(factorAnalysisMeals)

# save factor scores

scoresMeals <- data.frame(factorAnalysisMeals$scores)

# append variables lessIll_scaled and DGECriteria_scaled to matrix with factor scores

lessIll_DGECriteriaNo_scoresMeals <- cbind.data.frame(scoresMeals, lessIll_DGECriteriaNo)

# show first couple of values of each variable

str(lessIll_DGECriteriaNo_scoresMeals)

# regress lessIll_scaled on DGECriteriaNoScaled, with factors as controls

lm_lessIll_DGECriteriaNo_scoresMeals <- lm(lessIll_scaled ~ DGECriteriaNoScaled + ML1 + ML2 + ML3, lessIll_DGECriteriaNo_scoresMeals)

# show summary of linear model fit

summary(lm_lessIll_DGECriteriaNo_scoresMeals)

# factorAnalysisMeals2 <- fa(ordinalVariablesMeals, nfactors = 2, scores="regression", n.obs = 48, rotate = "varimax", fm = "ml")
# if you do not use fm = "ml", these warnings appear: 
# Warnmeldungen:
# 1: In fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs,  :
# The estimated weights for the factor scores are probably incorrect.  Try a different factor score estimation method.
# 2: In fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate,  :
# An ultra-Heywood case was detected.  Examine the results carefully




#ordinalVariablesMeals <- ordinalVariablesMeals[,all(is.na(ordinalVariablesMeals))]
# %>% 
#          select_if(all(!is.na(.))) %>% 
#            data.matrix()

#ordinalVariablesMeals <- ordinalVariablesMeals[,colSums(is.na(ordinalVariablesMeals)) == 0]

# only select variables that were have at least one year of collection in common with all other variables

ordinalVariablesTrips <- mergedData %>% 
  select(tripsSuggestions,
         tripsDecisions,
         tripsOrganization,
         tripsCostCalculation, # only collected for 2018
         tripsBudget,
         tripsMoney, # only collected for 2018
         tripsReview,
         tripsPublicTransport,
         tripsMobility,
         tripsNewPlaces,
         tripsNewCommunities,
         tripsNewIdeas,
         tripsAdditionalActivities,
         tripsSpecificSkills,
         tripsDayToDaySkills,
         tripsSuccess, # only collected for 2018
         tripsSelfEfficacy, # only collected for 2018
         tripsSelfworth,
         tripsSocialSkills,
         tripsFrustrationTolerance, # only collected for 2018
         tripsCHILDRENSuggestions)
  #        tripsReached,
  #        tripsKnowledge,
  #        tripsBehavior,
  #        claimBTP,
  #        benefitBTP)

correlationMatrixTrips <- hetcor(ordinalVariablesTrips, use = "pairwise.complete.obs")

correlationMatrixTripsLong <- data.frame(variable1=rownames(correlationMatrixTrips$correlations)[row(correlationMatrixTrips$correlations)[upper.tri(correlationMatrixTrips$correlations)]], 
                                         variable2=colnames(correlationMatrixTrips$correlations)[col(correlationMatrixTrips$correlations)[upper.tri(correlationMatrixTrips$correlations)]], 
                                         correlation=correlationMatrixTrips$correlations[upper.tri(correlationMatrixTrips$correlations)])
highCorrelationsTrips <- correlationMatrixTripsLong %>%  arrange(desc(correlation)) %>% filter(abs(correlation)>0.5)

numberFactorsTrips <- fa.parallel(correlationMatrixTrips$correlations, fm = 'ml', fa = 'fa', n.obs = 200)

factorAnalysisTrips <- fa(correlationMatrixTrips$correlations, nfactors = 4, scores="tenBerge", n.obs = 200, rotate = "varimax", fm = "ml")


# factor analysis assuming variables are metric, standardized



