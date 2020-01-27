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


ordinalVariablesMeals <- mergedData %>% 
  dplyr::select(participateMore, 
         tasksLunch,
         monthlyCooks,
         weeklyCooks,
         shoppers,
         ownIdeas,
         stayLonger,
         easyDishes,
         dietaryKnowledge,
         appreciateHealthy,
         foodCulture,
         influenceHome,
         cookAtHome,
         askRecipes,
         moreConcentrated,
         moreBalanced,
         lessIll,
         dayToDaySkills,
         moreIndependent,
         betterTeamwork,
         betterReading,
         betterNumbers,
         betterGrades,
         moreRegularSchoolVisits,
         selfworth,
         moreOpen,
         moreConfidence,
         addressProblems,
         proud)
         # success, # collected only for 2018
         # enoughFood,
         # enoughStaffLunch,
         # enoughStaffActivities,
         # qualitySatisfies,
         # regionalProducts,
         # cultureReligion,
         # unsweetenedDrinks,
         # noSchoolLunch,
         # expensiveSchoolLunch,
         # organicFoodstuff,
         # seasonalFoodstuff,
         # parentalDialog,
         # friendlyEnvironment,
         # menu,
         # menuCycle) 

# with ML=TRUE, hetcor() takes very long to compute
# https://john-uebersax.com/stat/tetra.htm
# latent correlations better name than polychoric correlations
# pairwise.complete.obs. considered dangerous
# https://www.r-bloggers.com/pairwise-complete-correlation-considered-dangerous/

correlationMatrixMeals <- hetcor(ordinalVariablesMeals, ML=FALSE, use = "pairwise.complete.obs")

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

numberFactorsMeals <- fa.parallel(correlationMatrixMeals$correlations, fm = 'ml', fa = 'fa', n.obs = 300)

# fa.parallel(correlationMatrixMeals$correlations, fm = 'ml', fa = 'fa', n.obs = 300) suggests that the number of factors =  9

factorAnalysisMeals <- fa(correlationMatrixMeals$correlations, nfactors = 9, scores="tenBerge", n.obs = 300, rotate = "varimax", fm = "ml")

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




#factorAnalysisMeals2 <- fa.poly(ordinalVariablesMeals, nfactors = 10)
# #weitere R packages installieren/laden
# library(psych)
# #install.packages('psy')
# library(psy)
# #install.packages('nFactors')
# library(nFactors)
# 
# #alle ausgwählten Variablen beschreiben
# #bei allen min und max anschauen
# #schauen ob es outliers gibt
# #und ob ich tatsächlich nur kategoriale Variablen ausgewählt habe
# describe(factor)
# 
# 
# #correlation mit ordinalen variablen
# 
# 
# 
# #Bartlett-Test
# #prüft ob die Variablen miteinander korrelieren
# #wenn der Test signifikant, dann wissen wir ob die variablen miteinander korrelieren
# cortest.bartlett(factor)
# 
# #umwandeln der variablen in numeric
# factor <- data.frame(lapply(factor, function(factor_vector) as.numeric(as.character(factor_vector))))
# 
# #mochmal bartlett-test
# cortest.bartlett(factor)
# 
# cor(factor, y = NULL, use = 'pairwise.complete.obs', method = 'pearson')
# 

