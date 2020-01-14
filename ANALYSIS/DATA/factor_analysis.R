#Faktoranalyse
#Laura

#zuerst aus dem Datensatz mergedData nur die Variablen auswählen die auch tatsächlich dafür
#verwendet werden können
#z.B. kann ID der Einrichtung nicht verwendet werden 
#deshalb muss ich nur die relevanten Variablen auswählen

#relevate packages laden
library(dplyr)
library(tidyverse)

factor <- mergedData

factor <- factor %>% 
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
         proud,
         success,
         enoughFood,
         enoughStaffLunch,
         enoughStaffActivities,
         qualitySatisfies,
         regionalProducts,
         cultureReligion,
         unsweetenedDrinks,
         tripsSuggestions,
         tripsDecisions,
         tripsOrganization,
         tripsCostCalculation,
         tripsBudget,
         tripsMoney,
         tripsReview,
         tripsPublicTransport,
         tripsMobility,
         tripsNewPlaces,
         tripsNewCommunities,
         tripsNewIdeas,
         tripsAdditionalActivities,
         tripsSpecificSkills,
         tripsDayToDaySkills,
         tripsSuccess,
         tripsSelfEfficacy,
         tripsSelfworth,
         tripsSocialSkills,
         tripsFrustrationTolerance,
         tripsCHILDRENSuggestions,
         organicFoodstuff,
         seasonalFoodstuff,
         parentalDialog,
         friendlyEnvironment,
         menu,
         menuCycle,
         tripsReached,
         tripsKnowledge,
         tripsBehavior,
         claimBTP,
         benefitBTP,
         noSchoolLunch,
         expensiveSchoolLunch)


#weitere R packages installieren/laden
library(psych)
#install.packages('psy')
library(psy)
#install.packages('nFactors')
library(nFactors)

#alle ausgwählten Variablen beschreiben
#bei allen min und max anschauen
#schauen ob es outliers gibt
#und ob ich tatsächlich nur kategoriale Variablen ausgewählt habe
describe(factor)


#correlation mit ordinalen variablen



#Bartlett-Test
#prüft ob die Variablen miteinander korrelieren
#wenn der Test signifikant, dann wissen wir ob die variablen miteinander korrelieren
cortest.bartlett(factor)

#umwandeln der variablen in numeric
factor <- data.frame(lapply(factor, function(factor_vector) as.numeric(as.character(factor_vector))))

#mochmal bartlett-test
cortest.bartlett(factor)

cor(factor, y = NULL, use = 'pairwise.complete.obs', method = 'pearson')


