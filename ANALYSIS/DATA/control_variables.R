### Kontrollvariablen für die DiD-Estimations

# Die id-fixed effects kontrollieren bereits für alle zeit-invarianten Eigenschaften, die 
# zwischen den Einrichtungen variieren (z.B. Bundesland, Große / Kleine Stadt). Die time-fixed
# effects kontrollieren für Eigenschaften, die über die Zeit variieren, aber für die Einrichtungen
# konstant sind (z.B. allgemeiner Zeittrend). Aus diesem Grund werden nur Kontrollvariablen 
# ermittelt, die über die Zeit variieren.

### Ermitteln relevanter zeit-varianter Kontrollvariablen


# Methode: Korrelation zwischen den relevanten Variablen und möglichen Kontrollvariablen aus
# den Mittagstisch Outcomes



#Aim: Correlation Matrix of Mittagstisch Outcomes
#data of interest: data201118 to include only numeric variables
#show cor between complete observations to include missing values


# Outcomes Mittagstisch ---------------------------------------------------


#1. Correlation matrix: Outcomes Mittagstisch
#Create subset including the outcomes of interest:

corMT_controls <- subset(dfcEF, select = c('selfworth',
                                           'dayToDaySkills',
                                           'treatEF',
                                           "eatersPerMealNo", "newKidsNo", "cateringNo",
                                           "mealsInInstitutionNo", "mealsNo",  "breakfastsNo",
                                           "lunchesNo", "snacksNo", "dinnersNo", "offersPerWeekNo",
                                           "weeksOfferedNo", "daysOfferedNo", "DGECriteriaNo", "totalCost",
                                           "subsidyRequest", "subsidy", "migrantBackgroundShare", "refugeesShare",
                                           "unemploymentShare", "multipleChildrenHouseholdShare", "singleParentShare",
                                           "povertyShare", "participateMore", "tasksLunch", "monthlyCooks", "weeklyCooks",
                                           "shoppers", "ownIdeas", "stayLonger", "easyDishes", "dietaryKnowledge",
                                           "appreciateHealthy", "foodCulture", "influenceHome", "cookAtHome", "askRecipes",
                                           "moreConcentrated", "moreBalanced", "lessIll", "moreIndependent",
                                           "betterTeamwork", "betterReading", "betterNumbers", "betterGrades",
                                           "moreRegularSchoolVisits", "moreOpen", "moreConfidence",
                                           "addressProblems", "proud", "success", "abiturNo", "mittlereReifeNo", "hauptschuleNo",
                                           "noDegreeNo", "trainingStartedNo", "trainingCompletedNo", "enoughFood",
                                           "enoughStaffLunch", "enoughStaffActivities", "qualitySatisfies", "regionalProducts",
                                           "cultureReligion", "unsweetenedDrinks"))

#create a correlation matrix 
cor(dfcEF$selfworth, corMT_controls, method = "pearson", use = 'complete.obs')

#as matrix

library(Hmisc)

correlation_matrix_controls <- rcorr(as.matrix(corMT_controls))

#extract p-values
#matrix with p-values only
p <- correlation_matrix_controls$P
R <- correlation_matrix_controls$r

#define notions for significance levels



