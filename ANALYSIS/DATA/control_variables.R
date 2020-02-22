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

corMT <- subset(dfcEF, select = c("eatersPerMealNo", "newKidsNo", "cateringNo",
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

cor(dfcEF$selfworth, corMT, method = c("pearson", "kendall", "spearman"), use = "complete.obs")

# Day-To-Day-Skills als Zielvariable
lm1_skills <- lm_robust(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$subsidy)
summary(lm1_skills)


