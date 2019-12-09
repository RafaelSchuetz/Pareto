#Aim: Correlation Matrix of Mittagstisch Outcomes
#data of interest: data201118 to include only numeric variables
#show cor between complete observations to include missing values


# Outcomes Mittagstisch ---------------------------------------------------

#1. Correlation matrix: Outcomes Mittagstisch
#Create subset including the outcomes of interest:

corMT <- subset(data201118, select = c("eatersPerMealNo", "newKidsNo", "cateringNo",
                                 "mealsInInstitutionNo", "mealsNo",  "breakfastsNo",
                                 "lunchesNo", "snacksNo", "dinnersNo", "offersPerWeekNo",
                                 "weeksOfferedNo", "daysOfferedNo", "DGECriteriaNo", "totalCost",
                                 "subsidyRequest", "subsidy", "migrantBackgroundShare", "refugeesShare",
                                 "unemploymentShare", "multipleChildrenHouseholdShare", "singleParentShare",
                                 "povertyShare", "participateMore", "tasksLunch", "monthlyCooks", "weeklyCooks",
                                 "shoppers", "ownIdeas", "stayLonger", "easyDishes", "dietaryKnowledge",
                                 "appreciateHealthy", "foodCulture", "influenceHome", "cookAtHome", "askRecipes",
                                 "moreConcentrated", "moreBalanced", "lessIll", "dayToDaySkills", "moreIndependent",
                                 "betterTeamwork", "betterReading", "betterNumbers", "betterGrades",
                                 "moreRegularSchoolVisits", "selfworth", "moreOpen", "moreConfidence",
                                 "addressProblems", "proud", "success", "abiturNo", "mittlereReifeNo", "hauptschuleNo",
                                 "noDegreeNo", "trainingStartedNo", "trainingCompletedNo", "enoughFood",
                                 "enoughStaffLunch", "enoughStaffActivities", "qualitySatisfies", "regionalProducts",
                                 "cultureReligion", "unsweetenedDrinks"))

#note: DGECriteriaBinary and recipesBinary are excluded, Fehlermeldung "keine vollständigen Elementpaare" 


#create correlation matrix with missing values included


cor(corMT, method = "pearson", use = "complete.obs")

#method: indicates the correlation coefficient to be computed. 
#The default is pearson correlation coefficient which measures the linear dependence between two variables.



