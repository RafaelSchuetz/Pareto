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


#create a simple correlation matrix with missing values included


cor(corMT, method = "pearson", use = "complete.obs")

#method: indicates the correlation coefficient to be computed. 
#The default is pearson correlation coefficient which measures the linear dependence between two variables.

# Create Matrix with p-values (significance levels) ---------------------------------------------

#install.packages("Hmisc")

library("Hmisc")
corMTSign <- rcorr(as.matrix(corMT))

# Extract the correlation coefficients
corMTSign$r
# Extract p-values
corMTSign$P

#Note:The output of the function rcorr() is a list containing the following elements : 
#- r : the correlation matrix 
#- n : the matrix of the number of observations used in analyzing each pair of variables 
#- P : the p-values corresponding to the significance levels of correlations. 



# better overview ---------------------------------------------------------

#Use symnum() function: Symbolic number coding

#The R function symnum() replaces correlation coefficients by symbols according to the level of the correlation. 
#It takes the correlation matrix as an argument :

symnum(corMT, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95),
       symbols = c(" ", ".", ",", "+", "*", "B"),
       abbr.colnames = TRUE)
?symnum

#error: "benoetige 2 symbols fuer boolesches 'x' argument

#Draw a correlogram

#install.packages("corrplot")

library(corrplot)
corrplot(corMT, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
?corrplot
#Error in matrix(if (is.null(value)) logical() else value, nrow = nr, dimnames = list(rn,  : 
#length of 'dimnames' [2] not equal to array extent