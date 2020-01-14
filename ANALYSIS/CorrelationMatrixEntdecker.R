# Theoretical background
# Measures of association for ordinal variables
# http://rcompanion.org/handbook/H_11.html

# load packages

library(DescTools)
library(lavaan)

#Aim: Correlation matrix for Entdecker Outcomes


# Outcomes Entdecker ------------------------------------------------------

#Create subset including the outcomes of interest:

corTrips <- subset(mergedData, select = c("tripsSuggestions", "tripsDecisions", "tripsOrganization",
                                       "tripsCostCalculation", "tripsBudget", "tripsMoney", "tripsReview",
                                       "tripsPublicTransport", "tripsMobility", "tripsNewPlaces",
                                       "tripsNewCommunities", "tripsNewIdeas", "tripsAdditionalActivities",
                                       "tripsSpecificSkills", "tripsDayToDaySkills", "tripsSuccess",
                                       "tripsSelfEfficacy", "tripsSelfworth", "tripsSocialSkills",
                                       "tripsFrustrationTolerance", "tripsCHILDRENSuggestions",
                                       "tripsNo", "tripsKidsNo", "tripsTotalCost", "tripsDifferentKidsNo",
                                       "tripsSubsidyRequest", "tripsSubsidy"))

#create a simple correlation matrix with missing values included

map2(corTrips, corTrips, compare_tbls)

GoodmanKruskalGamma(mergedData$dietaryKnowledge, mergedData$appreciateHealthy, conf.level = 0.95)

cor_auto(corTrips)

corSpearman <- cor(corTrips, corTrips, method = "spearman", use = "complete.obs")

corSpearman <- cor.test(corTrips, )

#method: indicates the correlation coefficient to be computed. 
#The default is pearson correlation coefficient which measures the linear dependence between two variables.


# Create Matrix with p-values (significance levels) ---------------------------------------------

#install.packages("Hmisc")

library("Hmisc")
corTripsSign <- rcorr(as.matrix(corTrips))

# Extract the correlation coefficients
corTripsSign$r
# Extract p-values
corTripsSign$P

#Note:The output of the function rcorr() is a list containing the following elements : 
#- r : the correlation matrix 
#- n : the matrix of the number of observations used in analyzing each pair of variables 
#- P : the p-values corresponding to the significance levels of correlations. 



# better overview ---------------------------------------------------------

#Use symnum() function: Symbolic number coding

#The R function symnum() replaces correlation coefficients by symbols according to the level of the correlation. 
#It takes the correlation matrix as an argument :

symnum(corTrips, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95),
       symbols = c(" ", ".", ",", "+", "*", "B"),
       abbr.colnames = TRUE)


#error: "benoetige 2 symbols fuer boolesches 'x' argument

#Draw a correlogram

#install.packages("corrplot")

library(corrplot)
corrplot(corTrips, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Error in matrix(if (is.null(value)) logical() else value, nrow = nr, dimnames = list(rn,  : 
#length of 'dimnames' [2] not equal to array extent

plot(data201118$tripsBudget, data201118$tripsNewPlaces)
