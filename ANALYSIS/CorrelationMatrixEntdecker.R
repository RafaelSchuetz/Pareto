#Aim: Correlation matrix for Entdecker Outcomes


# Outcomes Entdecker ------------------------------------------------------

#Create subset including the outcomes of interest:

corTrips <- subset(data201118, select = c("tripsSuggestions", "tripsDecisions", "tripsOrganization",
                                       "tripsCostCalculation", "tripsBudget", "tripsMoney", "tripsReview",
                                       "tripsPublicTransport", "tripsMobility", "tripsNewPlaces",
                                       "tripsNewCommunities", "tripsNewIdeas", "tripsAdditionalActivities",
                                       "tripsSpecificSkills", "tripsDayToDaySkills", "tripsSuccess",
                                       "tripsSelfEfficacy", "tripsSelfworth", "tripsSocialSkills",
                                       "tripsFrustrationTolerance", "tripsCHILDRENSuggestions",
                                       "tripsNo", "tripsKidsNo", "tripsTotalCost", "tripsDifferentKidsNo",
                                       "tripsSubsidyRequest", "tripsSubsidy"))

#create correlation matrix with missing values included


cor(corTrips, method = "pearson", use = "complete.obs")

#method: indicates the correlation coefficient to be computed. 
#The default is pearson correlation coefficient which measures the linear dependence between two variables.


