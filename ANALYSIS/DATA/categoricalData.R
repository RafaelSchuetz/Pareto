mergedData <- as_tibble(mergedData)

#alle Variablen die 99 sind: Kategorische Daten und 99 bedeutet: keine Angabe
mergedData[mergedData == '99'] <- NA

#kategoriale Variablen definieren
#umändern der numerischen Variablen in kategorische Variablen

#numerisch in kategorische Variablen umändern
#so wird numeric in factor umgewandelt
#muss den Datensatz aber nochmal laden, dass es dann auch richtig angezeigt wir

#einfach alle relevanten variablen als kategorische variablen
#code wäre etwas problematisch

mergedData$participateMore=as.factor(mergedData$participateMore)
mergedData$tasksLunch = as.factor((mergedData$tasksLunch))
mergedData$monthlyCooks = as.factor(mergedData$monthlyCooks)
mergedData$weeklyCooks = as.factor(mergedData$weeklyCooks)
mergedData$shoppers = as.factor(mergedData$shoppers)
mergedData$ownIdeas = as.factor(mergedData$ownIdeas)
mergedData$easyDishes = as.factor(mergedData$easyDishes)
mergedData$dietaryKnowledge = as.factor(mergedData$dietaryKnowledge)
mergedData$appreciateHealthy = as.factor((mergedData$appreciateHealthy))
mergedData$foodCulture = as.factor(mergedData$foodCulture)
mergedData$influenceHome = as.factor(mergedData$influenceHome)
mergedData$cookAtHome = as.factor(mergedData$cookAtHome)
mergedData$askRecipes = as.factor(mergedData$askRecipes)
mergedData$moreConcentrated = as.factor(mergedData$moreConcentrated)
mergedData$moreBalanced = as.factor(mergedData$moreBalanced)
mergedData$lessIll = as.factor(mergedData$lessIll)
mergedData$dayToDaySkills = as.factor(mergedData$dayToDaySkills)
mergedData$moreIndependent = as.factor(mergedData$moreIndependent)
mergedData$betterTeamwork = as.factor(mergedData$betterTeamwork)
mergedData$betterReading = as.factor(mergedData$betterReading)
mergedData$betterGrades = as.factor(mergedData$betterGrades)
mergedData$moreRegularSchoolVisits = as.factor(mergedData$moreRegularSchoolVisits)
mergedData$selfworth = as.factor(mergedData$selfworth)
mergedData$moreOpen = as.factor(mergedData$moreOpen)
mergedData$moreConfidence = as.factor(mergedData$moreConfidence)
mergedData$addressProblems = as.factor(mergedData$addressProblems)
mergedData$proud = as.factor(mergedData$proud)
mergedData$success = as.factor(mergedData$success)
mergedData$enoughFood = as.factor(mergedData$enoughFood)
mergedData$enoughStaffLunch = as.factor(mergedData$enoughStaffLunch)
mergedData$enoughStaffActivities = as.factor(mergedData$enoughStaffActivities)
mergedData$qualitySatisfies = as.factor(mergedData$qualitySatisfies)
mergedData$regionalProducts = as.factor(mergedData$regionalProducts)
mergedData$cultureReligion = as.factor(mergedData$cultureReligion)
mergedData$unsweetenedDrinks = as.factor(mergedData$unsweetenedDrinks)
mergedData$tripsSuggestions = as.factor(mergedData$tripsSuggestions)
mergedData$tripsDecisions = as.factor(mergedData$tripsDecisions)
mergedData$tripsOrganization = as.factor(mergedData$tripsOrganization)
mergedData$tripsCostCalculation = as.factor(mergedData$tripsCostCalculation)
mergedData$tripsBudget = as.factor(mergedData$tripsBudget)
mergedData$tripsMoney = as.factor(mergedData$tripsMoney)
mergedData$tripsReview = as.factor(mergedData$tripsReview)
mergedData$tripsPublicTransport = as.factor(mergedData$tripsPublicTransport)
mergedData$tripsMobility = as.factor(mergedData$tripsMobility)
mergedData$tripsNewPlaces = as.factor(mergedData$tripsNewPlaces)
mergedData$tripsNewCommunities = as.factor(mergedData$tripsNewCommunities)
mergedData$tripsNewIdeas = as.factor(mergedData$tripsNewIdeas)
mergedData$tripsAdditionalActivities = as.factor(mergedData$tripsAdditionalActivities)
mergedData$tripsSpecificSkills = as.factor(mergedData$tripsSpecificSkills)
mergedData$tripsDayToDaySkills = as.factor(mergedData$tripsDayToDaySkills)
mergedData$tripsSuccess = as.factor(mergedData$tripsSuccess)
mergedData$tripsSelfEfficacy = as.factor(mergedData$tripsSelfEfficacy)
mergedData$tripsSelfworth = as.factor(mergedData$tripsSelfworth)
mergedData$tripsSocialSkills = as.factor(mergedData$tripsSocialSkills)
mergedData$tripsFrustrationTolerance = as.factor(mergedData$tripsFrustrationTolerance)
mergedData$tripsCHILDRENSuggestions = as.factor(mergedData$tripsCHILDRENSuggestions)
mergedData$betterNumbers = as.factor(mergedData$betterNumbers)
mergedData$addressProblems = as.factor(mergedData$addressProblems)
mergedData$parentalDialog = as.factor(mergedData$parentalDialog)
mergedData$friendlyEnvironment = as.factor(mergedData$friendlyEnvironment)
mergedData$menu = as.factor(mergedData$menu)
mergedData$menuCycle = as.factor(mergedData$menuCycle)
mergedData$tripsReached = as.factor(mergedData$tripsReached)
mergedData$tripsKnowledge = as.factor(mergedData$tripsKnowledge)
mergedData$tripsBehavior = as.factor(mergedData$tripsBehavior)
mergedData$selfworth = as.factor(mergedData$selfworth)
mergedData$claimBTP = as.factor(mergedData$claimBTP)
mergedData$benefitBTP = as.factor(mergedData$benefitBTP)
mergedData$stayLonger = as.factor(mergedData$stayLonger)
mergedData$suggestionsForCHILDREN = as.factor(mergedData$suggestionsForCHILDREN)
mergedData$organicFoodstuff = as.factor(mergedData$organicFoodstuff)
mergedData$seasonalFoodstuff = as.factor(mergedData$seasonalFoodstuff)
mergedData$noSchoolLunch = as.factor(mergedData$noSchoolLunch)
mergedData$expensiveSchoolLunch = as.factor(mergedData$expensiveSchoolLunch)














