df <- dataWithState
df <- as_tibble(df)

#alle Variablen die 99 sind: Kategorische Daten und 99 bedeutet: keine Angabe
df[df == '99'] <- NA

#kategoriale Variablen definieren
#umändern der numerischen Variablen in kategorische Variablen

#numerisch in kategorische Variablen umändern
#so wird numeric in factor umgewandelt
#muss den Datensatz aber nochmal laden, dass es dann auch richtig angezeigt wir

#einfach alle relevanten variablen als kategorische variablen
#code wäre etwas problematisch

df$participateMore=as.factor(df$participateMore)
df$tasksLunch = as.factor((df$tasksLunch))
df$monthlyCooks = as.factor(df$monthlyCooks)
df$weeklyCooks = as.factor(df$weeklyCooks)
df$shoppers = as.factor(df$shoppers)
df$ownIdeas = as.factor(df$ownIdeas)
df$easyDishes = as.factor(df$easyDishes)
df$dietaryKnowledge = as.factor(df$dietaryKnowledge)
df$appreciateHealthy = as.factor((df$appreciateHealthy))
df$foodCulture = as.factor(df$foodCulture)
df$influenceHome = as.factor(df$influenceHome)
df$cookAtHome = as.factor(df$cookAtHome)
df$askRecipes = as.factor(df$askRecipes)
df$moreConcentrated = as.factor(df$moreConcentrated)
df$moreBalanced = as.factor(df$moreBalanced)
df$lessIll = as.factor(df$lessIll)
df$dayToDaySkills = as.factor(df$dayToDaySkills)
df$moreIndependent = as.factor(df$moreIndependent)
df$betterTeamwork = as.factor(df$betterTeamwork)
df$betterReading = as.factor(df$betterReading)
df$betterGrades = as.factor(df$betterGrades)
df$moreRegularSchoolVisits = as.factor(df$moreRegularSchoolVisits)
df$selfworth = as.factor(df$selfworth)
df$moreOpen = as.factor(df$moreOpen)
df$moreConfidence = as.factor(df$moreConfidence)
df$addressProblems = as.factor(df$addressProblems)
df$proud = as.factor(df$proud)
df$success = as.factor(df$success)
df$enoughFood = as.factor(df$enoughFood)
df$enoughStaffLunch = as.factor(df$enoughStaffLunch)
df$enoughStaffActivities = as.factor(df$enoughStaffActivities)
df$qualitySatisfies = as.factor(df$qualitySatisfies)
df$regionalProducts = as.factor(df$regionalProducts)
df$cultureReligion = as.factor(df$cultureReligion)
df$unsweetenedDrinks = as.factor(df$unsweetenedDrinks)
df$tripsSuggestions = as.factor(df$tripsSuggestions)
df$tripsDecisions = as.factor(df$tripsDecisions)
df$tripsOrganization = as.factor(df$tripsOrganization)
df$tripsCostCalculation = as.factor(df$tripsCostCalculation)
df$tripsBudget = as.factor(df$tripsBudget)
df$tripsMoney = as.factor(df$tripsMoney)
df$tripsReview = as.factor(df$tripsReview)
df$tripsPublicTransport = as.factor(df$tripsPublicTransport)
df$tripsMobility = as.factor(df$tripsMobility)
df$tripsNewPlaces = as.factor(df$tripsNewPlaces)
df$tripsNewCommunities = as.factor(df$tripsNewCommunities)
df$tripsNewIdeas = as.factor(df$tripsNewIdeas)
df$tripsAdditionalActivities = as.factor(df$tripsAdditionalActivities)
df$tripsSpecificSkills = as.factor(df$tripsSpecificSkills)
df$tripsDayToDaySkills = as.factor(df$tripsDayToDaySkills)
df$tripsSuccess = as.factor(df$tripsSuccess)
df$tripsSelfEfficacy = as.factor(df$tripsSelfEfficacy)
df$tripsSelfworth = as.factor(df$tripsSelfworth)
df$tripsSocialSkills = as.factor(df$tripsSocialSkills)
df$tripsFrustrationTolerance = as.factor(df$tripsFrustrationTolerance)
df$tripsCHILDRENSuggestions = as.factor(df$tripsCHILDRENSuggestions)
df$betterNumbers = as.factor(df$betterNumbers)
df$addressProblems = as.factor(df$addressProblems)
df$parentalDialog = as.factor(df$parentalDialog)
df$friendlyEnvironment = as.factor(df$friendlyEnvironment)
df$menu = as.factor(df$menu)
df$menuCycle = as.factor(df$menuCycle)
df$tripsReached = as.factor(df$tripsReached)
df$tripsKnowledge = as.factor(df$tripsKnowledge)
df$tripsBehavior = as.factor(df$tripsBehavior)
df$selfworth = as.factor(df$selfworth)
df$claimBTP = as.factor(df$claimBTP)
df$benefitBTP = as.factor(df$benefitBTP)
df$stayLonger = as.factor(df$stayLonger)
df$suggestionsForCHILDREN = as.factor(df$suggestionsForCHILDREN)
df$organicFoodstuff = as.factor(df$organicFoodstuff)
df$seasonalFoodstuff = as.factor(df$seasonalFoodstuff)
df$noSchoolLunch = as.factor(df$noSchoolLunch)
df$expensiveSchoolLunch = as.factor(df$expensiveSchoolLunch)














