library(readxl)
library(dplyr)

data2014 <- read_excel("ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2014")
data2015 <- read_excel("ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2015")

#umbennenm

data2014 <- data2014 %>% 
    dplyr::rename(id = 'Einrichtungsnummer', 
                      numberOfMeals = 'MT_Mahlzeiten 2014',
                      regularEaters = 'MT_Kinder 2014',
                      finalCosts = 'MT_Gesamtkosten 2014',
                      subsidy = 'Fördersumme final MT 2014',
                      monthlyCooks = 'Kochen 1x Monat',
                      weeklyCooks = 'Kochen 1 Woche',
                      shoppers = 'einkaufen',
                      snackers = 'zubereiten', 
                      dietaryKnowledge = 'Wissen erweitert',
                      appreciateHealthy = 'schätzen gesunde Ernährung',
                      foodCulture = 'schätzen gem. Esskultur',
                      influenceHome = 'beeinflussen Esskultur Familien', 
                      lessIll = 'seltener krank', 
                      dayToDaySkills = 'erweiterte Alltagskompetenzen', 
                      selfworth = 'Selbstwertgefühl gestärkt', 
                      participateMore = 'kommen häufiger', 
                      enoughFood = 'genug Essen', 
                      qualitySatisfies = 'Qualität zufrieden', 
                      enoughStaffLunch = 'genug Personal MT', 
                      enoughStaffActivities = 'genug Personal / weitere Akt.', 
                      trips = "Entdeckerfonds", 
                      tripsNo = 'Aktivitäten 2014', 
                      tripsKidsNo = 'Kinder 2014', 
                      tripsSubsidy = 'Fördersumme EF 2014', 
                      tripsDecisions = 'entschieden', 
                      tripsOrganization = 'organisiert', 
                      tripsReview = 'nachbereitet', 
                      tripsMobility = 'Mobilität', 
                      tripsKnowledge = 'veränderte Kenntnisse', 
                      tripsBehavior = 'Verhalten verändert')

data2015 <- data2015 %>% 
  dplyr::rename(id = 'Einrichtungsnummer',
                kidsPerMeal = 'Anzahl Kinder pro Mahlzeit',
                numberOfMeals = 'MT_Mahlzeiten 2015', 
                frequency = 'Häufigkeit 2015(pro Woche x Wochen pro Jahr)', 
                totalCosts = 'MT_Gesamtkosten 2015', 
                subsidy = 'MT 2015', 
                DGE = 'DGE Kritierien', 
                participateMore = 'häufiger wegen MT', 
                tasksLunch = 'Aufgaben rund um MT', 
                monthlyCooks = 'Kochen 1x Monat',
                weeklyCooks = 'Kochen 1 Woche', 
                shoppers = 'einkaufen', 
                ownIdeas = 'eigene Ideen& Vorschläge', 
                easyDish = 'einfache Gerichte zubereiten', 
                dietaryKnowledge = 'Wissen erweitert',
                appreciateHealthy = 'schätzen gesunde Ernährung', 
                foodCulture = 'schätzen gem. Esskultur', 
                influenceHome = 'beeinflussen Esskultur Familien', 
                cookAtHome = 'kochen MT-Gerichte zu Hause nach', 
                askRecipe = 'fragen Rezepte nach'
                



names(data2015)[1] <- "id" 
  names(data2015)[2] <- "kidsPerMeal"
  names(data2015)[3] <- "numberOfMeals"
  names(data2015)[4] <- "Frequency"
  names(data2015)[5] <- "totalCosts"
  names(data2015)[6] <- "MT2015"
  names(data2015)[7] <- "criteriaDGE"
  names(data2015)[8] <- "moreOftenMT"
  names(data2015)[9] <- "tasksMT"
  names(data2015)[10] <- "monthlyCooks"
  names(data2015)[11] <- "weeklyCooks"
  names(data2015)[12] <- "shoppers"
  names(data2015)[13] <- "ownIdeas"
  names(data2015)[14] <- "easyDish"
  names(data2015)[15] <- "dietaryKnowledge"
  names(data2015)[16] <- "appreciateHealthyDietary"
  names(data2015)[17] <- "appreciateFoodCulture"
  names(data2015)[18] <- "influenceHomeFoodCulture"
  names(data2015)[19] <- "cookMTDishAtHome"
  names(data2015)[20] <- "askForRecipe"
  names(data2015)[21] <- "moreConcentrated"
  names(data2015)[22] <- "moreBalanced"
  names(data2015)[23] <- "lessIll"
  names(data2015)[24] <- "dayToDaySkillsNo"
  names(data2015)[25] <- "moreIndependent"
  names(data2015)[26] <- "selfworthNo"
  names(data2015)[27] <- "moreOpen"
  names(data2015)[28] <- "moreConfidence"
  names(data2015)[29] <- "adressProblems"
  names(data2015)[30] <- "proud"
  names(data2015)[31] <- "enoughFood"
  names(data2015)[32] <- "enoughStaffMT"
  names(data2015)[33] <- "enoughStaffMore"
  names(data2015)[34] <- "qualitySatisfies"
  names(data2015)[35] <- "bio"
  names(data2015)[36] <- "seasonal"
  names(data2015)[37] <- "regional"
  names(data2015)[38] <- "culture"
  names(data2015)[39] <- "unsweetenedDrinks"
  names(data2015)[40] <- "dialogParents"
  names(data2015)[41] <- "niceFoodSector"
  names(data2015)[42] <- "menu"
  names(data2015)[43] <- "menuCycle"
  names(data2015)[44] <- "numberOfActivities"
  names(data2015)[45] <- "tripsKidsNo"
  names(data2015)[46] <- "tripsConveyorSum"
  names(data2015)[47] <- "tripsDecisions"
  names(data2015)[48] <- "tripsOrganized"
  names(data2015)[49] <- "tripsFollowUp"
  names(data2015)[50] <- "tripsReached"
  names(data2015)[51] <- "tripsMobility"
  names(data2015)[52] <- "tripsChangedKnowledge"
  names(data2015)[53] <- "tripsChangedBehavior"
  
  data2014 <- data2014 %>% mutate_if(is.character, as.numeric)
  data2015 <- data2015 %>% mutate_if(is.character, as.numeric)
  
  
  
  