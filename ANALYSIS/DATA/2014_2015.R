library(readxl)
library(dplyr)

data2014 <- read_excel("ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2014")
data2015 <- read_excel("ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2015")
View(data2014)
#umbennen

data2014 <- data2014 %>% 
    dplyr::rename(id = 'Einrichtungsnummer', 
                      numberOfMeals = 'MT_Mahlzeiten 2014',
                      numberOfKids = 'MT_Kinder 2014',
                      finalCosts = 'MT_Gesamtkosten 2014',
                      conveyorSum = 'Fördersumme final MT 2014',
                      monthlyCooks = 'Kochen 1x Monat',
                      weeklyCooks = 'Kochen 1 Woche',
                      shoppers = 'einkaufen',
                      cooks = 'zubereiten', 
                      dietaryKnowledge = 'Wissen erweitert',
                      appreciateHealthyDietary = 'schätzen gesunde Ernährung',
                      appreciateFoodCulture = 'schätzen gem. Esskultur',
                      influenceHomeFoodCulture = 'beeinflussen Esskultur Familien', 
                      lessIll = 'seltener krank', 
                      dayToDaySkillsNo = 'erweiterte Alltagskompetenzen', 
                      selfworthNo = 'Selbstwertgefühl gestärkt', 
                      participateMoreOften = 'kommen häufiger', 
                      enoughFood = 'genug Essen', 
                      qualitySatisfies = 'Qualität zufrieden', 
                      enoughStaff = 'genug Personal MT', 
                      enoughStaffMore = 'genug Personal / weitere Akt.', 
                      trips = "Entdeckerfonds", 
                      numberOfActivities = 'Aktivitäten 2014', 
                      tripsKidsNo = 'Kinder 2014', 
                      tripsConeyorSum = 'Fördersumme EF 2014', 
                      tripsDecisions = 'entschieden', 
                      tripsOrganization = 'organisiert', 
                      tripFollowUp = 'nachbereitet', 
                      tripMobility = 'Mobilität', 
                      tripChangedKnowledge = 'veränderte Kenntnisse', 
                      tripChangedBehavior = 'Verhalten verändert')





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
  
  
  
  