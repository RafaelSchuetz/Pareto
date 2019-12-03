library(readxl)
library(dplyr)
library(tidyverse)

data2013 <- read_excel("ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2014")
data2014 <- read_excel("ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2015")

data2013 <- data2013 %>% 
  filter(data2013$Einrichtungsnummer != is.na(data2013$Einrichtungsnummer))

data2014 <- data2014 %>% 
  filter(data2014$Einrichtungsnummer != is.na(data2014$Einrichtungsnummer))


#umbennen

data2013 <- data2013 %>% 
    dplyr::rename(id = 'Einrichtungsnummer', 
                      mealNo = 'MT_Mahlzeiten 2014',
                      regularEatersNo = 'MT_Kinder 2014',
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
                      tripsReached = 'erreicht',
                      tripsMobility = 'Mobilität', 
                      tripsKnowledge = 'veränderte Kenntnisse', 
                      tripsBehavior = 'Verhalten verändert')

data2014 <- data2014 %>% 
  dplyr::rename(id = 'Einrichtungsnummer',
                kidsPerMealNo = 'Anzahl Kinder pro Mahlzeit 2015',
                mealNo = 'MT_Mahlzeiten 2015', 
                frequency = 'Häufigkeit 2015(pro Woche x Wochen pro Jahr)', 
                totalCosts = 'MT_Gesamtkosten 2015', 
                subsidy = 'MT 2015', 
                DGENo = 'DGE Kritierien', 
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
                askRecipe = 'fragen Rezepte nach', 
                moreConcentrated = 'sind konzentrierter', 
                moreBalanced = 'sind ausgeglichener', 
                lessIll = 'seltener krank', 
                dayToDaySkills = 'erweiterte Alltagskompetenzen',
                moreIndependent = 'sind selbstständiger',
                selfworth = 'Selbstwertgefühl gestärkt',
                moreOpen = 'sind offener', 
                moreConfidence = 'stärkeres Selbstvertrauen', 
                addressProblems = 'sprechen Probleme an', 
                proud = 'sind stolz',
                enoughFood = 'genug Essen',
                enoughStaffLunch = 'genug Personal MT', 
                enoughStaffActivities = 'genug Personal / weitere Akt.', 
                qualitySatisfies = 'mit Qualität zufrieden', 
                bio = 'Bio/ öko', 
                seasonal = 'Saisonal',  
                regional = 'regional', 
                culture = 'Kultur', 
                unsweetenedDrinks = 'ungesüßte Getränke', 
                parentalDialog = 'Dialog mit Eltern', 
                friendlyEnvironment = 'Speisebreich freundlich', 
                menu = 'Speiseplan', 
                menuCycle = 'Menüzyklus', 
                tripsNo = 'Anzahl Entdeckeraktivitäten 2015', 
                tripsKidsNo = 'Anzahl Kinder 2015', 
                tripsSubsidy = 'EF 2015', 
                tripsDecisions = 'entschieden', 
                tripsOrganization = 'organisiert', 
                tripsReview = 'nachbereitet',
                tripsReached = 'erreicht',
                tripsMobility = 'Mobilität', 
                tripsKnowledge = 'veränderte Kenntnisse', 
                tripsBehavior = 'Verhalten verändert')
                
                
  
  data2013 <- data2013 %>% mutate_if(is.character, as.numeric)
  data2014 <- data2014 %>% mutate_if(is.character, as.numeric)
  
  
  data2013 <- data2013 %>% add_column(year = 2013)
  data2014 <- data2014 %>% add_column(year = 2014)
  
  