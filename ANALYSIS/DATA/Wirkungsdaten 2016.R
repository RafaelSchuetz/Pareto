# PaRE3To-Projekt

# 1. Herunterladen der Daten ----------------------------------------------

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyverse)

### Daten 2016

Wirkungsdaten_2016unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                                 sheet = "2016")
View(Wirkungsdaten_2016unbereinigt)

# Löschen von Spalten ohne Inhalt

Wirkungsdaten_2016unbereinigt$Entdeckerfonds <- NULL


# 2. Bearbeitung der Daten ------------------------------------------------

# Änderung des Datentyps

data2016 <- Wirkungsdaten_2016unbereinigt %>% mutate_if(is.character, as.numeric)
View(data2016)

# Umbennen der Spalten bzw. Variablen

data2016 <- data2016 %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    kidsPerMeal = 'Anzahl Kinder pro Mahlzeit 2016',
    numberOfMeals = 'MT_Mahlzeiten 2016',
    frequency = 'Häufigkeit 2016(pro Woche x Wochen pro Jahr)',
    totalCosts = 'MT_Gesamtkosten 2016',
    subsidy = 'Bewilligt MT 2016',
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
    adressProblems = 'sprechen Probleme an',
    proud = 'sind stolz',
    enoughFood = 'genug Essen',
    enoughStaffLunch = 'genug Personal MT',
    enoughStaffActivities = 'genug Personal / weitere Akt.',
    qualitySatisfies = 'mit Qualität zufrieden',
    regional = 'regional',
    culture = 'Kultur',
    unsweetenedDrinks = 'ungesüßte Getränke',
    DGEbinary = 'DGE-Kriterien',
    recordRecipesBinary = 'Rezepte aufschreiben',
    tripsNo = 'Anzahl EF-Aktivitäten 2016',
    tripsKidsNo = 'Anzahl Kinder 2016',
    tripsSubsidy = 'Bewilligt EF 2016',
    tripsSuggestions = 'Vorschläge gemacht',
    tripsDecisions = 'entschieden',
    tripsOrganization = 'organisiert',
    tripsBudget = 'Budget verwaltet',
    tripsReview = 'nachbereitet',
    tripsPublicTransport = 'öffentl. Nahverkehr',
    tripsMobility = 'Mobilität',
    tripsNewPlaces = 'Neue Orte',
    tripsNewCommunities = 'Neue Lebenswelten',
    tripsNewIdeas = 'Neue Ideen',
    tripsSpecificSkills = 'Konkrete Kompetenzen',
    tripsDayToDaySkills = 'Kompetenzen im Alltag',
    tripsSelfworth = 'Selbstwertgefühl',
    tripsSocialSkills = 'soziale Kompetenzen',
    tripsNetworkSuggestions = 'CHILDREN Netzwerk Anregungen')

# Hinzufügen des Jahres: Allen Datenpunkten aus dem einzelnen Sheet muss die Variable "Jahr" 
# hinzugefügt werden, die für jede Beobachtung aus dem Sheet "2016" den Wert 2016 annimmt.

data2016 <- data2016 %>% add_column(year = 2016)
