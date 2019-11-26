# PaRE3To-Projekt

# 1. Herunterladen der Daten ----------------------------------------------

rm(list = ls())

library(readxl)
library(dplyr)
library(tidyverse)

### Daten 2017

Wirkungsdaten_2017unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                 sheet = "2017")
View(Wirkungsdaten_2017unbereinigt)

# Löschen von Spalten ohne Inhalt

Wirkungsdaten_2017unbereinigt$...17 <- NULL
Wirkungsdaten_2017unbereinigt$Entdeckerfonds <- NULL

# 2. Bearbeitung der Daten ------------------------------------------------

# Änderung des Datentyps

data2017 <- Wirkungsdaten_2017unbereinigt %>% mutate_if(is.character, as.numeric)

View(data2017)

# Umbennen der Spalten bzw. Variablen

data2017 <- data2017 %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    overallChildrenPerInstitution = 'Anzahl KiJu insgesamt in der Einrichtung',
    age = 'Alter der KiJu',
    overallBudget = 'Gesamtbudget der Einrichtung',
    kidsPerMeal = 'Anzahl Ki pro Mahlzeit 2017',
    newChildren = 'neue Ki beim MT 2017',
    numberCatering = 'Anzahl Catering 2017',
    numberMealWithinInstitution = 'Anzahl idEg 2017',
    numberBreakfast = 'Anzahl Frü 2017',
    numberMT = 'Anzahl MT 2017',
    numberAfternoon = 'Anzahl Nachmi 2017',
    numberDinner = 'Anzahl AbBr 2017',
    frequency = 'Häufigkeit 2017(pro Woche x Wochen pro Jahr)',
    DGE = 'Anzahl DGE-Kriterien',
    totalCosts = 'MT_Gesamtkosten 2017',
    subsidy = 'Bewilligt MT 2017',
    participateMore = 'häufiger wegen MT',
    tasksLunch = 'Aufgaben rund um MT',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    ownIdeas = 'eigene Ideen& Vorschläge',
    longerPeriodVisitors = 'längeren Zeitraum Besucher',
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
    suggestionMTforChildren = 'Anregungen MT über CH',
    DGEbinary = 'DGE-Kriterien',
    recordRecipesBinary = 'Rezepte aufschreiben',
    tripsNo = 'Anzahl EF-Aktivitäten 2017',
    tripsKidsNo = 'Anzahl Kinder 2017',
    tripsSubsidy = 'Bewilligt EF 2017',
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
# hinzugefügt werden, die für jede Beobachtung aus dem Sheet "2017" den Wert 2017 annimmt.

data2017 <- data2017 %>% add_column(year = 2017)


# 3. Analyse der Daten ----------------------------------------------------

library(ggplot2)

# Histogramm

hist(Wirkungsdaten_2017$`Anzahl KiJu insgesamt in der Einrichtung`)

# Scatterplot

plot(x = Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`, 
     y = Wirkungsdaten_2017$`Anzahl Ki pro Mahlzeit 2017`)

# Statistische Zusammenfassung

summary(Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`)
