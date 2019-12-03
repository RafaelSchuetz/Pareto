library(tidyverse)
library(readxl)
library(dplyr)
library(rlang)


df_2018 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2019")

df_2018 <- as_tibble(df_2018)

#nur Zeilen auswählen mit Information (davor waren es 1000 observations oder so)
df_2018 <- df_2018 %>% 
  filter(df_2018$Einrichtungsnummer != is.na(df_2018$Einrichtungsnummer))

#not available
df_2018[df_2018 == '-'] <- NA
df_2018[df_2018 == 'k.A.'] <- NA
df_2018[df_2018 == 'n.a.'] <- NA


#numerisch

df_2018 <- df_2018 %>% 
  mutate_if(is.character, as.numeric)

# Löschen von Spalten ohne Inhalt

df_2018$`Beteiligung 2019 bei`<- NULL
df_2018$`Hintergrund der KiJU in %`<- NULL
df_2018$`4,3,2,1,0,99`<- NULL
df_2018$`Schulischer Werdegang`<- NULL
df_2018$`Beruflicher Werdegang (Zahl)`<- NULL
df_2018$Aussagen <- NULL
df_2018$`DGE Kriterien (ja/nein)`<- NULL
df_2018$`Rezepte (ja/nein)` <- NULL
df_2018$Entdeckerfonds <- NULL
df_2018$`Wirkungen (4,3,2,1,0,99)`<- NULL
df_2018$`2019`<- NULL
df_2018$`Beschreibung der Aktivitäten`<- NULL

df_2018$'Davon:' <- NULL
df_2018$'Häufigkeit:' <- NULL


# Namen der Variablen ändern

#zeigt noch an, dass es 3 unknown columns gibt?

df_2018 <- df_2018 %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    age = 'Alter',
    kidsPerMealNo = 'Anzahl Ki pro Mahlzeit 2019',
    newkidsNo = 'neue Ki 2019',
    CateringNo = 'Anzahl Catering 2019',
    mealInInstitutionNo = 'Anzahl idEg 2019',
    mealNo = 'Summe der MZ für 2019',
    breakfastNo = 'Anzahl Frü 2019',
    lunchNo = 'Anzahl MT 2019',
    snackNo = 'Anzahl Nachmi 2019',
    dinnerNo = 'Anzahl AbBr 2019',
    frequency = 'x-mal pro Woche 2019',
    weeksOfferedNo = 'Wochen im Jahr 2019',
    daysOfferedNo = 'Angebotstage',
    DGENo = 'Anzahl DGE-Kriterien (NORA)',
    totalCosts = 'MT_Gesamtkosten pro Jahr 2019',
    subsidyAsked = 'beantragte Summe bei CH MT 2019',
    subsidy = 'Förderentscheidung 2019...21',
    migrationBackground = 'Migrationshintergrund %',
    refugee = 'Geflüchtete %',
    unemployment = 'Arbeitslosigkeit / prekäre Beschäftigung %',
    moreThanOneChildHousehold = 'Mehrkinderhaushalt %',
    singleParent = 'Alleinerziehend %',
    poverty = 'Aufwachsen in Armut %',
    participateMore = 'häufiger wegen MT',
    tasksLunch = 'Aufgaben rund um MT',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1x Woche',
    shoppers = 'einkaufen',
    ownIdeas = 'eigene Ideen & Vorschläge',
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
    betterTeamwork = 'besser im Team arbeiten',
    betterReading = 'besser lesen',
    betterGrades = 'Schulnoten verbessert',
    moreRegularSchoolVisits = 'regelmäßiger zur Schule gehen',
    selfworth = 'Selbstwertgefühl gestärkt',
    moreOpen = 'sind offener',
    moreConfidence = 'stärkeres Selbstvertrauen',
    adressProblems = 'sprechen Probleme an',
    proud = 'sind stolz',
    success = 'Erfolgserlebnisse...59',
    abiturNo = 'Abitur/FHR',
    mittlereReifeNo = 'Mittlerer SA',
    hauptschuleNo = 'Hauptschule',
    schoolNoneNo = 'Ohne',
    careerStarted = 'begonnen',
    careerFinished = 'abgeschlossen',
    enoughFood = 'ausreichend Essen',
    enoughStaffLunch = 'ausreichend Personal MT',
    enoughStaffActivities = 'ausreichend Personal / weitere Akt.',
    qualitySatisfies = 'mit Qualität zufrieden',
    regional = 'regionale Produkte',
    culture = 'kulturspez und rel Aspekte',
    unsweetenedDrinks = 'natürliche Getränke',
    tripsSuggestions = 'Vorschläge gemacht',
    tripsDecisions = 'entschieden',
    tripsOrganization = 'organisiert',
    tripsCostCalculation = 'Kostenplanung',
    tripsBudget = 'Budget verwaltet',
    tripsMoney = 'Kasse geführt',
    tripsReview = 'nachbereitet',
    tripsPublicTransport = 'öffentl. Nahverkehr',
    tripsMobility = 'Mobilität',
    tripsNewPlaces = 'Neue Orte',
    tripsNewCommunities = 'Neue Lebenswelten',
    tripsNewIdeas = 'Neue Ideen',
    tripsAdditionalActivities = 'TN weitere Aktivitäten PE',
    tripsSpecificSkills = 'Konkrete Kompetenzen',
    tripsDayToDaySkills = 'Kompetenzen im Alltag',
    tripsSuccess = 'Erfolgserlebnisse...95',
    tripsSelfWorking = 'positiv Selbstwirksam',
    tripsSelfworth = 'Selbstwertgefühl',
    tripsSocialSkills = 'soziale Kompetenzen',
    tripsAnger = 'Frusttoleranz',
    tripsNetworkSuggestions = 'CHILDREN Netzwerk Anregungen',
    tripsKidsVariousNo = 'Anzahl verschiedene Kinder gesamt',
    tripsSubsidyAskedNo = 'beantragte Gesamtsumme 2019',
    tripsSubsidyNo = 'Förderentscheidung 2019...108')

###von denen Wird eine Fehlermeldung gezeigt
#    tripsNo = 'Anzahl Aktivitäten 2019',
#tripsKidsNo = 'Anzahl Kinder 2019 gesamt',
#tripsEstimatedCosts = 'geschätzte Gesamtkosten 2019',
 #   tripsKidsNo = 'Anzahl Kinder 2019 gesamt',
  #  tripsEstimatedCosts = 'geschätzte Gesamtkosten 2019')

names(df_2018) [89] <- "tripsNo"
names(df_2018) [90] <- "tripsKidsNo"
names(df_2018) [91] <- "tripsEstimatedCostsNo"


# Jahr noch hinzufügen

df_2018 <- df_2018 %>% add_column(year = 2018)

library(ggplot2)
library(cowplot)















