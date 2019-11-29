library(tidyverse)
library(readxl)
library(dplyr)


df_2019 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2019")

df_2019 <- as_tibble(df_2019)

#nur Zeilen auswählen mit Information (davor waren es 1000 observations oder so)
df_2019 <- df_2019 %>% 
  filter(df_2019$Einrichtungsnummer != is.na(df_2019$Einrichtungsnummer))

#not available
df_2019[df_2019 == '-'] <- NA
df_2019[df_2019 == 'k.A.'] <- NA
df_2019[df_2019 == 'n.a.'] <- NA


#numerisch

df_2019 <- df_2019 %>% 
  mutate_if(is.character, as.numeric)

# Löschen von Spalten ohne Inhalt

df_2019$`Beteiligung 2019 bei`<- NULL
df_2019$`Hintergrund der KiJU in %`<- NULL
df_2019$`4,3,2,1,0,99`<- NULL
df_2019$`Schulischer Werdegang`<- NULL
df_2019$`Beruflicher Werdegang (Zahl)`<- NULL
df_2019$Aussagen <- NULL
df_2019$`DGE Kriterien (ja/nein)`<- NULL
df_2019$`Rezepte (ja/nein)` <- NULL
df_2019$Entdeckerfonds <- NULL
df_2019$`Wirkungen (4,3,2,1,0,99)`<- NULL
df_2019$`2019`<- NULL
df_2019$`Beschreibung der Aktivitäten`<- NULL

df_2019$'Davon:' <- NULL
df_2019$'Häufigkeit:' <- NULL


# Namen der Variablen ändern

#zeigt noch an, dass es 3 unknown columns gibt?

df_2019 <- df_2019 %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    age = 'Alter',
    kidsPerMeal = 'Anzahl Ki pro Mahlzeit 2019',
    newkids = 'neue Ki 2019',
    numberOfCatering = 'Anzahl Catering 2019',
    numberOfidEg = 'Anzahl idEg 2019',
    numberOfMeals = 'Summe der MZ für 2019',
    numberOfBreakfasts = 'Anzahl Frü 2019',
    numberOfLunch = 'Anzahl MT 2019',
    numberOfSnack = 'Anzahl Nachmi 2019',
    numberOfDinner = 'Anzahl AbBr 2019',
    frequency = 'x-mal pro Woche 2019',
    weeksOffered = 'Wochen im Jahr 2019',
    daysOffered = 'Angebotstage',
    amountDGECriteria = 'Anzahl DGE-Kriterien (NORA)',
    totalCosts = 'MT_Gesamtkosten pro Jahr 2019',
    subsidyAsked = 'beantragte Summe bei CH MT 2019',
    subsidyReceived = 'Förderentscheidung 2019...21',
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
    abitur = 'Abitur/FHR',
    mittlererSA = 'Mittlerer SA',
    hauptschule = 'Hauptschule',
    none = 'Ohne',
    began = 'begonnen',
    finished = 'abgeschlossen',
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
    tripsNo = 'Anzahl Aktivitäten 2019',
    tripsKidsNo = 'Anzahl Kinder 2019 gesamt',
    tripsEstimatedCosts = 'geschätzte Gesamtkosten 2019',
    tripsKidsVariousNo = 'Anzahl verschiedene Kinder gesamt',
    tripsSubsidyAsked = 'beantragte Gesamtsumme 2019',
    tripsSubsidy = 'Förderentscheidung 2019...108')


# Jahr noch hinzufügen

#df_2019 <- df_2019 %>% add_column(year = 2019)


















