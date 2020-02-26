# PaRE3To-Projekt

# 1. Herunterladen der Daten ----------------------------------------------

### Daten 2016

Wirkungsdaten_2015unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                                 sheet = "2016")

# Löschen von Spalten ohne Inhalt

Wirkungsdaten_2015unbereinigt$Entdeckerfonds <- NULL


# 2. Bearbeitung der Daten ------------------------------------------------

# Änderung des Datentyps

data2015 <- Wirkungsdaten_2015unbereinigt %>% mutate_if(is.character, as.numeric)

# Umbennen der Spalten bzw. Variablen

data2015 <- data2015 %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    eatersPerMealNo = 'Anzahl Kinder pro Mahlzeit 2016',
    mealsNo = 'MT_Mahlzeiten 2016',
    daysOfferedNo = 'Häufigkeit 2016(pro Woche x Wochen pro Jahr)',
    totalCost = 'MT_Gesamtkosten 2016',
    subsidy = 'Bewilligt MT 2016',
    participateMore = 'häufiger wegen MT',
    tasksLunch = 'Aufgaben rund um MT',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    ownIdeas = 'eigene Ideen& Vorschläge',
    easyDishes = 'einfache Gerichte zubereiten',
    dietaryKnowledge = 'Wissen erweitert',
    appreciateHealthy = 'schätzen gesunde Ernährung',
    foodCulture = 'schätzen gem. Esskultur',
    influenceHome = 'beeinflussen Esskultur Familien',
    cookAtHome = 'kochen MT-Gerichte zu Hause nach',
    askRecipes = 'fragen Rezepte nach',
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
    regionalProducts = 'regional',
    cultureReligion = 'Kultur',
    unsweetenedDrinks = 'ungesüßte Getränke',
    DGECriteriaBinary = 'DGE-Kriterien',
    recipesBinary = 'Rezepte aufschreiben',
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
    tripsCHILDRENSuggestions = 'CHILDREN Netzwerk Anregungen')

# Hinzufügen des Jahres: Allen Datenpunkten aus dem einzelnen Sheet muss die Variable "Jahr" 
# hinzugefügt werden, die für jede Beobachtung aus dem Sheet "2016" den Wert 2016 annimmt.

data2015 <- data2015 %>% add_column(year = 2015)
