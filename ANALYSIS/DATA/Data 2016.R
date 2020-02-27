# PaRE3To-Projekt

# 1. Herunterladen der Daten -----------------------------------------------

### Daten 2017

Wirkungsdaten_2016unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                 sheet = "2017")

# Löschen von Spalten ohne Inhalt

Wirkungsdaten_2016unbereinigt$...17 <- NULL
Wirkungsdaten_2016unbereinigt$Entdeckerfonds <- NULL

# 2. Bearbeitung der Daten ------------------------------------------------

# Änderung des Datentyps

data2016 <- Wirkungsdaten_2016unbereinigt %>% mutate_if(is.character, as.numeric)


# Umbennen der Spalten bzw. Variablen

data2016 <- data2016 %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    kidsTotalNo = 'Anzahl KiJu insgesamt in der Einrichtung',
    age = 'Alter der KiJu',
    totalBudget = 'Gesamtbudget der Einrichtung',
    eatersPerMealNo = 'Anzahl Ki pro Mahlzeit 2017',
    newKidsNo = 'neue Ki beim MT 2017',
    cateringNo = 'Anzahl Catering 2017',
    mealsInInstitutionNo = 'Anzahl idEg 2017',
    breakfastsNo = 'Anzahl Frü 2017',
    lunchesNo = 'Anzahl MT 2017',
    snacksNo = 'Anzahl Nachmi 2017',
    dinnersNo = 'Anzahl AbBr 2017',
    daysOfferedNo = 'Häufigkeit 2017(pro Woche x Wochen pro Jahr)',
    DGECriteriaNo = 'Anzahl DGE-Kriterien',
    totalCost = 'MT_Gesamtkosten 2017',
    subsidy = 'Bewilligt MT 2017',
    participateMore = 'häufiger wegen MT',
    tasksLunch = 'Aufgaben rund um MT',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    ownIdeas = 'eigene Ideen& Vorschläge',
    stayLonger = 'längeren Zeitraum Besucher',
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
    suggestionsForCHILDREN = 'Anregungen MT über CH',
    DGECriteriaBinary = 'DGE-Kriterien',
    recipesBinary = 'Rezepte aufschreiben',
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
    tripsCHILDRENSuggestions = 'CHILDREN Netzwerk Anregungen')


# Hinzufügen des Jahres: Allen Datenpunkten aus dem einzelnen Sheet muss die Variable "Jahr" 
# hinzugefügt werden, die für jede Beobachtung aus dem Sheet "2016" den Wert 2016 annimmt.

data2016 <- data2016 %>% add_column(year = 2016)


