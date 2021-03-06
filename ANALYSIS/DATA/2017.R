

df_2017 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2018")

df_2017 <- as_tibble(df_2017)

#not available machen

df_2017[df_2017 == '-'] <- NA

# numerisch 

df_2017 <- df_2017 %>% 
  mutate_if(is.character, as.numeric)

# Löschen von Spalten ohne Inhalt

df_2017$'%' <- NULL
df_2017$`4,3,2,1,0,99` <- NULL
df_2017$Entdeckerfonds <- NULL
df_2017$...72 <- NULL

# Namen der Variablen ändern

df_2017 <- df_2017 %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    eatersPerMealNo = 'Anzahl Ki pro Mahlzeit 2018',
    newKidsNo = 'neue Ki 2018',
    cateringNo = 'Anzahl Catering 2018',
    mealsInInstitutionNo = 'Anzahl idEg 2018',
    breakfastsNo = 'Anzahl Frü 2018',
    lunchesNo = 'Anzahl MT 2018',
    snacksNo = 'Anzahl Nachmi 2018',
    dinnersNo = 'Anzahl AbBr 2018',
    daysOfferedNo = 'Häufigkeit 2018(pro Woche x Wochen pro Jahr)',
    DGECriteriaNo = 'Anzahl DGE-Kriterien',
    totalCost = 'MT_Gesamtkosten 2018',
    subsidy = 'Förderentscheidung 2018',
    additionalSubsidy = 'Zusätzliche Förderung(Juni 2018)',
    refugeesShare = 'Geflüchtete %',
    notGermanSpokenHomeShare = 'Nicht Deutsch zu Hause %',
    perfectGermanShare = 'Fehlerfrei Deutsch %',
    povertyShare = 'Armut %',
    unemploymentShare = 'Eltern arbeitslos %',
    participateMore = 'häufiger wegen MT',
    tasksLunch = 'Aufgaben rund um MT',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    ownIdeas = 'eigene Ideen & Vorschläge',
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
    betterTeamwork = 'besser im Team arbeiten',
    betterReading = 'besser lesen',
    betterNumbers = 'besser mit Zahlen',
    betterGrades = 'Schulnoten verbessert',
    moreRegularSchoolVisits = 'regelmäßiger zur Schule gehen',
    selfworth = 'Selbstwertgefühl gestärkt',
    moreOpen = 'sind offener',
    moreConfidence = 'stärkeres Selbstvertrauen',
    addressProblems = 'sprechen Probleme an',
    proud = 'sind stolz',
    enoughFood = 'genug Essen',
    enoughStaffLunch = 'genug Personal MT',
    enoughStaffActivities = 'genug Personal / weitere Akt.',
    qualitySatisfies = 'mit Qualität zufrieden',
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
    tripsAdditionalActivities = 'TN weitere Aktivitäten PE',
    tripsSpecificSkills = 'Konkrete Kompetenzen',
    tripsDayToDaySkills = 'Kompetenzen im Alltag',
    tripsSelfworth = 'Selbstwertgefühl',
    tripsSocialSkills = 'soziale Kompetenzen',
    tripsCHILDRENSuggestions = 'CHILDREN Netzwerk Anregungen',
    tripsNo = 'Anzahl EF-Aktivitäten 2018',
    tripsKidsNo = 'Anzahl Kinder 2018',
    tripsSubsidy = 'Bewilligt EF 2018')

# noch zu jeder Spalte das Jahr hinzufügen

df_2017 <- df_2017 %>% add_column(year = 2017)



#rename.files("~./ANALYSIS/DATA", "2018.R", "2017.R")




