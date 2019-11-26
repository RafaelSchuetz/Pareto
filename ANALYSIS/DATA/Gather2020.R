# This file gathers and cleans CHILDREN survey data for the year 2019, collected with the applications for grants in 2020

# Load packages

library(tidyverse)
library(readxl)
library(dplyr)

# read in data

## read in headers

headers <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2020_Unbereinigte Daten", n_max = 0) %>% 
    names()

## read in rest of data

dataWithOldHeaders <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = "2020_Unbereinigte Daten", skip = 2, col_names = headers)

# rename columns

data2020 <- dataWithOldHeaders %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    eatersPerMeal = 'Anzahl Ki pro Mahlzeit 2020',
    statistics = 'Statistische Auswertung',
    regularEaters = 'Anzahl der KiJu, die regelmäßig in PE gegessen haben',
    outOfWhich = 'Von diesen Kindern und Jugendlichen…',
    daySchoolers = 'besuchen Ganztagsschule...6',
    aimedEaters = 'gezielt für das Essen in Einrichtung...7',
    otherOffers = 'nehmen weitere Angebote wahr...8',
    moreGreens = 'essen mehr Obst und Gemüse...9',
    monthlyShoppers = 'einkaufen 1x Monat...10',
    weeklyCooks = 'kochen 1x Woche selbst...11',
    snackers = 'bereiten Snacks zu...12',
    newGreens = 'drei neue Obst- und Gemüsesorten...13',
    farmers = 'Gemüse oder Kräuter angebaut...14',
    dietaryKnowledge = 'Ernährungswissen erweitert...15',
    simpleMeals= 'einfache Gerichte zubereiten...16',
    dietaryTrip = 'mind. einen Ausflug zum Thema Ernährung...17',
    independent = 'selbstständiger...18',
    teamwork = 'besser im Team arbeiten...19',
    selfworth = 'Selbstwertgefühl...20',
    success = 'Erfolgserlebnisse gesammelt...21',
    dayToDaySkills = 'erweiterte Alltagskompetenzen...22',
    wholeNumbers = 'Umrechnung in absolute Zahlen',
    daySchoolersNo = 'besuchen Ganztagsschule...24',
    aimedEatersNo = 'gezielt für das Essen in Einrichtung...25',
    otherOffersNo = 'nehmen weitere Angebote wahr...26',
    moreGreensNo = 'essen mehr Obst und Gemüse...27',
    monthlyShoppersNo = 'einkaufen 1x Monat...28',
    weeklyCooksNo = 'kochen 1x Woche selbst...29',
    snackersNo = 'bereiten Snacks zu...30',
    newGreensNo = 'drei neue Obst- und Gemüsesorten...31',
    farmersNo = 'Gemüse oder Kräuter angebaut...32',
    dietaryKnowledgeNo = 'Ernährungswissen erweitert...33',
    simpleMealsNo = 'einfache Gerichte zubereiten...34',
    dietaryTripNo = 'mind. einen Ausflug zum Thema Ernährung...35',
    independentNo = 'selbstständiger...36',
    teamworkNo = 'besser im Team arbeiten...37',
    selfworthNo = 'Selbstwertgefühl...38',
    successNo = 'Erfolgserlebnisse gesammelt...39',
    dayToDaySkillsNo = 'erweiterte Alltagskompetenzen...40',
    establishment = '… wir als Einrichtung… (4,3,2,1,0,99)',
    dietaryKnowledgeEst = 'Wissen zum Thema Ernährung erweitert',
    qualityEst = 'Qualität der Angebote verbessert',
    designLunchEst = 'Anregungen für die Gestaltung des MT',
    needsPoorEst = 'besser auf Bedürfnisse armer Kinder eingehen',
    possibilitiesEst = 'mehr Möglichkeiten, päd. Arbeit zu gestalten',
    relationshipEst = 'Beziehung zu den KiJu gestärkt',
    participationEst = 'KiJu öfter und umfassender beteiligt',
    tryoutNo = 'Neues ausprobiert',
    notSureQuestionsEst = 'Wir waren uns nicht sicher, was mit den Fragen gemeint ist',
    agreementQuestions = 'Wir sind und bei allen Fragen einig gewesen',
    trips = 'Entdeckerfonds',
    tripsSuggestions = 'Vorschläge gemacht',
    tripsDecisions = 'mitentschieden',
    tripsOrganization = 'organisiert',
    tripsBudget = 'Budget verwaltet',
    tripsReview = 'nachbereitet',
    tripsPublicTransport = 'öffentl. Nahverkehr',
    tripsMobility = 'Mobilität',
    tripsNewPlaces = 'Neue Orte',
    tripsNewCommunities = 'Neue Lebenswelten',
    tripsNewIdeas = 'Neue Ideen',
    tripsOngoingParticipation = 'TN weitere Aktivitäten PE',
    tripsSpecificSkills = 'Konkrete Kompetenzen',
    tripsDayToDaySkills = 'Kompetenzen im Alltag', 
    tripsSelfworth = 'Selbstwertgefühl...66', 
    tripsSocialSkills = 'soziale Kompetenzen',
    tripsNetworkSuggestions = 'CHILDREN Netzwerk Anregungen')

data2020 <- data2020 %>% add_column(year=2019)