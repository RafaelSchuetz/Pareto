#Datens?tze 2012 und 2013 in R importieren und bearbeiten


# Datensaetze aus Excel in R ziehen ----------------------------------------

headers2011 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 11, n_max = 0) %>% 
  names()

data2011_unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 11) 

headers2012 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 10, n_max = 0) %>% 
  names()

data2012_unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 10)



# rename columns 2012 ----------------------------------------------------------

data2011 <- data2011_unbereinigt %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    eatersPerMealNo = 'Kinder',
    mealsNo = 'Mittagsmahlzeiten',
    subsidy = 'MT 2012_Fördersumme final',
    totalCost = 'Gesamtkosten MT 2012',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    easyDishes = 'zubereiten',
    dietaryKnowledge = 'Wissen erweitert',
    appreciateHealthy = 'schätzen gesunde Ernährung',
    foodCulture = 'schätzen gem. Esskultur',
    influenceHome = 'beeinflussen Esskultur Familien',
    lessIll = 'seltener krank',
    dayToDaySkills = 'erweiterte Alltagskompetenzen',
    selfworth = 'Selbstwertgefühl gestärkt',
    participateMore = 'kommen häufiger',
    claimBTP = 'Anspruch BTP',
    benefitBTP = 'nutzen BTP',
    noSchoolLunch = 'Schule ohne MT',
    expensiveSchoolLunch = 'MT Schule zu teuer',
    enoughFood = 'genug Essen',
    qualitySatisfies = 'Qualität zufrieden',
    enoughStaffLunch = 'genug Personal MT', 
    enoughStaffActivities = 'genug Personal / weitere Akt.')
    

# rename columns 2013 -----------------------------------------------------

data2012 <- data2012_unbereinigt %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    eatersPerMealNo = 'MT_Kinder',
    mealsNo = 'MT_Mahlzeiten',
    totalCost = 'MT_Gesamtkosten',
    subsidy = 'Bewilligt MT 2013',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    easyDishes = 'zubereiten',
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
    tripsNo = 'Aktivitäten 2013', 
    tripsKidsNo = 'Kinder 2013', 
    tripsSubsidy = 'Bewilligt EF 2013', 
    tripsDecisions = 'entschieden', 
    tripsOrganization = 'organisiert', 
    tripsReview = 'nachbereitet', 
    tripsReached = 'erreicht',
    tripsMobility = 'Mobilität', 
    tripsKnowledge = 'veränderte Kenntnisse', 
    tripsBehavior = 'Verhalten verändetr')


# final data as numeric ---------------------------------------------------

data2011 <- data2011 %>% mutate_if(is.character, as.numeric)
data2012 <- data2012 %>% mutate_if(is.character, as.numeric)


# Add years ---------------------------------------------------------------

data2011 <- data2011 %>% add_column(year=2011)
data2012 <- data2012 %>% add_column(year=2012)





