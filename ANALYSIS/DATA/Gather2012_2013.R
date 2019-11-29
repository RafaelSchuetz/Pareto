#Datens?tze 2012 und 2013 in R importieren und bearbeiten


# Packages Laden ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)


# Datensaetze aus Excel in R ziehen ----------------------------------------

headers2012 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 11, n_max = 0) %>% 
  names()

data2012_unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 11) 

headers2013 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 10, n_max = 0) %>% 
  names()

data2013_unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 10)



# rename columns 2012 ----------------------------------------------------------

data2012 <- data2012_unbereinigt %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    numberOfKids = 'Kinder',
    numberOfMeals = 'Mittagsmahlzeiten',
    conveyorSum = 'MT 2012_Fördersumme final',
    finalCosts = 'Gesamtkosten MT 2012',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    cooks = 'zubereiten',
    dietaryKnowledge = 'Wissen erweitert',
    appreciateHealthyDietary = 'schätzen gesunde Ernährung',
    appreciateFoodCulture = 'schätzen gem. Esskultur',
    influenceHomeFoodCulture = 'beeinflussen Esskultur Familien',
    lessIll = 'seltener krank',
    dayToDaySkillsNo = 'erweiterte Alltagskompetenzen',
    selfworthNo = 'Selbstwertgefühl gestärkt',
    participateMoreOften = 'kommen häufiger',
    claimBTP = 'Anspruch BTP',
    benefitBTP = 'nutzen BTP',
    schoolWithoutMT = 'Schule ohne MT',
    schoolExpensiveMT = 'MT Schule zu teuer',
    enoughFood = 'genug Essen',
    qualitySatisfies = 'Qualität zufrieden',
    enoughStaff = 'genug Personal MT', 
    enoughStaffMore = 'genug Personal / weitere Akt.')
    

# rename columns 2013 -----------------------------------------------------

data2013 <- data2013_unbereinigt %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    numberOfKids = 'MT_Kinder',
    numberOfMeals = 'MT_Mahlzeiten',
    finalCosts = 'MT_Gesamtkosten',
    MTGranted = 'Bewilligt MT 2013',
    monthlyCooks = 'Kochen 1x Monat',
    weeklyCooks = 'Kochen 1 Woche',
    shoppers = 'einkaufen',
    cooks = 'zubereiten',
    dietaryKnowledge = 'Wissen erweitert',
    appreciateHealthyDietary = 'schätzen gesunde Ernährung',
    appreciateFoodCulture = 'schätzen gem. Esskultur',
    influenceHomeFoodCulture = 'beeinflussen Esskultur Familien',
    lessIll = 'seltener krank',
    dayToDaySkillsNo = 'erweiterte Alltagskompetenzen',
    selfworthNo = 'Selbstwertgefühl gestärkt',
    participateMoreOften = 'kommen häufiger',
    enoughFood = 'genug Essen',
    qualitySatisfies = 'Qualität zufrieden',
    enoughStaff = 'genug Personal MT', 
    enoughStaffMore = 'genug Personal / weitere Akt.',
    trips = "Entdeckerfonds", 
    tripsNo = 'Aktivitäten 2013', 
    tripsKidsNo = 'Kinder 2013', 
    tripsGranted = 'Bewilligt EF 2013', 
    tripsDecisions = 'entschieden', 
    tripsOrganization = 'organisiert', 
    tripsReview = 'nachbereitet', 
    tripsReached = 'erreicht',
    tripsMobility = 'Mobilität', 
    tripsKnowledge = 'veränderte Kenntnisse', 
    tripsBehavior = 'Verhalten verändetr')


# final data as numeric ---------------------------------------------------

data2012 <- data2012 %>% mutate_if(is.character, as.numeric)
data2013 <- data2013 %>% mutate_if(is.character, as.numeric)


# Add years ---------------------------------------------------------------

data2012 <- data2012 %>% add_column(year=2011)
data2013 <- data2013 %>% add_column(year=2012)




