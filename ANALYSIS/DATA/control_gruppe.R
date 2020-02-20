#Analyse mit der Kontrollgruppe Entdeckerfonds

#data frame control
dfc <- mergedData

library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyselect)

#dataset dfc2 wählt nur die Variablen mit trips aus
dfc2 <- dfc
dfc2 <- dfc2 %>% 
  dplyr::select(starts_with('trips') & !(contains('scaled'))) %>% 
  select_if(is.numeric)

#alle die NA sind einfach zu 0 umwandeln, dass ich die Summe bilden kann & somit den treatment
#dummy erstellen kann
dfc2[is.na(dfc2)] <- 0

#Problem:
#invalid factor level, NA generated
#nicht alle VAriablen haben die gleichen levels (Kategorien)

#deshalb muss ich zu den levels die 0 als Faktor level hinzufügen
#dann kann ich das umändern

levels(dfc2$tripsSuggestions) <- c(levels(dfc2$tripsSuggestions), '0')
dfc2$tripsSuggestions[is.na(dfc2$tripsSuggestions)] <- 0

levels(dfc2$tripsNewIdeas) <- c(levels(dfc2$tripsNewIdeas), '0')
dfc2$tripsNewIdeas[is.na(dfc2$tripsNewIdeas)] <- 0

levels(dfc2$tripsAdditionalActivities) <- c(levels(dfc2$tripsAdditionalActivities), '0')
dfc2$tripsAdditionalActivities[is.na(dfc2$tripsAdditionalActivities)] <- 0

levels(dfc2$tripsDayToDaySkills) <- c(levels(dfc2$tripsDayToDaySkills), '0')
dfc2$tripsDayToDaySkills[is.na(dfc2$tripsDayToDaySkills)] <- 0

levels(dfc2$tripsSuccess) <- c(levels(dfc2$tripsSuccess), '0')
dfc2$tripsSuccess[is.na(dfc2$tripsSuccess)] <- 0

levels(dfc2$tripsSelfEfficacy) <- c(levels(dfc2$tripsSelfEfficacy), '0')
dfc2$tripsSelfEfficacy[is.na(dfc2$tripsSelfEfficacy)] <- 0

levels(dfc2$tripsSelfworth) <- c(levels(dfc2$tripsSelfworth), '0')
dfc2$tripsSelfworth[is.na(dfc2$tripsSelfworth)] <- 0

levels(dfc2$tripsSocialSkills) <- c(levels(dfc2$tripsSocialSkills), '0')
dfc2$tripsSocialSkills[is.na(dfc2$tripsSocialSkills)] <- 0

levels(dfc2$tripsFrustrationTolerance) <- c(levels(dfc2$tripsFrustrationTolerance), '0')
dfc2$tripsFrustrationTolerance[is.na(dfc2$tripsFrustrationTolerance)] <- 0

levels(dfc2$tripsCHILDRENSuggestions) <- c(levels(dfc2$tripsCHILDRENSuggestions), '0')
dfc2$tripsCHILDRENSuggestions[is.na(dfc2$tripsCHILDRENSuggestions)] <- 0

levels(dfc2$tripsReached) <- c(levels(dfc2$tripsReached), '0')
dfc2$tripsReached[is.na(dfc2$tripsReached)] <- 0

levels(dfc2$tripsKnowledge) <- c(levels(dfc2$tripsKnowledge), '0')
dfc2$tripsKnowledge[is.na(dfc2$tripsKnowledge)] <- 0

#jetzt hab ich einen datensatz wo alle Variablen die davor NA waren 0 sind
#nur wichtig um die Treatment Variable zu erstellen

#alle variablen numerisch machen, damit ich die summe bilden kann
#umwandeln in numerisch passiert nur in dem Datensatz dfc2
dfc2 <- data.frame(lapply(dfc2, function(x) as.numeric(as.character(x))))

#Summe der Rows bilden, wenn das Ergebnis 0 ist, wurden keine Fragen beantwortet
#somit ist die Einrichtung in der Kontroll Gruppe
#es sind teilweise sehr große Zahlen, weil die Fördersumme inkludiert ist
dfc2$treatment <- rowSums(dfc2, na.rm = FALSE, dims = 1L)

#generiere die treatment variable die 1 ist wenn treatment und 0 wenn control
dfc2$treat <- ifelse(dfc2$treatment > 1, '1', '0')

#hinzufügen der treat variable zu dem dfc datensatz
#der dfc datensatz ist einfach eine Kopie des Datensatzes mergedData
dfc$treat <- dfc2$treat

#2011
dfc$dummy_2011 <- ifelse(dfc$year == 2011, '1', '0')
dfc$treat_2011 <- ifelse(dfc$dummy_2011 == 1 & dfc$treat == 1, '1', '0')

#2012
dfc$dummy_2012 <- ifelse(dfc$year == 2012, '1', '0')
dfc$treat_2012 <- ifelse(dfc$dummy_2012 == 1 & dfc$treat == 1, '1', '0')

#2013
dfc$dummy_2013 <- ifelse(dfc$year == 2013, '1', '0')
dfc$treat_2013 <- ifelse(dfc$dummy_2013 == 1 & dfc$treat == 1, '1', '0')

#2014
dfc$dummy_2014 <- ifelse(dfc$year == 2014, '1', '0')
dfc$treat_2014 <- ifelse(dfc$dummy_2014 == 1 & dfc$treat == 1, '1', '0')

#2015
dfc$dummy_2015 <- ifelse(dfc$year == 2015, '1', '0')
dfc$treat_2015 <- ifelse(dfc$dummy_2015 == 1 & dfc$treat == 1, '1', '0')

#2016
dfc$dummy_2016 <- ifelse(dfc$year == 2016, '1', '0')
dfc$treat_2016 <- ifelse(dfc$dummy_2016 == 1 & dfc$treat == 1, '1', '0')

#2017
dfc$dummy_2017 <- ifelse(dfc$year == 2017, '1', '0')
dfc$treat_2017 <- ifelse(dfc$dummy_2017 == 1 & dfc$treat == 1, '1', '0')

#2018
dfc$dummy_2018 <- ifelse(dfc$year == 2018, '1', '0')
dfc$treat_2018 <- ifelse(dfc$dummy_2018 == 1 & dfc$treat == 1, '1', '0')





