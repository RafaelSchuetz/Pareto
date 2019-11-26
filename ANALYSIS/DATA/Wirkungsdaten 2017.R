# PaRE3To-Projekt

# 1. Herunterladen der Daten ----------------------------------------------

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
    overallchildrenperinstitution = 'Anzahl KiJu insgesamt in der Einrichtung',
    age = 'Alter der KiJu',
    overallbudget = 'Gesamtbudget der Einrichtung',
    chilrdenpermeal = 'Anzahl Ki pro Mahlzeit 2017',
    newchildren = 'neue Ki beim MT 2017',
    number_catering = 'Anzahl Catering 2017',
    number_mealwithininstitution = 'Anzahl idEg 2017',
    number_breakfast = 'Anzahl Frü 2017',
    number_MT = 'Anzahl MT 2017',
    number_afternoon = 'Anzahl Nachmi 2017',
    number_dinner = 'Anzahl AbBr 2017',
    frequency = 'Häufigkeit 2017(pro Woche x Wochen pro Jahr)',
    DGE = 'Anzahl DGE-Kriterien',
    finalcosts = 'MT_Gesamtkosten 2017',
    MT_granted = 'Bewilligt MT 2017',
    )







# 3. Analyse der Daten ----------------------------------------------------

library(ggplot2)

# Histogramm

hist(Wirkungsdaten_2017$`Anzahl KiJu insgesamt in der Einrichtung`)

# Scatterplot

plot(x = Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`, 
     y = Wirkungsdaten_2017$`Anzahl Ki pro Mahlzeit 2017`)

# Statistische Zusammenfassung

summary(Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`)
