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

# Umbennen der Spalten bzw. Variablen

data2017 <- Wirkungsdaten_2017unbereinigt %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    overallchildrenperinstitution = 'Anzahl KiJu insgesamt in der Einrichtung',
    age = 'Alter der KiJu',
    overallbudget = 'Gesamtbudget der Einrichtung')


# Änderung des Datentyps
as.numeric(Wirkungsdaten_2017$`Anzahl KiJu insgesamt in der Einrichtung`)



# 3. Analyse der Daten ----------------------------------------------------

library(ggplot2)

# Histogramm

hist(Wirkungsdaten_2017$`Anzahl KiJu insgesamt in der Einrichtung`)

# Scatterplot

plot(x = Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`, 
     y = Wirkungsdaten_2017$`Anzahl Ki pro Mahlzeit 2017`)

# Statistische Zusammenfassung

summary(Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`)
