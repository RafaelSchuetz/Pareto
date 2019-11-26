# PaRE3To-Projekt

# 1. Herunterladen der Daten ----------------------------------------------

install.packages("readxl")
library(readxl)

### Daten 2017

Wirkungsdaten_2017 <- read_excel("Daten CHILDREN/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                 sheet = "2017")
View(Wirkungsdaten_2017)

# Löschen von Spalten ohne Inhalt

Wirkungsdaten_2017$...17 <- NULL
Wirkungsdaten_2017$Entdeckerfonds <- NULL


### Daten 2016
Wirkungsdaten_2016 <- read_excel("Daten CHILDREN/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                                 sheet = "2016")
View(Wirkungsdaten_2016)

# Löschen von Spalten ohne Inhalt

Wirkungsdaten_2016$Entdeckerfonds <- NULL


# 2. Analyse der Daten ----------------------------------------------------

library(dplyr)
library(ggplot2)

# Histogramm

hist(Wirkungsdaten_2017$`Anzahl KiJu insgesamt in der Einrichtung`)

hist(Wirkungsdaten_2016$`Anzahl Kinder pro Mahlzeit 2016`)

# Scatterplot

plot(x = Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`, 
     y = Wirkungsdaten_2017$`Anzahl Ki pro Mahlzeit 2017`)

# Statistische Zusammenfassung

summary(Wirkungsdaten_2017$`Gesamtbudget der Einrichtung`)
