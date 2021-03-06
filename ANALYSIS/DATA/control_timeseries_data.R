# Analyse des Kontrollgruppenmodells anhand deskriptiver Analysen
# Erstellung des Datensatzes für Zeitreihen zum vergleichen von Treatment und Kontrollgruppe

library(tidyverse)
library(dplyr)
library(ggplot2)
library(estimatr)
library(stats)

### Deskriptive Analyse

# Problem: Die kategorialen Variablen sind im Datentyp "Factor", welche zur weiteren 
# Bearbeitung in den Datentyp "numeric" geändert werden

dfc <- dfc %>% mutate_if(is.factor, as.numeric)

### Deskriptive Analyse

## 1. Schritt: Mittelwert einer Variable (z.B. "Selbstwertgefühl") für Treatment- und Kontroll-
# gruppe für jedes Jahr erstellen

# Erstellung von Datensätzen, in denen jeweils nur die Beobachtungen aus der Kontroll- oder
# Treatmentgruppe eines bestimmten Jahres aufgeführt werden, für jedes Jahr von 2012 bis 2018

dfc_2012 <- subset(dfc, dfc$dummy_2012 == "1")
dfc_2012_treat <- subset(dfc_2012, dfc_2012$treat_2012 == "1")
dfc_2012_control <- subset(dfc_2012, dfc_2012$treat_2012 == "0")

dfc_2013 <- subset(dfc, dfc$dummy_2013 == "1")
dfc_2013_treat <- subset(dfc_2013, dfc_2013$treat_2013 == "1")
dfc_2013_control <- subset(dfc_2013, dfc_2013$treat_2013 == "0")

dfc_2014 <- subset(dfc, dfc$dummy_2014 == "1")
dfc_2014_treat <- subset(dfc_2014, dfc_2014$treat_2014 == "1")
dfc_2014_control <- subset(dfc_2014, dfc_2014$treat_2014 == "0")

dfc_2015 <- subset(dfc, dfc$dummy_2015 == "1")
dfc_2015_treat <- subset(dfc_2015, dfc_2015$treat_2015 == "1")
dfc_2015_control <- subset(dfc_2015, dfc_2015$treat_2015 == "0")

dfc_2016 <- subset(dfc, dfc$dummy_2016 == "1")
dfc_2016_treat <- subset(dfc_2016, dfc_2016$treat_2016 == "1")
dfc_2016_control <- subset(dfc_2016, dfc_2016$treat_2016 == "0")

dfc_2017 <- subset(dfc, dfc$dummy_2017 == "1")
dfc_2017_treat <- subset(dfc_2017, dfc_2017$treat_2017 == "1")
dfc_2017_control <- subset(dfc_2017, dfc_2017$treat_2017 == "0")

dfc_2018 <- subset(dfc, dfc$dummy_2018 == "1")
dfc_2018_treat <- subset(dfc_2018, dfc_2018$treat_2018 == "1")
dfc_2018_control <- subset(dfc_2018, dfc_2018$treat_2018 == "0")

#Folgende Schritte müssen nicht zwingend ausgeführt werden - NA's werden bei den Durchschnitten nicht berücksichtigt:

# Problem: Die Datensätze der Kontrollgruppen enthalten Beobachtungen, zu denen auch keine Daten
# für den Mittagstisch vorliegen. Diese Beobachtungen müssen aus den Kontrollgruppen entfernt
# werden. 

# 2012 in Ordnung

# 2013: Beobachtungen mit der ID-Nummer 404, 418, 437 (die sich in den Zeilen 8, 9 und 10
# befinden) müssen entfernt werden
#drops <- c(8, 9, 10)
#dfc_2013_control <- dfc_2013_control[-drops,]

# 2014: Beobachtungen mit der ID-Nummer 482, 483 müssen entfernt werden
#drops <- c(7, 8)
#dfc_2014_control <- dfc_2014_control[-drops,]

# 2015: in Ordnung

# 2016: Beobachtungen mit der ID-Nummer 599, 600, 602 müssen entfernt werden
#drops <- c(7, 8, 10)
#dfc_2016_control <- dfc_2016_control[-drops,]

# 2017: Beobachtungen mit der ID-Nummer 600, 623, 663 - 667 müssen entfernt werden
#drops <- c(8, 11, 12, 13, 14, 15, 16)
#dfc_2017_control <- dfc_2017_control[-drops,]

# 2018: Beobachtungen mit der ID-Nummer 663 - 667 müssen entfernt werden
#drops <- c(10, 11, 12, 13, 14)
#dfc_2018_control <- dfc_2018_control[-drops,]



