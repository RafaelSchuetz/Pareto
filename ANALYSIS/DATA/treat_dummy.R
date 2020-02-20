### Erstellung der Dummy-Variable für den Treatment-Status ###

library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyselect)

# Grundlage für die Erstellung der Treatment-Variable ist der Datensatz dfc (aus dem R-Skript
# control_gruppe.R). Der Datensatz enthält zum einen Dummy-Variablen für das Jahr, welche angeben,
# ob eine Einrichtung i im Jahr t beobachtet wurde (dummy_year = 1) oder nicht (dummy_year = 0).
# Diese Variablen sind für year-fixed effects und zeit-variante Treatment-Effekte relevant. 
# Zum anderen enthält der Datensatz Dummy-Variablen für den Treatment-Status für jedes Jahr,
# die angeben, ob eine eine Einrichtung i im Jahr t am Entdeckerfonds teilgenommen hat und 
# damit in der Treatment-Gruppe war (treat_year = 1) oder nicht (treat_year = 0; Kontrollgruppe).


# Erstellung eines neuen Datensatzes, der nur die Treatment-Dummies für jedes Jahr enthält

dfc_treat_dummies <- dfc[ ,c("treat_2011", "treat_2012", "treat_2013", "treat_2014",
                             "treat_2015", "treat_2016", "treat_2017", "treat_2018")]

# Problem: Die Treatment-Dummies für jedes Jahr liegen im Datentyp "character" vor.
# Umwandeln des Datentyps in "nummeric", um die Summe bilden zu können

dfc_treat_dummies <- data.frame(lapply(dfc_treat_dummies, function(x) as.numeric(as.character(x))))

# Erstellen eines allgemeinen Treatment-Dummies, der angibt, ob eine Beobachtungseinheit 
# (Einrichtung i im Jahr t) in der Treatmentgruppe ist oder nicht. Die Dummy-Variable "TreatEF"
# ist gleich 1, wenn eine Einrichtung i im betrachteten Jahr am Entdeckerfonds teilgenommen hat
# (= Treatment-Gruppe), und sonst 0 (= Kontrollgruppe). Die Dummy-Variable "TreatEF" wird mit i
# für die Einrichtung und t für das Jahr indexiert.

# Dazu wird für jede Zeile die Summe über alle Spalten gebildet, welche die Treatment-Dummies 
# für jedes Jahr enthalten (z.B. treat_2011, ..., treat_2018). Da eine Zeile eine Einrichtung i
# im Jahr t enthält (= eine Beobachtungseinheit), dürfte maximal eine jahr-spezifischer
# Treatment-Dummy den Wert 1 annehmen.

dfc_treat_dummies$rowSums <- rowSums(dfc_treat_dummies, na.rm = FALSE, dims = 1L) 

# Der allgemeine Treatment-Dummy ist gleich 1, wenn innerhalb einer Zeile die Summe über alle
# Spalten hinweg größer ist als 0 (= Kontrollgruppe). Analog: Der allgemeine Treatment-Dummy ist
# gleich 0, wenn die Summe der Spalten gleich 0.

dfc_treat_dummies$treatEF <- ifelse(dfc_treat_dummies$rowSums > 0, '1', '0')

# Hinzufügen der allgemeinen Treatment-Variable zum ursprünglichen Datensatz dfc

dfc$treatEF <- dfc_treat_dummies$treatEF 






