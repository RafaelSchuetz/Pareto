### Erstellung der Dummy-Variable für den Treatment-Status ###

library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyselect)

dfcEF <- dfc

# Grundlage für die Erstellung der Treatment-Variable ist der Datensatz dfc (aus dem R-Skript
# control_gruppe.R). Der Datensatz enthält zum einen Dummy-Variablen für das Jahr, welche angeben,
# ob eine Einrichtung i im Jahr t beobachtet wurde (dummy_year = 1) oder nicht (dummy_year = 0).
# Diese Variablen sind für year-fixed effects und zeit-variante Treatment-Effekte relevant. 
# Zum anderen enthält der Datensatz Dummy-Variablen für den Treatment-Status für jedes Jahr,
# die angeben, ob eine eine Einrichtung i im Jahr t am Entdeckerfonds teilgenommen hat und 
# damit in der Treatment-Gruppe war (treat_year = 1) oder nicht (treat_year = 0; Kontrollgruppe).


# Erstellung eines neuen Datensatzes, der nur die Treatment-Dummies für jedes Jahr enthält

dfcEF_treat_dummies <- dfcEF[ ,c("treat_2011", "treat_2012", "treat_2013", "treat_2014",
                             "treat_2015", "treat_2016", "treat_2017", "treat_2018")]

# Problem: Die Treatment-Dummies für jedes Jahr liegen im Datentyp "character" vor.
# Umwandeln des Datentyps in "nummeric", um die Summe bilden zu können

dfcEF_treat_dummies <- data.frame(lapply(dfcEF_treat_dummies, function(x) as.numeric(as.character(x))))

# Erstellen eines allgemeinen Treatment-Dummies, der angibt, ob eine Beobachtungseinheit 
# (Einrichtung i im Jahr t) in der Treatmentgruppe ist oder nicht. Die Dummy-Variable "TreatEF"
# ist gleich 1, wenn eine Einrichtung i im betrachteten Jahr am Entdeckerfonds teilgenommen hat
# (= Treatment-Gruppe), und sonst 0 (= Kontrollgruppe). Die Dummy-Variable "TreatEF" wird mit i
# für die Einrichtung und t für das Jahr indexiert.

# Dazu wird für jede Zeile die Summe über alle Spalten gebildet, welche die Treatment-Dummies 
# für jedes Jahr enthalten (z.B. treat_2011, ..., treat_2018). Da eine Zeile eine Einrichtung i
# im Jahr t enthält (= eine Beobachtungseinheit), dürfte maximal eine jahr-spezifischer
# Treatment-Dummy den Wert 1 annehmen.

dfcEF_treat_dummies$rowSums <- rowSums(dfcEF_treat_dummies, na.rm = FALSE, dims = 1L) 

# Der allgemeine Treatment-Dummy ist gleich 1, wenn innerhalb einer Zeile die Summe über alle
# Spalten hinweg größer ist als 0 (= Kontrollgruppe). Analog: Der allgemeine Treatment-Dummy ist
# gleich 0, wenn die Summe der Spalten gleich 0.

dfcEF_treat_dummies$treatEF <- ifelse(dfcEF_treat_dummies$rowSums > 0, '1', '0')

# Problem: Der allgemeine Treatment-Dummy liegt im Datentyp "character" vor.
# Daher wird der Datentyp aller Variablen im Datensatz nochmals zu "numeric" umgewandelt.

dfcEF_treat_dummies <- data.frame(lapply(dfcEF_treat_dummies, function(x) as.numeric(as.character(x))))

# Hinzufügen der allgemeinen Treatment-Variable zum ursprünglichen Datensatz dfcEF

dfcEF$treatEF <- dfcEF_treat_dummies$treatEF 

# Wichtig: Der Datensatz enthält immer noch Beobachtungen, bei denen auch keine Daten
# zum Mittagstisch vorliegen.

# Plausibilisierung 

dfcEF_plausi <- subset(dfcEF, treatEF=="0")

dfcEF_plausi <- subset(dfcEF_plausi, year=="2015")