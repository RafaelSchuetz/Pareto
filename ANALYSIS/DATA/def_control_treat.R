# Analyse des Kontrollgruppenmodells

library(tidyverse)
library(dplyr)
library(ggplot2)
library(estimatr)
library(stats)

### Deskriptive Analyse

# Problem: Die kategorialen Variablen sind im Datentyp "Factor", welche zur weiteren 
# Bearbeitung in den Datentyp "numeric" geändert werden

dfc <- dfc %>% mutate_if(is.factor, as.numeric)


# Erstellung eines Datensatzes für die Kontrollgruppe, in der sich nur Einrichtungen befinden,
# die in jedem Jahr NUR vom Mittagstisch finanziell gefördert werden und in keinem Jahr vom 
# Entdeckerfonds 

data_control <- subset(dfc, dfc$id == "112" | dfc$id == "191" | dfc$id == "213" | dfc$id == "599"
                       | dfc$id == "601" | dfc$id == "602" | dfc$id == "623" | dfc$id == "684"
                       | dfc$id == "685" | dfc$id == "686" | dfc$id == "687")

# Erstellung eines Datensatzes, der nur Einrichtungen enthält, die zwischen Kontroll- und 
# Treatment wechseln

data_skip <- subset(dfc, dfc$id == "131" | dfc$id == "113" | dfc$id == "190" | dfc$id == "282" 
                    | dfc$id == "226" | dfc$id == "141" | dfc$id == "404" | dfc$id == "221")

# Erstellung eines Datensatzes für die Treatmentgruppe, in der sich nur Einrichtungen befinden,
# die in jedem Jahr vom Entdeckerfonds finanziell unterstützt werden

data_treat <- subset(dfc, dfc$id != "112" & dfc$id != "191" & dfc$id != "213" & dfc$id != "599"
                     & dfc$id != "601" & dfc$id != "602" & dfc$id != "623" & dfc$id != "684"
                     & dfc$id != "685" & dfc$id != "686" & dfc$id != "687"
                     & dfc$id != "131" & dfc$id != "113" & dfc$id != "190" & dfc$id != "282" 
                     & dfc$id != "226" & dfc$id != "141" & dfc$id != "404" & dfc$id != "221")
                     