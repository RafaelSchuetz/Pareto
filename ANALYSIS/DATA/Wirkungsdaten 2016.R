# PaRE3To-Projekt

# 1. Herunterladen der Daten ----------------------------------------------

library(readxl)
library(dplyr)
library(tidyverse)

### Daten 2016

Wirkungsdaten_2016 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                                 sheet = "2016")
View(Wirkungsdaten_2016)

# Löschen von Spalten ohne Inhalt

Wirkungsdaten_2016$Entdeckerfonds <- NULL


# 2. Bearbeitung der Daten ------------------------------------------------




# 3. Analyse der Daten ----------------------------------------------------

library(ggplot2)

# Histogramm

hist(Wirkungsdaten_2016$`Anzahl Kinder pro Mahlzeit 2016`)