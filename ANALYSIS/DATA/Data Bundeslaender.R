
# 1. Herunterladen der Daten ----------------------------------------------

library(readxl)
library(dplyr)
library(tidyverse)

# 2019: 73 Beobachtungen (5 Einrichtungen sind im laufenden Jahr neu dazugekommen), 
# 2018: 68 Beobachtungen (stimmt mit der Anzahl der Beobachtungen im Sheet "Stammdaten" überein)
# 2017: 64 Beobachtungen ()

dataBundeslaender_unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx",
                                            sheet = "Stammdaten")


# 2. Bearbeitung der Daten ------------------------------------------------

# Umbennen der Spalten bzw. Variablen

dataBundeslaender <- dataBundeslaender_unbereinigt %>% 
  dplyr::rename(
    id = "Einrichtungsnummer",
    state = "Bundesland",
    supportSince = "Förderung seit…")

