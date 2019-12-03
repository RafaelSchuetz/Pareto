
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

# Join the data with Bundesland

dataWithState <- merge(dataBundeslaender, data201119, all.x = TRUE, all.y = TRUE)

# Einrichtungsnummern, die keinem Bundesland und "Förderung seit" zugeordnet werden können:
# 113 (2011-13), 137 (2011), 190 (2011, 2012), 219 (2011-2016), 226 (2011-13)
# Dabei handelt es sich um Einrichtungen, die aktuell nicht mehr gefördert werden und zu denen 
# somit auch keine Information bezüglich Bundesland und "Förderung seit" vorliegen.