#Datens?tze 2012 und 2013 in R importieren und bearbeiten


# Packages Laden ----------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)


# Datens?tze aus Excel in R ziehen ----------------------------------------

headers2012 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 11, n_max = 0) %>% 
  names()

data2012_unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 11) 

headers2013 <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 10, n_max = 0) %>% 
  names()

data2013_unbereinigt <- read_excel("./ANALYSIS/DATA/CHILDREN Wirkungsdaten_VERTRAULICH_final.xlsx", sheet = 10)



# rename columns ----------------------------------------------------------

data2012 <- data2012_unbereinigt %>% 
  dplyr::rename(
    id = 'Einrichtungsnummer',
    children = 'Kinder'
    totalmeal = 'Mittagsmahlzeiten'
    moneyfinal = 'MT 2012_Fördersumme final'
    

