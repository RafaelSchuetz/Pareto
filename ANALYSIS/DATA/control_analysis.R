# Analyse des Kontrollgruppenmodells

library(tidyverse)
library(dplyr)
library(ggplot2)
library(estimatr)
library(stats)

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

# Problem: Die Datensätze der Kontrollgruppen enthalten Beobachtungen, zu denen auch keine Daten
# für den Mittagstisch vorliegen. Diese Beobachtungen müssen aus den Kontrollgruppen entfernt
# werden. 


  
# Erstellung 

  
dfc_                      
         

mean_by_year <- dfc %>%
  group_by(df$year) %>% 
  group_by(df$treat) %>%
  summarise(averagedSelfworth = mean(df$selfworth), na.rm = TRUE))             
                      
