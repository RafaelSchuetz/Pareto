# Analyse des Kontrollgruppenmodells

library(tidyverse)
library(dplyr)
library(ggplot2)
library(estimatr)
library(stats)
library(tidyselect)

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


#neuen Datensatz dfc3 erstellen
#damit wir nur die Ids auswählen können, die tatsächlich eine Konroll oder treatment Gruppe sind

dfc3 <- dfc

#datensatz filtern und nur variablen auswählen die entweder treatment oder kontroll gruppe sind
#alle anderen IDs die treatment Gruppen wechseln rausfiltern

dfc3 <- dfc3 %>% 
  filter(
    #control
    dfc$id == "112" | dfc$id =="191" | dfc$id =="213" | dfc$id == "599"
    | dfc$id == "601" | dfc$id == "602" | dfc$id == "623" | dfc$id == "684"
    | dfc$id == "685" | dfc$id == "686" | dfc$id == "687"| 
    #treatment
    dfc$id == "103" | dfc$id == "104" | dfc$id == "105" | 
      dfc$id == "106" | dfc$id == "108" | dfc$id == "109" | 
      dfc$id == "111" | dfc$id == "114" | dfc$id == "118" | 
      dfc$id == "122" | dfc$id == "123" | dfc$id == "124" |
      dfc$id == "125" | dfc$id == "130" | dfc$id == "132" |
      dfc$id == "133" | dfc$id == "136" | dfc$id == "137" | dfc$id == "139" |
      dfc$id == "142" | dfc$id == "165" | dfc$id == "186" |
      dfc$id == "187" | dfc$id == "188" | dfc$id == "189" |
      dfc$id == "192" | dfc$id == "193" | dfc$id == "194" |
      dfc$id == "209" | dfc$id == "214" | dfc$id == "215" |
      dfc$id == "216" | dfc$id == "217" | dfc$id == "218" |
      dfc$id == "219" | dfc$id == "220" | dfc$id == "233" |
      dfc$id == "249" | dfc$id == "255" | dfc$id == "269" |
      dfc$id == "270" | dfc$id == "281" | dfc$id == "403" |
      dfc$id == "417" | dfc$id == "418" | dfc$id == "437" |
      dfc$id == "482" | dfc$id == "483" | dfc$id == "600" |
      dfc$id == "663" | dfc$id == "664" | dfc$id == "665" |
      dfc$id == "666" | dfc$id == "667"
  )

#erstellen der treatment variable
#ist 1 wenn treatment
#ist 0 wenn control
#treat_diff weil es für denn

dfc3 <- dfc3 %>% 
  rename(
    treatment = 'treat'
  )


dfc3 <- dfc3 %>% 
  mutate(
    treat = case_when(
      #control
      id == "112" ~ 0,
      id =="191" ~ 0,
      id =="213" ~ 0,
      id == "599" ~ 0,
      id == "601" ~ 0,
      id == "602" ~ 0,
      id == "623" ~ 0,
      id == "684" ~ 0,
      id == "685" ~ 0,
      id == "686" ~ 0,
      id == "687"~ 0,
      #treatment
      id == "103"~ 1,
      id == "104"~ 1,
      id == "105"~ 1,
      id == "106"~ 1,
      id == "108"~ 1,
      id == "109"~ 1,
      id == "111"~ 1,
      id == "114"~ 1,
      id == "118"~ 1,
      id == "122"~ 1,
      id == "123"~ 1,
      id == "124"~ 1,
      id == "125"~ 1,
      id == "130"~ 1,
      id == "132"~ 1,
      id == "133"~ 1,
      id == "136"~ 1,
      id == "137"~ 1,
      id == "139"~ 1,
      id == "142"~ 1,
      id == "165"~ 1,
      id == "186"~ 1,
      id == "187"~ 1,
      id == "188"~ 1,
      id == "189"~ 1,
      id == "192"~ 1,
      id == "193"~ 1,
      id == "194"~ 1,
      id == "209"~ 1,
      id == "214"~ 1,
      id == "215"~ 1,
      id == "216"~ 1,
      id == "217"~ 1,
      id == "218"~ 1,
      id == "219"~ 1,
      id == "220"~ 1,
      id == "233"~ 1,
      id == "249"~ 1,
      id == "255"~ 1,
      id == "269"~ 1,
      id == "270"~ 1,
      id == "281"~ 1,
      id == "403"~ 1,
      id == "417"~ 1,
      id == "418"~ 1,
      id == "437"~ 1,
      id == "482"~ 1,
      id == "483"~ 1,
      id == "600"~ 1, 
      id == "663"~ 1,
      id == "664"~ 1,
      id == "665"~ 1,
      id == "666"~ 1,
      id == "667"~ 1
      
    )
    
  )

#erstellen der Post variable

dfc3 <- dfc3 %>% 
  mutate(
    post = if_else(dfc3$year == '2011', 0, 1)
  )

# Diff in Diff Analyse

#treat ist der Treatment Dummy
#post ist der Time Dummy

#erstellen des Interaktionstermes

dfc3 <- dfc3 %>% 
  mutate(
    did = post*treat
  )

didreg = lm(dayToDaySkills ~ treat + post + did, data = dfc3)
summary(didreg)


#Treatment Intensity Analyse #####










