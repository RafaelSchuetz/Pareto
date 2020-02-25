### DiD-Schätzungen

# Laden der benötigten Pakete
library(estimatr)
library(robustbase)
library(tidyverse)
library(sandwich)
library(lmtest)
library(broom)
library(zoo)
library(dplyr)
library(stargazer)



### Zeit-variabler Treatment-Dummy ### ----------------------------------------

### Variante 1: Regression für DiD-Estimation ###

### a) dayToDaySkills

# Als Zielvariable (= y-Variable) wird "dayToDaySkills" in skalierter Form verwendet
# Der Treatment-Dummy "TreatEF" (= variable of interest) ist gleich 1, wenn Einrichtung i im Jahr t
# am Entdeckerfonds teilnimmt und sich somit in der Treatmentgruppe befindet; "TreatEF" ist gleich 0, wenn 
# Einrichtung i im Jahr t nicht am Entdeckerfonds teilnimmt und somit in der Kontrollgruppe ist
# Die Variable "id" und "year" werden im Datentyp "factor" codiert, sodass die Funktion lm() automatisch
# Dummy-Variablen für jedes Jahr und jede Einrichtung erstellt und diese in die Regression mitaufnimmt.
# Somit werden id-fixed effects und year-fixed effects für die DiD-Estimation implementiert. 


# Änderung des Datentyps
dfcEF$id <- as.factor(dfcEF$id)
dfcEF$year <- as.factor(dfcEF$year)
dfcEF$treatEF <- as.numeric(dfcEF$treatEF)

# Für die oben beschriebene Regression wird der Befehl lm() verwendet, wobei lm() zunächst normale
# Standardfehler berechnet. Für die erste Regression werden keine Kontrollvariablen verwendet.
# Für die zweite Regression werden dann Einrichtungs-spezifische Kontrollvariablen implementiert:
# Subsidy und Total Cost 

# Ohne Kontrollvariablen
lmdid_dayToDaySkills <- lm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year)
summary(lmdid_dayToDaySkills)

# Mit Kontrollvariablen
# Problem: Die Kontrollvariablen "dfcEF$subsidy" und "dfcEF$totalCost" sind in Euro-Einheiten angegeben,
# sodass der Regressionskoeffizient der beiden Variablen erst an der 5. - 7. Nachkommastelle eine Zahl
# enthält und im Regressiontable somit ein Regressionskoeffizient von 0 ausgewiesen wird.
# Lösung: Die Kontrollvariablen "dfcEF$subsidy" und "dfcEF$totalCost" werden durch 1000 geteilt. 

# dfcEF$subsidy <- (dfcEF$subsidy/1000)
# dfcEF$totalCost <- (dfcEF$totalCost/1000)

lmdid_dayToDaySkills_controls <- lm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year
                                    + dfcEF$subsidy + dfcEF$totalCost)
summary(lmdid_dayToDaySkills_controls)


# Berechnung von robusten Standardfehlern, indem eine Heteroskedastität-konsistente Varianz-Kovarianz-Matrix
# für die geschätzten Regressionskoeffizienten generiert wird. Die robusten Standardfehler der geschätzten
# Regressionskoeffizienten werden entsprechend gespeichert
lmdid_dayToDaySkills <- coeftest(lmdid_dayToDaySkills, vcov. = vcovHC(lmdid_dayToDaySkills, type = 'HC1'))
summary(lmdid_dayToDaySkills)

lmdid_dayToDaySkills_controls <- coeftest(lmdid_dayToDaySkills_controls, 
                                          vcov. = vcovHC(lmdid_dayToDaySkills_controls, type = 'HC1'))
summary(lmdid_dayToDaySkills_controls)


# Erstellen des Regression-Tables mit der Funktion stargazer()
table_did_dayToDaySkills <- stargazer(lmdid_dayToDaySkills,
                                      omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                               'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                               'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                               'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                               'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                               'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                               'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                               'year2018'),
                                      add.lines = list(c('ID fixed effects', 'Yes'),
                                                       c('Year fixed effects', 'Yes')),
                                      type = 'text')

table_did_dayToDaySkills_controls <- stargazer(lmdid_dayToDaySkills_controls,
                                      omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                               'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                               'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                               'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                               'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                               'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                               'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                               'year2018'),
                                      add.lines = list(c('ID fixed effects', 'Yes'),
                                                       c('Year fixed effects', 'Yes')),
                                      type = 'text')


# Regressionstables als RDS speichern
saveRDS(lmdid_dayToDaySkills, file = "./ANALYSIS/Tables/lmdid_dayToDaySkills.Rds")
saveRDS(lmdid_dayToDaySkills_controls, file = "./ANALYSIS/Tables/lmdid_dayToDaySkills_controls.Rds")



### b) Selfworth:

# Ohne Kontrollvariablen
lmdid_selfworth <- lm(dfcEF$selfworth_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year)
summary(lmdid_selfworth)

# Mit Kontrollvariablen
lmdid_selfworth_controls <- lm(dfcEF$selfworth_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year
                               + dfcEF$subsidy + dfcEF$totalCost)
summary(lmdid_selfworth_controls)


# Berechnung von robusten Standardfehlern, indem eine Heteroskedastität-konsistente Varianz-Kovarianz-Matrix
# für die geschätzten Regressionskoeffizienten generiert wird. Die robusten Standardfehler der geschätzten
# Regressionskoeffizienten werden wird unter "lmdid" gespeichert
lmdid_selfworth <- coeftest(lmdid_selfworth, vcov. = vcovHC(lmdid_selfworth, type = 'HC1'))
summary(lmdid_selfworth)

lmdid_selfworth_controls <- coeftest(lmdid_selfworth_controls, 
                                     vcov. = vcovHC(lmdid_selfworth_controls, type = 'HC1'))
summary(lmdid_selfworth_controls)

# Erstellen des Regression-Tables mit der Funktion stargazer()
table_did_selfworth <- stargazer(lmdid_selfworth,
                                      omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                               'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                               'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                               'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                               'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                               'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                               'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                               'year2018'),
                                      add.lines = list(c('ID fixed effects', 'Yes'),
                                                       c('Year fixed effects', 'Yes')),
                                      type = 'text')

table_did_selfworth_controls <- stargazer(lmdid_selfworth_controls,
                                 omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                          'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                          'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                          'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                          'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                          'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                          'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                          'year2018'),
                                 add.lines = list(c('ID fixed effects', 'Yes'),
                                                  c('Year fixed effects', 'Yes')),
                                 type = 'text')

# Regressionstables als RDS speichern
saveRDS(lmdid_selfworth, file = "./ANALYSIS/Tables/lmdid_selfworth.rds")
saveRDS(lmdid_selfworth_controls, file = "./ANALYSIS/Tables/lmdid_selfworth_controls.rds")




### Variante 2: Regression für die DiD-Estimation mit year-specific treatment effects ###

# Um für die DiD-Estimation für jedes einzelne Jahr einen Treatment-Effekt zu berechnen zu können
# (= year-specific treatment effects), müssen Interaktionsterme erstellt werden 

# Dazu müssen die bereits bestehenden Variablen treat_2011, treat_2012, treat_2013, treat_2014, treat_2015,
# treat_2016, treat_2017, treat_2018 gelöscht werden
dfcEF$treat_2011 <- NULL
dfcEF$treat_2012 <- NULL
dfcEF$treat_2013 <- NULL
dfcEF$treat_2014 <- NULL
dfcEF$treat_2015 <- NULL
dfcEF$treat_2016 <- NULL
dfcEF$treat_2017 <- NULL
dfcEF$treat_2018 <- NULL

# Erstellen eines Interaktionsterms für jedes Jahr, indem der Treatment-Dummy "TreatEF"
# mit den Dummy-Variablen für jedes Jahr multipliziert wird (z.B. treat_2012 = treatEF * dummy_2012)
dfcEF <- dfcEF %>% 
  mutate(
    treat_2011 = treatEF*dummy_2011,
    treat_2012 = treatEF*dummy_2012,
    treat_2013 = treatEF*dummy_2013,
    treat_2014 = treatEF*dummy_2014,
    treat_2015 = treatEF*dummy_2015,
    treat_2016 = treatEF*dummy_2016,
    treat_2017 = treatEF*dummy_2017,
    treat_2018 = treatEF*dummy_2018
  )


### a) dayToDaySkills

# Als Zielvariable (= y-Variable) wird wieder "dayToDaySkills" in skalierter Form verwendet.
# Statt dem einfachen Treatment-Dummy "TreatEF", werden nun die verschiedenen Interaktionsterme 
# (TreatEF * Year) in die Regressionsgleichung aufgenommen. # Die Regression enthält außerdem wieder
# id-fixed effects und year-fixed effects, in dem die Variablen "id" und "year" im Datentyp "factor" in die
# Regressionsgleichung aufgenommen werden.

lmdid2_dayToDaySkills <- lm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treat_2011 + dfcEF$treat_2012 + dfcEF$treat_2013
                            + dfcEF$treat_2014 + dfcEF$treat_2015 + dfcEF$treat_2016
                            + dfcEF$treat_2017 + dfcEF$treat_2018 + dfcEF$id + dfcEF$year)
summary(lmdid2_dayToDaySkills)

# Berechnung von robusten Standardfehlern, indem eine Heteroskedastität-konsistente Varianz-Kovarianz-Matrix
# für die geschätzten Regressionskoeffizienten generiert wird. Die robusten Standardfehler der geschätzten
# Regressionskoeffizienten werden wird unter "lmdid" gespeichert
lmdid2_dayToDaySkills <- coeftest(lmdid2_dayToDaySkills, vcov. = vcovHC(lmdid2_dayToDaySkills, type = 'HC1'))
summary(lmdid2_dayToDaySkills)

# Erstellen des Regression-Tables mit der Funktion stargazer()
table_did2_dayToDaySkills <- stargazer(lmdid2_dayToDaySkills,
                                       omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                                'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                                'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                                'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                                'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                                'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                                'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                                'year2018'),
                                       add.lines = list(c('ID fixed effects', 'Yes'),
                                                        c('Year fixed effects', 'Yes')),
                                       type = 'text')

# Regressionstables als RDS speichern
saveRDS(lmdid2_dayToDaySkills, file = "./ANALYSIS/Tables/lmdid2_dayToDaySkills.Rds")



### b) Selfworth:

lmdid2_selfworth <- lm(dfcEF$selfworth_scaled ~ dfcEF$treat_2011 + dfcEF$treat_2012 + dfcEF$treat_2013
                       + dfcEF$treat_2014 + dfcEF$treat_2015 + dfcEF$treat_2016
                       + dfcEF$treat_2017 + dfcEF$treat_2018 + dfcEF$id + dfcEF$year)
summary(lmdid2_selfworth)

# Berechnung von robusten Standardfehlern, indem eine Heteroskedastität-konsistente Varianz-Kovarianz-Matrix
# für die geschätzten Regressionskoeffizienten generiert wird. Die robusten Standardfehler der geschätzten
# Regressionskoeffizienten werden wird unter "lmdid" gespeichert
lmdid2_selfworth <- coeftest(lmdid2_selfworth, vcov. = vcovHC(lmdid2_selfworth, type = 'HC1'))
summary(lmdid2_selfworth)

# Erstellen des Regression-Tables mit der Funktion stargazer()
table_did2_selfworth <- stargazer(lmdid2_selfworth,
                                  omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                           'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                           'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                           'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                           'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                           'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                           'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                           'year2018'),
                                  add.lines = list(c('ID fixed effects', 'Yes'),
                                                   c('Year fixed effects', 'Yes')),
                                  type = 'text')


# Regressionstables als RDS speichern
saveRDS(lmdid2_selfworth, file = "./ANALYSIS/Tables/lmdid2_selfworth.Rds")




### Einmal Treatment, immer Treatment ------------------------------------

dfcEF2 <- dfcEF

# Neue Definition der Dummy-Variable für das Treatment
# Sobald eine Einrichtung i das Treatment erhält, ist der Treatment-Dummy für die folgenden Perioden 
# immer gleich 1 (= Einmal Treatment, immer Treatment)

# Identifizieren aller Einrichtungen, bei denen der Treatment-Status von TreatEF = 1 auf TreatEF = 0 wechselt

dfcEF_check <- dfcEF2

dfcEF_check <- dfcEF_check %>% 
  dplyr :: select(id, treatEF)

# Identifizierte Einrichtungen mit folgenden ID's: 113, 141, 191, 221, 226 und 282

# Sobald eine Einrichtung das Treatment erhält, gilt TreatEF = 1 für alle Folgejahre
# Für die identifizierten Einrichtungen wird der Treatment-Status von TreatEF = 0 auf TreatEF = 1 für alle
# Jahre ab dem ersten Treatment geändert.

dfcEF2 [67, "treatEF"] <- 1
dfcEF2 [168, "treatEF"] <- 1
dfcEF2 [169, "treatEF"] <- 1
dfcEF2 [225, "treatEF"] <- 1
dfcEF2 [226, "treatEF"] <- 1
dfcEF2 [227, "treatEF"] <- 1
dfcEF2 [228, "treatEF"] <- 1
dfcEF2 [229, "treatEF"] <- 1
dfcEF2 [230, "treatEF"] <- 1
dfcEF2 [330, "treatEF"] <- 1
dfcEF2 [331, "treatEF"] <- 1
dfcEF2 [335, "treatEF"] <- 1
dfcEF2 [389, "treatEF"] <- 1
dfcEF2 [390, "treatEF"] <- 1
dfcEF2 [391, "treatEF"] <- 1



### Regression für DiD-Estimation ###

### WICHTIG: Jetzt immer Datensatz dfcEF2 verwenden mit dem modifizierten Treatment-Dummy 

### a) dayToDaySkills

# Als Zielvariable (= y-Variable) wird "dayToDaySkills" in skalierter Form verwendet
# Die Variable "id" und "year" werden im Datentyp "factor" codiert, sodass die Funktion lm() automatisch
# Dummy-Variablen für jedes Jahr und jede Einrichtung erstellt und diese in die Regression mitaufnimmt.
# Somit werden id-fixed effects und year-fixed effects für die DiD-Estimation implementiert. 

# Änderung des Datentyps
dfcEF2$id <- as.factor(dfcEF$id)
dfcEF2$year <- as.factor(dfcEF2$year)
dfcEF2$treatEF <- as.numeric(dfcEF2$treatEF)

# Für die oben beschriebene Regression wird der Befehl lm() verwendet, wobei lm() zunächst normale
# Standardfehler berechnet. Für die erste Regression werden keine Kontrollvariablen verwendet. 
# Für die zweite Regression werden dann Einrichtungs-spezifische Kontrollvariablen implementiert:
# Subsidy und Total Cost

# Ohne Kontrollvariablen
lmdid3_dayToDaySkills <- lm(dfcEF2$dayToDaySkills_scaled ~ dfcEF2$treatEF + dfcEF2$id + dfcEF2$year)
summary(lmdid3_dayToDaySkills)

# Mit Kontrollvariablen
lmdid3_dayToDaySkills_controls <- lm(dfcEF2$dayToDaySkills_scaled ~ dfcEF2$treatEF + dfcEF2$id + dfcEF2$year
                                     + dfcEF2$subsidy + dfcEF2$totalCost)
summary(lmdid3_dayToDaySkills_controls)

# Berechnung von robusten Standardfehlern, indem eine Heteroskedastität-konsistente Varianz-Kovarianz-Matrix
# für die geschätzten Regressionskoeffizienten generiert wird. Die robusten Standardfehler der geschätzten
# Regressionskoeffizienten werden entsprechend gespeichert
lmdid3_dayToDaySkills <- coeftest(lmdid3_dayToDaySkills, vcov. = vcovHC(lmdid3_dayToDaySkills, type = 'HC1'))
summary(lmdid3_dayToDaySkills)

lmdid3_dayToDaySkills_controls <- coeftest(lmdid3_dayToDaySkills_controls, 
                                           vcov. = vcovHC(lmdid3_dayToDaySkills_controls, type = 'HC1'))
summary(lmdid3_dayToDaySkills_controls)

# Erstellen des Regression-Tables mit der Funktion stargazer()
table_did3_dayToDaySkills <- stargazer(lmdid3_dayToDaySkills,
                                      omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                               'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                               'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                               'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                               'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                               'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                               'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                               'year2018'),
                                      add.lines = list(c('ID fixed effects', 'Yes'),
                                                       c('Year fixed effects', 'Yes')),
                                      type = 'text')

table_did3_dayToDaySkills_controls <- stargazer(lmdid3_dayToDaySkills_controls,
                                       omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                                'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                                'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                                'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                                'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                                'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                                'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                                'year2018'),
                                       add.lines = list(c('ID fixed effects', 'Yes'),
                                                        c('Year fixed effects', 'Yes')),
                                       type = 'text')

# Regressionstables als RDS speichern
saveRDS(lmdid3_dayToDaySkills, file = "./ANALYSIS/Tables/lmdid3_dayToDaySkills.Rds")
saveRDS(lmdid3_dayToDaySkills_controls, file = "./ANALYSIS/Tables/lmdid3_dayToDaySkills_controls.Rds")



## b) Selfworth:

# Ohne Kontrollvariablen
lmdid3_selfworth <- lm(dfcEF2$selfworth_scaled ~ dfcEF2$treatEF + dfcEF2$id + dfcEF2$year)
summary(lmdid3_selfworth)

# Mit Kontrollvariablen 
lmdid3_selfworth_controls <- lm(dfcEF2$selfworth_scaled ~ dfcEF2$treatEF + dfcEF2$id + dfcEF2$year
                                + dfcEF2$subsidy + dfcEF2$totalCost)
summary(lmdid3_selfworth_controls)

# Berechnung von robusten Standardfehlern, indem eine Heteroskedastität-konsistente Varianz-Kovarianz-Matrix
# für die geschätzten Regressionskoeffizienten generiert wird. Die robusten Standardfehler der geschätzten
# Regressionskoeffizienten werden entsprechend gespeichert
lmdid3_selfworth <- coeftest(lmdid3_selfworth, vcov. = vcovHC(lmdid3_selfworth, type = 'HC1'))
summary(lmdid3_selfworth)

lmdid3_selfworth_controls <- coeftest(lmdid3_selfworth_controls, 
                                      vcov. = vcovHC(lmdid3_selfworth_controls, type = 'HC1'))
summary(lmdid3_selfworth_controls)

# Erstellen des Regression-Tables mit der Funktion stargazer()
table_did3_selfworth <- stargazer(lmdid3_selfworth,
                                 omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                          'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                          'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                          'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                          'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                          'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                          'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                          'year2018'),
                                 add.lines = list(c('ID fixed effects', 'Yes'),
                                                  c('Year fixed effects', 'Yes')),
                                 type = 'text')

table_did3_selfworth_controls <- stargazer(lmdid3_selfworth_controls,
                                  omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                           'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                           'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                           'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                           'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                           'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                           'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                           'year2018'),
                                  add.lines = list(c('ID fixed effects', 'Yes'),
                                                   c('Year fixed effects', 'Yes')),
                                  type = 'text')

# Regressionstables als RDS speichern
saveRDS(lmdid3_selfworth, file = "./ANALYSIS/Tables/lmdid3_selfworth.rds")
saveRDS(lmdid3_selfworth_controls, file = "./ANALYSIS/Tables/lmdid3_selfworth_controls.rds")