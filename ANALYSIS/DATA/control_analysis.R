# Analyse des Kontrollgruppenmodells

library(tidyverse)
library(dplyr)
library(ggplot2)
library(estimatr)
library(stats)

<<<<<<< HEAD
### Deskriptive Analyse

## 1. Schritt: Mittelwert einer Variable (z.B. "Selbstwertgefühl") für Treatment- und Kontroll-
# gruppe für jedes Jahr erstellen

# Erstellung von Datensätzen, in denen jeweils nur die Beobachtungen aus der Kontroll- oder
# Treatmentgruppe eines bestimmten Jahres aufgeführt werden, für jedes Jahr von 2012 bis 2018

=======
# Problem: Die kategorialen Variablen sind im Datentyp "Factor", welche zur weiteren 
# Bearbeitung in den Datentyp "numeric" geändert werden

dfc <- dfc %>% mutate_if(is.factor, as.numeric)

### Deskriptive Analyse

## 1. Schritt: Mittelwert einer Variable (z.B. "Selbstwertgefühl") für Treatment- und Kontroll-
# gruppe für jedes Jahr erstellen

# Erstellung von Datensätzen, in denen jeweils nur die Beobachtungen aus der Kontroll- oder
# Treatmentgruppe eines bestimmten Jahres aufgeführt werden, für jedes Jahr von 2012 bis 2018

>>>>>>> a6219b51965f321d7a65bd310fd02b3b46546d5a
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

<<<<<<< HEAD

  
# Erstellung 

  
dfc_                      
         

mean_by_year <- dfc %>%
  group_by(df$year) %>% 
  group_by(df$treat) %>%
  summarise(averagedSelfworth = mean(df$selfworth), na.rm = TRUE))             
=======
# 2012 in Ordnung

# 2013: Beobachtungen mit der ID-Nummer 404, 418, 437 (die sich in den Zeilen 8, 9 und 10
# befinden) müssen entfernt werden
drops <- c(8, 9, 10)
dfc_2013_control <- dfc_2013_control[-drops,]

# 2014: Beobachtungen mit der ID-Nummer 482, 483 müssen entfernt werden
drops <- c(7, 8)
dfc_2014_control <- dfc_2014_control[-drops,]

# 2015: in Ordnung

# 2016: Beobachtungen mit der ID-Nummer 599, 600, 602 müssen entfernt werden
drops <- c(7, 8, 10)
dfc_2016_control <- dfc_2016_control[-drops,]

# 2017: Beobachtungen mit der ID-Nummer 600, 623, 663 - 667 müssen entfernt werden
drops <- c(8, 11, 12, 13, 14, 15, 16)
dfc_2017_control <- dfc_2017_control[-drops,]

# 2018: Beobachtungen mit der ID-Nummer 663 - 667 müssen entfernt werden
drops <- c(10, 11, 12, 13, 14)
dfc_2018_control <- dfc_2018_control[-drops,]


## Mittelwert einer bestimmte Variable bilden für Treatment- und Kontrollgruppe in jedem Jahr

mean_2012_control <- mean(dfc_2012_control$selfworth, na.rm = TRUE)
mean_2012_treat <- mean(dfc_2012_treat$selfworth, na.rm = TRUE)

mean_2013_control <- mean(dfc_2013_control$selfworth, na.rm = TRUE)
mean_2013_treat <- mean(dfc_2013_treat$selfworth, na.rm = TRUE)

mean_2014_control <- mean(dfc_2014_control$selfworth, na.rm = TRUE)
mean_2014_treat <- mean(dfc_2014_treat$selfworth, na.rm = TRUE)

mean_2015_control <- mean(dfc_2015_control$selfworth, na.rm = TRUE)
mean_2015_treat <- mean(dfc_2015_treat$selfworth, na.rm = TRUE)

mean_2016_control <- mean(dfc_2016_control$selfworth, na.rm = TRUE)
mean_2016_treat <- mean(dfc_2016_treat$selfworth, na.rm = TRUE)

mean_2017_control <- mean(dfc_2017_control$selfworth, na.rm = TRUE)
mean_2017_treat <- mean(dfc_2017_treat$selfworth, na.rm = TRUE)

mean_2018_control <- mean(dfc_2018_control$selfworth, na.rm = TRUE)
mean_2018_treat <- mean(dfc_2018_treat$selfworth, na.rm = TRUE)

# Nun erstellen wir einen Datensatz (timeseries) mit den Mittelwerten und Jahren, 
# um es anschließend grafisch darstellen zu können

year <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
meanTreat <- c(mean_2012_treat, mean_2013_treat, mean_2014_treat, mean_2015_treat, mean_2016_treat, mean_2017_treat, 
               mean_2018_treat)
meanControl <- c(mean_2012_control, mean_2013_control, mean_2014_control, mean_2015_control, 
                 mean_2016_control, mean_2017_control, mean_2018_control)

timeseries <- data.frame(year, meanTreat, meanControl)

ts_selfworth_control = ts(timeseries$meanControl, start = 2012, end = 2018, frequency = 1)
ts_selfworth_treat = ts(timeseries$meanTreat, start = 2012, end = 2018, frequency = 1)

# Nun erstellen wir einen Graphen

plot(ts_selfworth_control, main = "Trend of selfworth: Treatment vs control", xlab = "Time", 
     ylab = "average selfworth", col = "blue", lwd = 2, ylim = c(2.5,3.5),cex.main = 1.25)
lines(ts_selfworth_treat, col = "red", lwd = 2)
text(2016, 2.6, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

## Linear trend component: 

# Definiere die Länge der Zeitreihe "ts_selfworth_control"mit der Funktion length(). 
# Definiere einen Vektor an Zeitindizes t mit der Funktion seq()
n <- length(ts_selfworth_control)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# lm_robust regressiert die Zeitreihe "ts_selfworth_control" (y-variable / outcome) 
# auf die Zeitindizes (x-variable)  anhand eines lineare Regressionsmodells mit robusten
# Standardfehlern. Speicehern der OLS regression unter "linearTrend_control"
linearTrend_control <- lm_robust(ts_selfworth_control ~ t)
summary(linearTrend_control)

# Speichern der geschätzten Werte der linearen Regression
linearTrend_fit_control <- linearTrend_control$fitted.values

# Umwandeln des Vektors in einer Zeitreihe
linearTrend_fit_control <- ts(linearTrend_fit_control, start = 2012, end = 2018, frequency = 1)

# Analog dazu für die Zeitreihe "ts_selfworth_treat"
n <- length(ts_selfworth_treat)
t <- seq(from = 1, to = n)

linearTrend_treat <- lm_robust(ts_selfworth_treat ~ t)
summary(linearTrend_treat)

linearTrend_fit_treat <- linearTrend_treat$fitted.values
linearTrend_fit_treat <- ts(linearTrend_fit_treat, start = 2012, end = 2018, frequency = 1)

# Hinzufügen der linearen Trends:

plot(ts_selfworth_control, main = "Trend of selfworth: Treatment vs control", xlab = "Time", 
     ylab = "average selfworth", col = "blue", lwd = 2, ylim = c(2.5,3.5),cex.main = 1.25)
lines(ts_selfworth_treat, col = "red", lwd = 2)
lines(linearTrend_fit_control, col = "blue", lwd = 1)
lines(linearTrend_fit_treat, col = "red", lwd = 1)
text(2016, 2.6, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "red", adj = 0.3, cex = 0.9)
