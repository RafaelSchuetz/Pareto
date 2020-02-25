#Skripte davor durchlaufen lassen: MakeGather, control_gruppe


###erstellen von professionelleren time series für unser paper:
###In das paper hhinzufügen: selfworth, daytodayskills
###In den Appendix: Placebos: weeklycooks, monthlycooks

#Laden der packages

library(dplyr)
library(tidyverse)
library(magrittr)
library(tidyselect)
library(tseries)
library(graphics)
library(estimatr)

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


# Daten für selfworth vorbereiten -----------------------------------------



#erstellen der Durchschnitte:

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

# Erstellen einer Zeitreihe mit den Mittelwerten und Jahren, um anschließend die Zeitreihe
# grafisch darstellen zu können

year <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
meanTreat <- c(mean_2012_treat, mean_2013_treat, mean_2014_treat, mean_2015_treat, mean_2016_treat, mean_2017_treat, 
               mean_2018_treat)
meanControl <- c(mean_2012_control, mean_2013_control, mean_2014_control, mean_2015_control, 
                 mean_2016_control, mean_2017_control, mean_2018_control)

timeseries <- data.frame(year, meanTreat, meanControl)

ts_selfworth_control = ts(timeseries$meanControl, start = 2012, end = 2018, frequency = 1)
ts_selfworth_treat = ts(timeseries$meanTreat, start = 2012, end = 2018, frequency = 1)

## Linear trend component: 

# Definiere die Länge der Zeitreihe "ts_selfworth_control"mit der Funktion length(). 
# Definiere einen Vektor an Zeitindizes t mit der Funktion seq()
n <- length(ts_selfworth_control)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# lm_robust regressiert die Zeitreihe "ts_selfworth_control" (y-variable / outcome) 
# auf die Zeitindizes (x-variable)  anhand eines lineare Regressionsmodells mit robusten
# Standardfehlern. Speichern der OLS regression unter "linearTrend_control"
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

# Daten für daytodayskills vorbereiten ------------------------------------

#erstellen der Durchschnitte

mean2_2012_control <- mean(dfc_2012_control$dayToDaySkills, na.rm = TRUE)
mean2_2012_treat <- mean(dfc_2012_treat$dayToDaySkills, na.rm = TRUE)

mean2_2013_control <- mean(dfc_2013_control$dayToDaySkills, na.rm = TRUE)
mean2_2013_treat <- mean(dfc_2013_treat$dayToDaySkills, na.rm = TRUE)

mean2_2014_control <- mean(dfc_2014_control$dayToDaySkills, na.rm = TRUE)
mean2_2014_treat <- mean(dfc_2014_treat$dayToDaySkills, na.rm = TRUE)

mean2_2015_control <- mean(dfc_2015_control$dayToDaySkills, na.rm = TRUE)
mean2_2015_treat <- mean(dfc_2015_treat$dayToDaySkills, na.rm = TRUE)

mean2_2016_control <- mean(dfc_2016_control$dayToDaySkills, na.rm = TRUE)
mean2_2016_treat <- mean(dfc_2016_treat$dayToDaySkills, na.rm = TRUE)

mean2_2017_control <- mean(dfc_2017_control$dayToDaySkills, na.rm = TRUE)
mean2_2017_treat <- mean(dfc_2017_treat$dayToDaySkills, na.rm = TRUE)

mean2_2018_control <- mean(dfc_2018_control$dayToDaySkills, na.rm = TRUE)
mean2_2018_treat <- mean(dfc_2018_treat$dayToDaySkills, na.rm = TRUE)


# Nun erstellen wir einen Datensatz (timeseries) mit den Mittelwerten und Jahren, 
# um es anschließend grafisch darstellen zu können

year2 <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
meanTreat2 <- c(mean2_2012_treat, mean2_2013_treat, mean2_2014_treat, mean2_2015_treat, mean2_2016_treat, mean2_2017_treat, 
                mean2_2018_treat)
meanControl2 <- c(mean2_2012_control, mean2_2013_control, mean2_2014_control, mean2_2015_control, 
                  mean2_2016_control, mean2_2017_control, mean2_2018_control)

timeseries2 <- data.frame(year2, meanTreat2, meanControl2)

ts_dayToDaySkills_control = ts(timeseries2$meanControl2, start = 2012, end = 2018, frequency = 1)
ts_dayToDaySkills_treat = ts(timeseries2$meanTreat2, start = 2012, end = 2018, frequency = 1)

## Linear trend component: 

# Definiere die Länge der Zeitreihe "ts_selfworth_control"mit der Funktion length(). 
# Definiere einen Vektor an Zeitindizes t mit der Funktion seq()
n <- length(ts_dayToDaySkills_control)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# lm_robust regressiert die Zeitreihe "ts_selfworth_control" (y-variable / outcome) 
# auf die Zeitindizes (x-variable)  anhand eines lineare Regressionsmodells mit robusten
# Standardfehlern. Speicehern der OLS regression unter "linearTrend_control"
linearTrend_control2 <- lm_robust(ts_dayToDaySkills_control ~ t)
summary(linearTrend_control2)

# Speichern der geschätzten Werte der linearen Regression
linearTrend_fit_control2 <- linearTrend_control2$fitted.values

# Umwandeln des Vektors in einer Zeitreihe
linearTrend_fit_control2 <- ts(linearTrend_fit_control2, start = 2012, end = 2018, frequency = 1)

# Analog dazu für die Zeitreihe "ts_selfworth_treat"
n <- length(ts_dayToDaySkills_treat)
t <- seq(from = 1, to = n)

linearTrend_treat2 <- lm_robust(ts_dayToDaySkills_treat ~ t)
summary(linearTrend_treat2)

linearTrend_fit_treat2 <- linearTrend_treat2$fitted.values
linearTrend_fit_treat2 <- ts(linearTrend_fit_treat2, start = 2012, end = 2018, frequency = 1)

# Daten für placebo "weekly cooks" vorbereiten ----------------------------

#erstellen der Durchschnitte 

mean3_2012_control <- mean(dfc_2012_control$weeklyCooks, na.rm = TRUE)
mean3_2012_treat <- mean(dfc_2012_treat$weeklyCooks, na.rm = TRUE)

mean3_2013_control <- mean(dfc_2013_control$weeklyCooks, na.rm = TRUE)
mean3_2013_treat <- mean(dfc_2013_treat$weeklyCooks, na.rm = TRUE)

mean3_2014_control <- mean(dfc_2014_control$weeklyCooks, na.rm = TRUE)
mean3_2014_treat <- mean(dfc_2014_treat$weeklyCooks, na.rm = TRUE)

mean3_2015_control <- mean(dfc_2015_control$weeklyCooks, na.rm = TRUE)
mean3_2015_treat <- mean(dfc_2015_treat$weeklyCooks, na.rm = TRUE)

mean3_2016_control <- mean(dfc_2016_control$weeklyCooks, na.rm = TRUE)
mean3_2016_treat <- mean(dfc_2016_treat$weeklyCooks, na.rm = TRUE)

mean3_2017_control <- mean(dfc_2017_control$weeklyCooks, na.rm = TRUE)
mean3_2017_treat <- mean(dfc_2017_treat$weeklyCooks, na.rm = TRUE)

mean3_2018_control <- mean(dfc_2018_control$weeklyCooks, na.rm = TRUE)
mean3_2018_treat <- mean(dfc_2018_treat$weeklyCooks, na.rm = TRUE)

# Erstellen einer Zeitreihe mit den Mittelwerten und Jahren, um anschließend die Zeitreihe
# grafisch darstellen zu können

year3 <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
meanTreat3 <- c(mean3_2012_treat, mean3_2013_treat, mean3_2014_treat, mean3_2015_treat, 
                mean3_2016_treat, mean3_2017_treat, mean3_2018_treat)
meanControl3 <- c(mean3_2012_control, mean3_2013_control, mean3_2014_control, mean3_2015_control, 
                  mean3_2016_control, mean3_2017_control, mean3_2018_control)

timeseries3 <- data.frame(year, meanTreat3, meanControl3)

ts_weeklyCooks_control = ts(timeseries3$meanControl3, start = 2012, end = 2018, frequency = 1)
ts_weeklyCooks_treat = ts(timeseries3$meanTreat3, start = 2012, end = 2018, frequency = 1)

## Linear trend component: 

# Definiere die Länge der Zeitreihe "ts_weeklyCooks_control"mit der Funktion length(). 
# Definiere einen Vektor an Zeitindizes t mit der Funktion seq()
n <- length(ts_weeklyCooks_control)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# lm_robust regressiert die Zeitreihe "ts_weeklyCooks_control" (y-variable / outcome) 
# auf die Zeitindizes (x-variable)  anhand eines lineare Regressionsmodells mit robusten
# Standardfehlern. Speichern der OLS regression unter "linearTrend_control3"
linearTrend_control3 <- lm_robust(ts_weeklyCooks_control ~ t)
summary(linearTrend_control3)

# Speichern der geschätzten Werte der linearen Regression
linearTrend_fit_control3 <- linearTrend_control3$fitted.values

# Umwandeln des Vektors in einer Zeitreihe
linearTrend_fit_control3 <- ts(linearTrend_fit_control3, start = 2012, end = 2018, frequency = 1)

# Analog dazu für die Zeitreihe "ts_weeklyCooks_treat"
n <- length(ts_weeklyCooks_treat)
t <- seq(from = 1, to = n)

linearTrend_treat3 <- lm_robust(ts_weeklyCooks_treat ~ t)
summary(linearTrend_treat3)

linearTrend_fit_treat3 <- linearTrend_treat3$fitted.values
linearTrend_fit_treat3 <- ts(linearTrend_fit_treat3, start = 2012, end = 2018, frequency = 1)



# Daten für placebo "monthlycooks" vorbereiten ----------------------------

#erstellen der Durchschnitte

mean5_2012_control <- mean(dfc_2012_control$monthlyCooks, na.rm = TRUE)
mean5_2012_treat <- mean(dfc_2012_treat$monthlyCooks, na.rm = TRUE)

mean5_2013_control <- mean(dfc_2013_control$monthlyCooks, na.rm = TRUE)
mean5_2013_treat <- mean(dfc_2013_treat$monthlyCooks, na.rm = TRUE)

mean5_2014_control <- mean(dfc_2014_control$monthlyCooks, na.rm = TRUE)
mean5_2014_treat <- mean(dfc_2014_treat$monthlyCooks, na.rm = TRUE)

mean5_2015_control <- mean(dfc_2015_control$monthlyCooks, na.rm = TRUE)
mean5_2015_treat <- mean(dfc_2015_treat$monthlyCooks, na.rm = TRUE)

mean5_2016_control <- mean(dfc_2016_control$monthlyCooks, na.rm = TRUE)
mean5_2016_treat <- mean(dfc_2016_treat$monthlyCooks, na.rm = TRUE)

mean5_2017_control <- mean(dfc_2017_control$monthlyCooks, na.rm = TRUE)
mean5_2017_treat <- mean(dfc_2017_treat$monthlyCooks, na.rm = TRUE)

mean5_2018_control <- mean(dfc_2018_control$monthlyCooks, na.rm = TRUE)
mean5_2018_treat <- mean(dfc_2018_treat$monthlyCooks, na.rm = TRUE)

# Erstellen einer Zeitreihe mit den Mittelwerten und Jahren, um anschließend die Zeitreihe
# grafisch darstellen zu können

year5 <- c(2012, 2013, 2014, 2015, 2016, 2017, 2018)
meanTreat5 <- c(mean5_2012_treat, mean5_2013_treat, mean5_2014_treat, mean5_2015_treat, 
                mean5_2016_treat, mean5_2017_treat, mean5_2018_treat)
meanControl5 <- c(mean5_2012_control, mean5_2013_control, mean5_2014_control, mean5_2015_control, 
                  mean5_2016_control, mean5_2017_control, mean5_2018_control)

timeseries5 <- data.frame(year, meanTreat5, meanControl5)

ts_monthlyCooks_control = ts(timeseries5$meanControl5, start = 2012, end = 2018, frequency = 1)
ts_monthlyCooks_treat = ts(timeseries5$meanTreat5, start = 2012, end = 2018, frequency = 1)

## Linear trend component: 

# Definiere die Länge der Zeitreihe "ts_monthlyCooks_control"mit der Funktion length(). 
# Definiere einen Vektor an Zeitindizes t mit der Funktion seq()
n <- length(ts_monthlyCooks_control)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# lm_robust regressiert die Zeitreihe "ts_monthlyCooks_control" (y-variable / outcome) 
# auf die Zeitindizes (x-variable)  anhand eines lineare Regressionsmodells mit robusten
# Standardfehlern. Speichern der OLS regression unter "linearTrend_control5"
linearTrend_control5 <- lm_robust(ts_monthlyCooks_control ~ t)
summary(linearTrend_control5)

# Speichern der geschätzten Werte der linearen Regression
linearTrend_fit_control5 <- linearTrend_control5$fitted.values

# Umwandeln des Vektors in einer Zeitreihe
linearTrend_fit_control5 <- ts(linearTrend_fit_control5, start = 2012, end = 2018, frequency = 1)

# Analog dazu für die Zeitreihe "ts_monthlyCooks_treat"
n <- length(ts_monthlyCooks_treat)
t <- seq(from = 1, to = n)

linearTrend_treat5 <- lm_robust(ts_monthlyCooks_treat ~ t)
summary(linearTrend_treat5)

linearTrend_fit_treat5 <- linearTrend_treat5$fitted.values
linearTrend_fit_treat5 <- ts(linearTrend_fit_treat5, start = 2012, end = 2018, frequency = 1)


###Graphische Darstellung:


####1. DayToDaySkills ----------------------------------------------------


#use the seqplot.ts befehl

seqplot.ts(ts_dayToDaySkills_treat, ts_dayToDaySkills_control, colx = "grey0", coly = "grey40", typex = "o",
           typey = "o", pchx = 2, pchy = 1, ltyx = "solid",
           ltyy = "dotted", oma = c(6, 0, 5, 0), ann = par("ann"),
           xlab = "Year", ylab = ("everyday expertise"), main = "Trend of everyday expertise")

#hinzufügen einer legende

legend("topleft", legend=c("Treatment", "Control"),
       col=c("grey0", "grey40"), lty=1:2, cex=0.8)

#hinzufügen von raster im hintergrund

grid(nx = NA, ny = NULL, col = "grey70", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#hinzufügen von linearen trends
#für dayToDaySkills: linearTrend_fit_treat2,linearTrend_fit_control2

lines(linearTrend_fit_treat2, col = "grey0", lwd = 1)
lines(linearTrend_fit_control2, col = "grey40", lwd = 1)


####2. selfworth ---------------------------------------------------------

#use the seqplot.ts befehl

seqplot.ts(ts_selfworth_treat, ts_selfworth_control, colx = "grey0", coly = "grey40", typex = "o",
           typey = "o", pchx = 2, pchy = 1, ltyx = "solid",
           ltyy = "dotted", oma = c(6, 0, 5, 0), ann = par("ann"),
           xlab = "Year", ylab = ("Selfworth"), main = "Trend of Selfworth")

#hinzufügen einer legende

legend("topleft", legend=c("Treatment", "Control"),
       col=c("grey0", "grey40"), lty=1:2, cex=0.8)

#hinzufügen von raster im hintergrund

grid(nx = NA, ny = NULL, col = "grey70", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#hinzufügen von linearen trends
#für selfworth: linearTrend_fit_treat,linearTrend_fit_control

lines(linearTrend_fit_treat, col = "grey0", lwd = 1)
lines(linearTrend_fit_control, col = "grey40", lwd = 1)


####3. weeklyCooks -------------------------------------------------------

#use the seqplot.ts befehl

seqplot.ts(ts_weeklyCooks_treat, ts_weeklyCooks_control, colx = "grey0", coly = "grey40", typex = "o",
           typey = "o", pchx = 2, pchy = 1, ltyx = "solid",
           ltyy = "dotted", oma = c(6, 0, 5, 0), ann = par("ann"),
           xlab = "Year", ylab = ("weekly cooks"), main = "Trend of weekly cooks")

#hinzufügen einer legende

legend("topleft", legend=c("Treatment", "Control"),
       col=c("grey0", "grey40"), lty=1:2, cex=0.8)

#hinzufügen von raster im hintergrund

grid(nx = NA, ny = NULL, col = "grey70", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#hinzufügen von linearen trends
#für weeklyCooks: linearTrend_fit_treat3,linearTrend_fit_control3

lines(linearTrend_fit_treat3, col = "grey0", lwd = 1)
lines(linearTrend_fit_control3, col = "grey40", lwd = 1)



####4. monthlyCooks ------------------------------------------------------

#use the seqplot.ts befehl

seqplot.ts(ts_monthlyCooks_treat, ts_monthlyCooks_control, colx = "grey0", coly = "grey40", typex = "o",
           typey = "o", pchx = 2, pchy = 1, ltyx = "solid",
           ltyy = "dotted", oma = c(6, 0, 5, 0), ann = par("ann"),
           xlab = "Year", ylab = ("monthly cooks"), main = "Trend of monthly cooks")

#hinzufügen einer legende

legend("topleft", legend=c("Treatment", "Control"),
       col=c("grey0", "grey40"), lty=1:2, cex=0.8)

#hinzufügen von raster im hintergrund

grid(nx = NA, ny = NULL, col = "grey70", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

#hinzufügen von linearen trends
#für monthlyCooks: linearTrend_fit_treat5,linearTrend_fit_control5

lines(linearTrend_fit_treat5, col = "grey0", lwd = 1)
lines(linearTrend_fit_control5, col = "grey40", lwd = 1)
