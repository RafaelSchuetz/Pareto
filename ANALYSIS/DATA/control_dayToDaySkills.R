
### Graphiken erstellen zur Variable "dayToDaySkills", um Treatment und Control zu vergleichen ------------------------------------------

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

# Nun erstellen wir einen Graphen

plot(ts_dayToDaySkills_control, main = "Trend of dayToDaySkills: Treatment vs control", xlab = "Time", 
     ylab = "average dayToDaySkills", col = "blue", lwd = 2, ylim = c(2.2,3.3),cex.main = 1.25)
lines(ts_dayToDaySkills_treat, col = "red", lwd = 2)
text(2016, 2.7, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

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

# Hinzufügen der linearen Trends:

plot(ts_dayToDaySkills_control, main = "Trend of dayToDaySkills: Treatment vs control", xlab = "Time", 
     ylab = "average dayToDaySkills", col = "blue", lwd = 2, ylim = c(2.2,3.3),cex.main = 1.25)
lines(ts_dayToDaySkills_treat, col = "red", lwd = 2)
lines(linearTrend_fit_control2, col = "blue", lwd = 1)
lines(linearTrend_fit_treat2, col = "red", lwd = 1)
text(2016, 2.7, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "red", adj = 0.3, cex = 0.9)


### Graphen für die Abschlussarbeit erstellen -------------------------------

##Ziel: Raster im Hintergrund einfügen, Linien + Punkte

plot(ts_dayToDaySkills_control, main = "Trend of dayToDaySkills: Treatment vs control", xlab = "Year", 
     ylab = "Average dayToDaySkills", col = "grey40", lwd = 1.7, type = "o", ylim = c(2.2,3.4),cex.main = 1.25)
lines(ts_dayToDaySkills_treat, col = "grey0", lwd = 1.7, type = "o")
lines(linearTrend_fit_control2, col = "grey40", lwd = 1)
lines(linearTrend_fit_treat2, col = "grey0", lwd = 1)
text(2016, 2.7, "Control group",col = "grey40", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "grey0", adj = 0.3, cex = 0.9)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")


