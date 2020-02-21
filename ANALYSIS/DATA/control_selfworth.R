### Grafische Analyse anhand der Variable „selfworth" -----------------------

## Mittelwert für "selfworth" bilden für Treatment- und Kontrollgruppe in jedem Jahr

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

# Graphische Darstellung der zeitlichen Entwicklung der Variable "selfworth" in der 
# Treatment- und Kontrollgruppe

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

# Hinzufügen der linearen Trends:

plot(ts_selfworth_control, main = "Trend of selfworth: Treatment vs control", xlab = "Time", 
     ylab = "average selfworth", col = "blue", lwd = 2, ylim = c(2.5,3.5),cex.main = 1.25)
lines(ts_selfworth_treat, col = "red", lwd = 2)
lines(linearTrend_fit_control, col = "blue", lwd = 1)
lines(linearTrend_fit_treat, col = "red", lwd = 1)
text(2016, 2.6, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "red", adj = 0.3, cex = 0.9)
