### Placebo-Test: 
# Erwartung: Die Variable "weekly cooks" sollte überhaupt nicht vom Entdeckerfonds beeinflusst 
# sein, sodass zwischen der Treatment- und Kontrollgruppe kein signifikanter Unterschied in dieser
# Variabel bestehen sollte

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

# Graphische Darstellung der zeitlichen Entwicklung der Variable "weeklyCooks" in der 
# Treatment- und Kontrollgruppe

plot(ts_weeklyCooks_control, main = "Trend of weeklyCooks: Treatment vs control", xlab = "Time", 
     ylab = "average weeklyCooks", col = "blue", lwd = 2, ylim = c(2.0,4.0),cex.main = 1.25)
lines(ts_weeklyCooks_treat, col = "red", lwd = 2)
text(2013, 2.4, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2013, 3.5, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

## Linear trend component: 

# Definiere die Länge der Zeitreihe "ts_weeklyCooks_control"mit der Funktion length(). 
# Definiere einen Vektor an Zeitindizes t mit der Funktion seq()
n <- length(ts_weeklyCooks_control)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# lm_robust regressiert die Zeitreihe "ts_weeklyCooks_control" (y-variable / outcome) 
# auf die Zeitindizes (x-variable)  anhand eines lineare Regressionsmodells mit robusten
# Standardfehlern. Speicehern der OLS regression unter "linearTrend_control"
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

# Hinzufügen der linearen Trends:

plot(ts_weeklyCooks_control, main = "Trend of weeklyCooks: Treatment vs control", xlab = "Time", 
     ylab = "average weeklyCooks", col = "blue", lwd = 2, ylim = c(2.0,4.0),cex.main = 1.25)
lines(ts_weeklyCooks_treat, col = "red", lwd = 2)
lines(linearTrend_fit_control3, col = "blue", lwd = 1)
lines(linearTrend_fit_treat3, col = "red", lwd = 1)
text(2013, 2.4, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2013, 3.5, "Treatment group", col = "red", adj = 0.3, cex = 0.9)
