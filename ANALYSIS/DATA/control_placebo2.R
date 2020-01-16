### Placebo-Test: 
# Erwartung: Die Variable "monthly cooks" sollte überhaupt nicht vom Entdeckerfonds beeinflusst 
# sein, sodass zwischen der Treatment- und Kontrollgruppe kein signifikanter Unterschied in dieser
# Variabel bestehen sollte

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

# Graphische Darstellung der zeitlichen Entwicklung der Variable "monthlyCooks" in der 
# Treatment- und Kontrollgruppe

plot(ts_monthlyCooks_control, main = "Trend of monthlyCooks: Treatment vs control", xlab = "Time", 
     ylab = "average monthlyCooks", col = "blue", lwd = 2, ylim = c(3.1,4.2),cex.main = 1.25)
lines(ts_monthlyCooks_treat, col = "red", lwd = 2)
text(2013.5, 3.5, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2014.5, 4.1, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

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

# Hinzufügen der linearen Trends:

plot(ts_monthlyCooks_control, main = "Trend of monthlyCooks: Treatment vs control", xlab = "Time", 
     ylab = "average monthlyCooks", col = "blue", lwd = 2, ylim = c(3.1,4.2),cex.main = 1.25)
lines(ts_monthlyCooks_treat, col = "red", lwd = 2)
lines(linearTrend_fit_control5, col = "blue", lwd = 1)
lines(linearTrend_fit_treat5, col = "red", lwd = 1)
text(2013.5, 3.5, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2014.5, 4.1, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

