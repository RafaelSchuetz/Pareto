### Analyse der Variable "dayToDaySkills" anhand der neu definierten
### Treatment- und Kontrollgruppe

# Mittelwert für die Variable "dayToDaySkills" für jedes Jahr von 2011 bis 2018 in der
# Treatment- und Kontrollgruppe bilden

mean_by_year_control <- data_control %>%
  group_by(data_control$year) %>% 
  summarise(averageDayToDaySkills = mean(dayToDaySkills, na.rm = TRUE))

mean_by_year_treat <- data_treat %>%
  group_by(data_treat$year) %>% 
  summarise(averageDayToDaySkills = mean(dayToDaySkills, na.rm = TRUE))

# Erstellen einer Zeitreihe für die Mittelwerte in der Treatment- und Kontrollgruppe für den
# Zeitraum von 2011 bis 2018, um anschließend die Zeitreihe

ts_averageDayToDaySkills_control = ts(mean_by_year_control$averageDayToDaySkills, 
                              start = 2011, end = 2018, frequency = 1)

ts_averageDayToDaySkills_treat = ts(mean_by_year_treat$averageDayToDaySkills, 
                                      start = 2011, end = 2018, frequency = 1)

# Graphische Darstellung der zeitlichen Entwicklung der Variable "dayToDaySkills" in der 
# Treatment- und Kontrollgruppe

plot(ts_averageDayToDaySkills_control, xlab = "Time", ylab = "average dayToDaySkills",
     main = "Trend of average Day-To-Day-Skills: Treatment vs control", col = "blue", 
     lwd = 2, ylim = c(1.9,3.3),cex.main = 1.25)
lines(ts_averageDayToDaySkills_treat, col = "red", lwd = 2)
text(2016, 2.2, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2013, 3.2, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

## Linear trend component: 

# Definiere die Länge der Zeitreihe "ts_selfworth_control"mit der Funktion length(). 
# Definiere einen Vektor an Zeitindizes t mit der Funktion seq()
n <- length(ts_averageDayToDaySkills_control)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# lm_robust regressiert die Zeitreihe "ts_selfworth_control" (y-variable / outcome) 
# auf die Zeitindizes (x-variable)  anhand eines lineare Regressionsmodells mit robusten
# Standardfehlern. Speichern der OLS regression unter "linearTrend_control"
linearTrend_control2 <- lm_robust(ts_averageDayToDaySkills_control ~ t)
summary(linearTrend_control2)

# Speichern der geschätzten Werte der linearen Regression
linearTrend_fit_control2 <- linearTrend_control2$fitted.values

# Umwandeln des Vektors in einer Zeitreihe
linearTrend_fit_control2 <- ts(linearTrend_fit_control2, start = 2011, end = 2018, frequency = 1)

# Analog dazu für die Zeitreihe "ts_selfworth_treat"
n <- length(ts_averageDayToDaySkills_treat)
t <- seq(from = 1, to = n)

linearTrend_treat2 <- lm_robust(ts_averageDayToDaySkills_treat ~ t)
summary(linearTrend_treat2)

linearTrend_fit_treat2 <- linearTrend_treat2$fitted.values
linearTrend_fit_treat2 <- ts(linearTrend_fit_treat2, start = 2011, end = 2018, frequency = 1)

# Hinzufügen der linearen Trends:

plot(ts_averageDayToDaySkills_control, xlab = "Time", ylab = "average dayToDaySkills",
     main = "Treatment vs control: Day-to-day-skills", col = "blue", 
     lwd = 2, ylim = c(1.9,3.3),cex.main = 1.25)
lines(ts_averageDayToDaySkills_treat, col = "red", lwd = 2)
lines(linearTrend_fit_control2, col = "blue", lwd = 1)
lines(linearTrend_fit_treat2, col = "red", lwd = 1)
text(2016, 2.2, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2013, 3.2, "Treatment group", col = "red", adj = 0.3, cex = 0.9)