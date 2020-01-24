
# Arbeit mit neu-definierten Treat und Control ----------------------------


#Als nächstes wollen wir die jährlichen Durchschnitte von 2011-2018 ausrechnen
#Zunächst für die Kontrollgruppe, selfworth als Variable

mean_by_year_control <- data_control %>%
  group_by(data_control$year) %>% 
  summarise(averageSelfworthControl = mean(selfworth, na.rm = TRUE))

#Nun für die Treatment Gruppe

mean_by_year_treat <- data_treat %>%
  group_by(data_treat$year) %>% 
  summarise(averageSelfworthTreat = mean(selfworth, na.rm = TRUE))

#Für die Übersicht: Mean der Switcher einbauen

mean_by_year_skip <- data_skip %>%
  group_by(data_skip$year) %>% 
  summarise(averageSelfworthSkip = mean(selfworth, na.rm = TRUE))

# Defining a time series object for the average selfworth, starting with the observation of year
# 2011 and ending in year 2018, using a frequency of 1 because the data are collected anually
ts_averageSelfworthTreat = ts(mean_by_year_treat$averageSelfworthTreat, start = 2011, end = 2018, frequency = 1)
ts_averageSelfworthControl = ts(mean_by_year_control$averageSelfworthControl, start = 2011, end = 2018, frequency = 1)
ts_averageSelfworthSkip = ts(mean_by_year_skip$averageSelfworthSkip, start = 2011, end = 2018, frequency = 1)
#Nun wollen wir die Zeitreihen grafisch darstellen

plot(ts_averageSelfworthTreat, main = "Treatment vs. control group: Selfworth", xlab = "Time", 
     ylab = "Average selfworth", col = "red", lwd = 2, cex.main = 1.25, ylim = c(2,3.5))
lines(ts_averageSelfworthControl, col = "blue", lwd = 2)
text(2013, 3.2, "Treatment group",col = "red", adj = 0.3, cex = 0.9)
text(2016, 2.4, "Control group", col = "blue", adj = 0.3, cex = 0.9)

