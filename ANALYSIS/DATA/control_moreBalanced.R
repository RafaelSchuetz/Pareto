
# Grafik erstellen zur Variable "moreBalanced" --------

## Mittelwert einer bestimmte Variable bilden für Treatment- und Kontrollgruppe in jedem Jahr


mean4_2014_control <- mean(dfc_2014_control$moreBalanced, na.rm = TRUE)
mean4_2014_treat <- mean(dfc_2014_treat$moreBalanced, na.rm = TRUE)

mean4_2015_control <- mean(dfc_2015_control$moreBalanced, na.rm = TRUE)
mean4_2015_treat <- mean(dfc_2015_treat$moreBalanced, na.rm = TRUE)

mean4_2016_control <- mean(dfc_2016_control$moreBalanced, na.rm = TRUE)
mean4_2016_treat <- mean(dfc_2016_treat$moreBalanced, na.rm = TRUE)

mean4_2017_control <- mean(dfc_2017_control$moreBalanced, na.rm = TRUE)
mean4_2017_treat <- mean(dfc_2017_treat$moreBalanced, na.rm = TRUE)

mean4_2018_control <- mean(dfc_2018_control$moreBalanced, na.rm = TRUE)
mean4_2018_treat <- mean(dfc_2018_treat$moreBalanced, na.rm = TRUE)

# Nun erstellen wir einen Datensatz (timeseries) mit den Mittelwerten und Jahren, 
# um es anschließend grafisch darstellen zu können

year4 <- c(2014, 2015, 2016, 2017, 2018)
meanTreat4 <- c(mean4_2014_treat, mean4_2015_treat, mean4_2016_treat, mean4_2017_treat, 
               mean4_2018_treat)
meanControl4 <- c(mean4_2014_control, mean4_2015_control, 
                 mean4_2016_control, mean4_2017_control, mean4_2018_control)

timeseries4 <- data.frame(year4, meanTreat4, meanControl4)

ts_moreBalanced_control = ts(timeseries4$meanControl4, start = 2014, end = 2018, frequency = 1)
ts_moreBalanced_treat = ts(timeseries4$meanTreat4, start = 2014, end = 2018, frequency = 1)

# Nun erstellen wir einen Graphen

plot(ts_moreBalanced_control, main = "Trend of moreBalanced: Treatment vs control", xlab = "Time", 
     ylab = "average moreBalanced", col = "blue", lwd = 2, ylim = c(2,3.5),cex.main = 1.25)
lines(ts_moreBalanced_treat, col = "red", lwd = 2)
text(2016, 2.6, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

