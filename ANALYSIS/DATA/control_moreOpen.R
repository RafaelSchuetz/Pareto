# Grafik erstellen zur Variable "moreOpen" --------

## Mittelwert einer bestimmte Variable bilden für Treatment- und Kontrollgruppe in jedem Jahr


mean6_2014_control <- mean(dfc_2014_control$moreOpen, na.rm = TRUE)
mean6_2014_treat <- mean(dfc_2014_treat$moreOpen, na.rm = TRUE)

mean6_2015_control <- mean(dfc_2015_control$moreOpen, na.rm = TRUE)
mean6_2015_treat <- mean(dfc_2015_treat$moreOpen, na.rm = TRUE)

mean6_2016_control <- mean(dfc_2016_control$moreOpen, na.rm = TRUE)
mean6_2016_treat <- mean(dfc_2016_treat$moreOpen, na.rm = TRUE)

mean6_2017_control <- mean(dfc_2017_control$moreOpen, na.rm = TRUE)
mean6_2017_treat <- mean(dfc_2017_treat$moreOpen, na.rm = TRUE)

mean6_2018_control <- mean(dfc_2018_control$moreOpen, na.rm = TRUE)
mean6_2018_treat <- mean(dfc_2018_treat$moreOpen, na.rm = TRUE)

# Nun erstellen wir einen Datensatz (timeseries) mit den Mittelwerten und Jahren, 
# um es anschließend grafisch darstellen zu können

year6 <- c(2014, 2015, 2016, 2017, 2018)
meanTreat6 <- c(mean6_2014_treat, mean6_2015_treat, mean6_2016_treat, mean6_2017_treat, 
                mean6_2018_treat)
meanControl6 <- c(mean6_2014_control, mean6_2015_control, 
                  mean6_2016_control, mean6_2017_control, mean6_2018_control)

timeseries6 <- data.frame(year6, meanTreat6, meanControl6)

ts_moreOpen_control = ts(timeseries6$meanControl6, start = 2014, end = 2018, frequency = 1)
ts_moreOpen_treat = ts(timeseries6$meanTreat6, start = 2014, end = 2018, frequency = 1)

# Nun erstellen wir einen Graphen

plot(ts_moreOpen_control, main = "Trend of moreOpen: Treatment vs control", xlab = "Time", 
     ylab = "average moreOpen", col = "blue", lwd = 2, ylim = c(2,3.5),cex.main = 1.25)
lines(ts_moreOpen_treat, col = "red", lwd = 2)
text(2016, 2.6, "Control group",col = "blue", adj = 0.3, cex = 0.9)
text(2016, 3.25, "Treatment group", col = "red", adj = 0.3, cex = 0.9)

