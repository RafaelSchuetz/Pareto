### Zeitreihe: Alltagskompetenzen

library(tidyverse)
library(dplyr)
library(ggplot2)

## a) Mittelwerte der Variable "dayToDaySkills" für jedes Jahr bilden

# Erstellen eines Datensatzes, der nur die beiden Variablen "dayToDaySkills" 
# und "year" enthält

df1 <- data.frame(dfc$dayToDaySkills, dfc$year)
View(df1)

# Die Variablen "dayToDaySkills" und "Year" werden als Vektoren definiert
Year <- c(df1$dfc.year)
DayToDaySkills <- c(df1$dfc.dayToDaySkills)

# Erstellen eines neuen Datensatzes mit den Variablen "Year" and "DayToDaySkills"
df1 <- data.frame(Year, DayToDaySkills)

# Gruppieren des Datensatzes df1 nach Jahr anhand des Befehl group_by(). Der Befehle summarise()
# generiert eine Variable "averageDayToDaySkills" mit den Mittelwerten von "dayToDaySkills"
mean_by_year1 <- df1 %>%
  group_by(df1$Year) %>% 
  summarise(averagedDayToDaySkills = mean(DayToDaySkills, na.rm = TRUE))


## b) Graphische Darstellung der Zeitreihe 

# Defining a time series object for the average subsidy, starting with the observation of year
# 2011 and ending in year 2018, using a frequency of 1 because the data are collected anually
ts_averageDayToDaySkills = ts(mean_by_year1$averagedDayToDaySkills, start = 2011, end = 2018, 
                              frequency = 1)
View(ts_averageSubsidy)

# The command plot() creates a time series graph only mention the time series (R knows that
# the object is a time series), time on the x-axis and the average subsidy on the y-axis
# The parament lwd controls the line width of the time series curve, the parameter cex.main
# defines the size of the head line
plot(ts_averageDayToDaySkills, main = "Trend of the average Day-To-Day-Skills", xlab = "Time", 
     ylab = "Average Day-To-Day-Skills", col = "blue", lwd = 2, 
     cex.main = 1.25, ylim = c(2.0,3.2))
text(2016, 2.8, "Average subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")


## c) Linear trend component: 
library(estimatr)
library(stats)

# Defining the length of the time series "ts_averageSubsidy" with the function length()
# Defining the vector of the time indices t with the function seq() from t = 1 (2011) 
# to t = n (2018)
n <- length(ts_averageDayToDaySkills)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# The command lm_robust regress the time series "ts_averageSubsidy" (y-variable / outcome) 
# on the time indices (x-variable) using a linear regression model and robust standard errors
# Saving the OLS regression under "linear trend"
linearTrend1 <- lm_robust(ts_averageDayToDaySkills ~ t)
summary(linearTrend)

# Saving the fitted values of the linear regression for every year under "linearTrend_fit"
linearTrend_fit1 <- linearTrend1$fitted.values

# Transforming the vector in a time series
linearTrend_fit1 <- ts(linearTrend_fit1, start = 2011, end = 2018, frequency = 1)

# Drawing the trend line in the time series graphic with the command lines() which includes
# the time series "linearTrend_fit" as data
plot(ts_averageDayToDaySkills, main = "Trend of the average Day-To-Day-Skills", xlab = "Time", 
     ylab = "Average Day-To-Day-Skills", col = "blue", lwd = 2, 
     cex.main = 1.25, ylim = c(2.0,3.2))
lines(linearTrend_fit1, col = "red", lwd = 1.5)
text(2013, 3.1, "Average subsidy", ,col = "blue", cex = 0.9)
text(2014, 2.8, "Linear trend", col = "red", cex = 0.9)
box(which = "figure")


## d) Gleitender Durchschnitt 3. Ordnung (simple moving average):

# Before calculating the ma we have to detach the package "dplyr"
detach("package:dplyr")

# The command filter() can be used for calculating simple moving average for the time series
# "ts_averageSubsidy. We choose the time frame (t-1, t+1) resulting in 3 time periods which are
# so weighted with 1/3 (the command rep() replicates the values in x). With sides = 2 we use 
# a centered moving average (= standard setting). 
ts_averageDayToDaySkills_ma <- filter(ts_averageDayToDaySkills, filter = rep(1/3,3), sides = 2)
View(ts_averageDayToDaySkills_ma)  

# Additional to the previous R code for the graphic illustration, the command lines() draws
# the moving averages in the time series graphic

# With trend lines:
plot(ts_averageDayToDaySkills, main = "Trend of the average Day-To-Day-Skills", xlab = "Time", 
     ylab = "Average Day-To-Day-Skills", col = "blue", lwd = 2, 
     cex.main = 1.25, ylim = c(2.0,3.2))
lines(linearTrend_fit1, col = "red", lwd = 1.5)
lines(ts_averageDayToDaySkills_ma, col = "green", lwd = 1.5)
text(2013, 3.1, "Day-To-Day-Skills", col = "blue", cex = 0.9)
text(2014, 2.8, "Linear trend", col = "red", cex = 0.9)
text(2016.5, 3.1, "Moving average", col = "green", cex = 0.9)
box(which = "figure")
