
# Analysing data 2011 - 2018 ----------------------------------------------

### 1. Time series

# a) Mean of a variable for every year

# Create a new data set only including the two variables "subsidyReceivedNo" and "year"
df <- data.frame(data201118$subsidy, data201118$year)
View(df)

library(tidyverse)
library(dplyr)
library(ggplot2)

# 1. Possibility: aggregate ()
# Aggregate column 1 of data set df, grouping by df$data201118.year and applying the mean-function
# na.rm = TRUE indicates that NA values are stripped before taking the mean

aggregate(df[,1], list(df$data201118.year), mean, na.rm = TRUE)


# 2. Possibility
# Defining variables "Year" and "Subsidy" as vectors by command c()
Year <- c(df$data201118.year)
Subsidy <- c(df$data201118.subsidy)

# Creating a new data frame with the variables "Year" and "Subsidy"
df <- data.frame(Year, Subsidy)


# The operator %>% pass the left hand side of the operator the first argument of the right hand 
# side. Grouping the data set df by the Year with group_by(). The command summarise() create
# a variable "averageSubsidy" which is the mean of "Subsidy".
mean_by_year <- df %>%
  group_by(df$Year) %>% 
  summarise(averagedSubsidy = mean(Subsidy, na.rm = TRUE))


# b) Graphic illustration of the time series

library(ggplot2)

# 1. Possibility
# Saving the data set "mean_by_year" in the variable "time_series"
time_series = mean_by_year

# Using the command plot.ts the average subsidy is plotted on the year 
plot.ts(x = time_series$`df$Year`, y = time_series$averagedSubsidy, plot.type = c("single"))


# 2. Possibility
# Defining a time series object for the average subsidy, starting with the observation of year
# 2011 and ending in year 2018, using a frequency of 1 because the data are collected anually
ts_averageSubsidy = ts(mean_by_year$averagedSubsidy, start = 2011, end = 2018, frequency = 1)
View(ts_averageSubsidy)

# Some checks: str() shows the internal structure of the time series, class() shows the class of 
# the object "ts_average subsidy" and ts_averageSubsidy represent the definition and content of 
# the objects / time series, summary() shows the summary statistics of the time series 
str(ts_averageSubsidy)
class(ts_averageSubsidy)
ts_averageSubsidy
summary(ts_averageSubsidy)       

# The command plot() creates a time series graph only mention the time series (R knows that
# the object is a time series), time on the x-axis and the average subsidy on the y-axis
# The parament lwd controls the line width of the time series curve, the parameter cex.main
# defines the size of the head line
plot(ts_averageSubsidy, main = "Trend of the average subsidy", xlab = "Time", 
     ylab = "Average subsidy", col = "blue", lwd = 2, cex.main = 1.25)
text(2016.25, 11500, "Average subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")


# c) Linear trend component: 
library(estimatr)
library(stats)

# Defining the length of the time series "ts_averageSubsidy" with the function length()
# Defining the vector of the time indices t with the function seq() from t = 1 (2011) 
# to t = n (2018)
n <- length(ts_averageSubsidy)
t <- seq(from = 1, to = n)

# Simple OLS regression: 
# The command lm_robust regress the time series "ts_averageSubsidy" (y-variable / outcome) 
# on the time indices (x-variable) using a linear regression model and robust standard errors
# Saving the OLS regression under "linear trend"
linearTrend <- lm_robust(ts_averageSubsidy ~ t)
summary(linearTrend)

# The intercept of 11886.0 is the trend value of the year before the observation period (2010).
# The slope of -199.0 represent the general time trend of the average subsidy, the coefficient
# means that in the treend the average subsidy is reduced by 199.0 units per year.

# Saving the fitted values of the linear regression for every year under "linearTrend_fit"
linearTrend_fit <- linearTrend$fitted.values

# Transforming the vector in a time series
linearTrend_fit <- ts(linearTrend_fit, start = 2011, end = 2018, frequency = 1)

# Drawing the trend line in the time series graphic with the command lines() which includes
# the time series "linearTrend_fit" as data
plot(ts_averageSubsidy, main = "Trend of the average subsidy", xlab = "Time", 
     ylab = "Average subsidy", col = "blue", lwd = 2, cex.main = 1.25)
lines(linearTrend_fit, col = "red", lwd = 1.5)
text(2016.25, 11500, "Average subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")


# d) Gleitender Durchschnitt 3. Ordnung (simple moving average):

# Before calculating the ma we have to detach the package "dplyr"
detach("package:dplyr")

# The command filter() can be used for calculating simple moving average for the time series
# "ts_averageSubsidy. We choose the time frame (t-1, t+1) resulting in 3 time periods which are
# so weighted with 1/3 (the command rep() replicates the values in x). With sides = 2 we use 
# a centered moving average (= standard setting). 
ts_averageSubsidy_ma3 <- filter(ts_averageSubsidy, filter = rep(1/3,3), sides = 2)
View(ts_averageSubsidy_ma3)  

# Additional to the previous R code for the graphic illustration, the command lines() draws
# the moving averages in the time series graphic

# Without trend lines:
plot(ts_averageSubsidy, main = "Trend of the average subsidy", xlab = "Time", 
     ylab = "Average subsidy", col = "blue", lwd = 2, cex.main = 1.25)
lines(ts_averageSubsidy_ma3, col = "green", lwd = 1.5)
text(2016.25, 11500, "Average subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

# With trend lines:
plot(ts_averageSubsidy, main = "Trend of the average subsidy", xlab = "Time", 
     ylab = "Average subsidy", col = "blue", lwd = 2, cex.main = 1.25)
lines(linearTrend_fit, col = "red", lwd = 1.5)
lines(ts_averageSubsidy_ma3, col = "green", lwd = 1.5)
text(2016.25, 11500, "Average subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")




