
# Analysing data EF -------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(estimatr)
library(stats)

### 1. Time series: Total subsidy EF for an institution


### 2. Time series: Activities EF for an institution

## a) Creating a data frame of the activities MT with the corresponding year

# Calculating the total subsidies MT across every institution for every year

totActivitiesMT_2012 <- sum(data2012$tripsNo, na.rm = TRUE)
totActivitiesMT_2013 <- sum(data2013$tripsNo, na.rm = TRUE)
totActivitiesMT_2014 <- sum(data2014$tripsNo, na.rm = TRUE)
totActivitiesMT_2015 <- sum(data2015$tripsNo, na.rm = TRUE)
totActivitiesMT_2016 <- sum(data2016$tripsNo, na.rm = TRUE)
totActivitiesMT_2017 <- sum(df_2017$tripsNo, na.rm = TRUE)
totActivitiesMT_2018 <- sum(df_2018$tripsNo, na.rm = TRUE)

# Defining the variables "totAct" (= total acitivties EF) and "yearTotAct" (= year) as vectors 
# by command c()
yearTotAct <- c(2012,2013,2014,2015,2016,2017,2018)
totAct <- c(totActivitiesMT_2012, totActivitiesMT_2013, totActivitiesMT_2014, totActivitiesMT_2015,
            totActivitiesMT_2016, totActivitiesMT_2017, totActivitiesMT_2018)

# Creating a new data frame
df_totAct <- data.frame(yearTotAct, totAct)

## b) Graphic illustration of the time series

# Defining a time series object for the total activities EF, now starting in 2012 because
# the variable is not available for 2011
ts_totAct <- ts(df_totAct$totAct, start = 2012, end = 2018, frequency = 1)
View(ts_totAct)

# Some checks:
str(ts_totAct)
class(ts_totAct)
ts_totAct
summary(ts_totAct)  

# The command plot() creates a time series graph only mention the time series (R knows that
# the object is a time series), time on the x-axis and the average subsidy on the y-axis
# The parament lwd controls the line width of the time series curve, the parameter cex.main
# defines the size of the head line
plot(ts_totAct, main = "Trend of total actitivies EF", xlab = "Time", 
     ylab = "Total activities EF", col = "blue", lwd = 2, ylim = c(175, 240), cex.main = 1.25)
text(2016.5, 195, "Total activities EF", adj = 0.3, cex = 0.9)
box(which = "figure")

## c) Linear trend component

# Defining the length of the time series "ts_averageSubsidy" with the function length()
# Defining the vector of the time indices t with the function seq() from t = 1 (2011) 
# to t = n (2018)
nAct <- length(ts_totAct)
tAct <- seq(from = 1, to = nAct)

# Simple OLS regression: 
# The command lm_robust regress the time series "ts_totAct" (y-variable / outcome) 
# on the time indices (x-variable) using a linear regression model and robust standard errors
# Saving the OLS regression under "linear trend"
linearTrend_totAct <- lm_robust(ts_totAct ~ tAct)
summary(linearTrend_totAct)

# The intercept of 184.714 is the trend value of the year before the observation period (2010).
# The slope of 5.429 represent the general time trend of the total acitivties of EF, 
# the coefficient says that in the trend total activies increased by 5.429 units per year.

# Saving the fitted values of the linear regression for every year under "lt_totSub_fit"
lt_totAct_fit <- linearTrend_totAct$fitted.values

# Transforming the vector in a time series
lt_totAct_fit <- ts(lt_totAct_fit, start = 2012, end = 2018, frequency = 1)

# Drawing the trend line in the time series graphic with the command lines() which includes
# the time series "linearTrend_totAct" as data
plot(ts_totAct, main = "Trend of total actitivies EF", xlab = "Time", 
     ylab = "Total activities EF", col = "blue", lwd = 2, ylim = c(175, 240), cex.main = 1.25)
lines(lt_totAct_fit, col = "red", lwd = 1.5)
text(2016.5, 195, "Total activities EF", adj = 0.3, cex = 0.9)
box(which = "figure")


## d) Gleitender Durchschnitt 3. Ordnung (simple moving average):

# The command filter() can be used for calculating simple moving average for the time series
# "ts_totAct. We choose the time frame (t-1, t+1) resulting in 3 time periods which are
# so weighted with 1/3 (the command rep() replicates the values in x). With sides = 2 we use 
# a centered moving average (= standard setting). 
ts_totAct_ma3 <- filter(ts_totAct, filter = rep(1/3,3), sides = 2)
View(ts_totAct_ma3)

# Additional to the previous R code for the graphic illustration, the command lines() draws
# the moving averages in the time series graphic

# With trend lines:
plot(ts_totAct, main = "Trend of total actitivies EF", xlab = "Time", 
     ylab = "Total activities EF", col = "blue", lwd = 2, ylim = c(175, 240), cex.main = 1.25)
lines(lt_totAct_fit, col = "red", lwd = 1.5)
lines(ts_totAct_ma3, col ="green", lwd = 1.5)
text(2016.5, 195, "Total activities EF", adj = 0.3, cex = 0.9)
box(which = "figure")
