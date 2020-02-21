#Real Subsidy Time Trend Total & Median

library(dplyr)
library(estimatr)
library(tidyverse)
library(ggplot2)
library(stats)

#yearlydatawithrealsub

Values2011 <- dplyr::filter(mergedData, mergedData$year == 2011)
Values2012 <- dplyr::filter(mergedData, mergedData$year == 2012)
Values2013 <- dplyr::filter(mergedData, mergedData$year == 2013)
Values2014 <- dplyr::filter(mergedData, mergedData$year == 2014)
Values2015 <- dplyr::filter(mergedData, mergedData$year == 2015)
Values2016 <- dplyr::filter(mergedData, mergedData$year == 2016)
Values2017 <- dplyr::filter(mergedData, mergedData$year == 2017)
Values2018 <- dplyr::filter(mergedData, mergedData$year == 2018)

#lunch

#totalvalues

realSub2011 <- sum(Values2011$realSubsidy, na.rm = TRUE)
realSub2012 <- sum(Values2012$realSubsidy, na.rm = TRUE)
realSub2013 <- sum(Values2011$realSubsidy, na.rm = TRUE)
realSub2014 <- sum(Values2014$realSubsidy, na.rm = TRUE)
realSub2015 <- sum(Values2015$realSubsidy, na.rm = TRUE)
realSub2016 <- sum(Values2016$realSubsidy, na.rm = TRUE)
realSub2017 <- sum(Values2017$realSubsidy, na.rm = TRUE)
realSub2018 <- sum(Values2018$realSubsidy, na.rm = TRUE)

Year <- c(2011,2012,2013,2014,2015,2016,2017,2018)
RealSubTotal <- c(realSub2011, realSub2012, realSub2013, realSub2014, realSub2015, realSub2016, realSub2017, realSub2018)

# Create new data frame
df_realtotsub <- data.frame(Year, RealSubTotal)
view(df_realtotsub)

ts_totalRealSubsidy <- ts(df_realtotsub$RealSubTotal, start = 2011, end = 2018, frequency = 1)
View(ts_totalRealSubsidy)

#checks
str(ts_totalRealSubsidy)
class(ts_totalRealSubsidy)
ts_totalRealSubsidy
summary(ts_totalRealSubsidy)

#plot 
plot(ts_totalRealSubsidy, main = "Trend of total real subsidy MT", xlab = "Time", 
     ylab = "Total real subsidy MT", col = "blue", lwd = 2, ylim = c(550000,710000),cex.main = 1.25)
text(2016.25, 580000, "Total real subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

#lineartrend 

n <- length(ts_totalRealSubsidy)
t <- seq(from = 1, to = n)

linearTrend_totrealsub <- lm_robust(ts_totalRealSubsidy ~ t )
summary(linearTrend_totrealsub)

lt_totRealSub_fit <- linearTrend_totrealsub$fitted.values

lt_totRealSub_fit <- ts(lt_totRealSub_fit, start = 2011, end = 2018, frequency = 1)

#plottimetrend 

plot(ts_totalRealSubsidy, main = "Trend of total real subsidy MT", xlab = "Time", 
     ylab = "Total real subsidy MT", col = "blue", lwd = 2, ylim = c(550000,710000),cex.main = 1.25)
lines(lt_totRealSub_fit, col = "red", lwd = 1.5)
text(2016.25, 580000, "Total real subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

#simplemovingaverage 

detach("package:dplyr")

ts_totalRealSubsidy_ma3 <- filter(ts_totalRealSubsidy, filter = rep(1/3,3), sides = 2)
View(ts_totalRealSubsidy_ma3) 

totalrealsub_trend <- plot(ts_totalRealSubsidy, main = "Trend of total real subsidy MT", xlab = "Time", 
     ylab = "Total real subsidy MT", col = "blue", lwd = 2, ylim = c(550000,710000),cex.main = 1.25)
lines(lt_totRealSub_fit, col = "red", lwd = 1.5)
lines(ts_totalRealSubsidy_ma3, col = "green", lwd = 1.5)
text(2016.25, 580000, "Total real subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

#saveplot 
saveRDS(totalrealsub_trend, "./ANALYSIS/GRAPHS/PAPER GRAPHS/totalRealSubTrend.Rds")

#######

#median 

medianrealSub2011 <- median(Values2011$realSubsidy, na.rm = TRUE)
medianrealSub2012 <- median(Values2012$realSubsidy, na.rm = TRUE)
medianrealSub2013 <- median(Values2011$realSubsidy, na.rm = TRUE)
medianrealSub2014 <- median(Values2014$realSubsidy, na.rm = TRUE)
medianrealSub2015 <- median(Values2015$realSubsidy, na.rm = TRUE)
medianrealSub2016 <- median(Values2016$realSubsidy, na.rm = TRUE)
medianrealSub2017 <- median(Values2017$realSubsidy, na.rm = TRUE)
medianrealSub2018 <- median(Values2018$realSubsidy, na.rm = TRUE)

Year <- c(2011,2012,2013,2014,2015,2016,2017,2018)
medianRealSubTotal <- c(medianrealSub2011, medianrealSub2012, medianrealSub2013, medianrealSub2014, medianrealSub2015, medianrealSub2016, medianrealSub2017, medianrealSub2018)

df_realmediansub <- data.frame(Year, medianRealSubTotal)
view(df_realmediansub)

#timetrend 

ts_medianRealSubsidy = ts(df_realmediansub$medianRealSubTotal, start = 2011, end = 2018, frequency = 1)
View(ts_medianRealSubsidy)

#checks 
str(ts_medianRealSubsidy)
class(ts_medianRealSubsidy)
ts_medianRealSubsidy
summary(ts_medianRealSubsidy)   

#plot trend 
plot(ts_medianRealSubsidy, main = "Trend of the median real subsidy MT", xlab = "Time", 
     ylab = "Median real subsidy", col = "blue", lwd = 2, cex.main = 1.25)
text(2016.25, 11500, "Median real subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

#linear time trend 

n2 <- length(ts_medianRealSubsidy)
t2 <- seq(from = 1, to = n2)

linearTrend_medianReal <- lm_robust(ts_medianRealSubsidy ~ t2)
summary(linearTrend_medianReal)

linearTrend_medianReal_fit <- linearTrend_medianReal$fitted.values

linearTrend_medianReal_fit <- ts(linearTrend_medianReal_fit, start = 2011, end = 2018, frequency = 1)

#plotlineartrend 

plot(ts_medianRealSubsidy, main = "Trend of the median real subsidy", xlab = "Time", 
     ylab = "Median real subsidy", col = "blue", lwd = 2, cex.main = 1.25)
lines(linearTrend_medianReal_fit, col = "red", lwd = 1.5)
text(2016.25, 11500, "Median real subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

#simple moving average 

detach("package:dplyr")

ts_medianRealSubsidy_ma3 <- filter(ts_medianRealSubsidy, filter = rep(1/3,3), sides = 2)
View(ts_medianRealSubsidy_ma3) 

#plot simple moving average & trend line

medianrealsub_trend <- plot(ts_medianRealSubsidy, main = "Trend of the median real subsidy", xlab = "Time", 
     ylab = "Median real subsidy", col = "blue", lwd = 2, cex.main = 1.25)
lines(linearTrend_medianReal_fit, col = "red", lwd = 1.5)
lines(ts_medianRealSubsidy_ma3, col = "green", lwd = 1.5)
text(2016.25, 11500, "Median real subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

saveRDS(medianrealsub_trend, "./ANALYSIS/GRAPHS/PAPER GRAPHS/medianRealSubTrend.Rds")

########################################################
#subsidy median time trend (NOT REAL, for comparison)

medianSub2011 <- median(Values2011$subsidy, na.rm = TRUE)
medianSub2012 <- median(Values2012$subsidy, na.rm = TRUE)
medianSub2013 <- median(Values2011$subsidy, na.rm = TRUE)
medianSub2014 <- median(Values2014$subsidy, na.rm = TRUE)
medianSub2015 <- median(Values2015$subsidy, na.rm = TRUE)
medianSub2016 <- median(Values2016$subsidy, na.rm = TRUE)
medianSub2017 <- median(Values2017$subsidy, na.rm = TRUE)
medianSub2018 <- median(Values2018$subsidy, na.rm = TRUE)

Year <- c(2011,2012,2013,2014,2015,2016,2017,2018)
medianSubTotal <- c(medianSub2011, medianSub2012, medianSub2013, medianSub2014, medianSub2015, medianSub2016, medianSub2017, medianSub2018)

df_mediansub <- data.frame(Year, medianSubTotal)
view(df_mediansub)


#timetrend 

ts_medianSubsidy = ts(df_mediansub$medianSubTotal, start = 2011, end = 2018, frequency = 1)
View(ts_medianSubsidy)

#checks 
str(ts_medianSubsidy)
class(ts_medianSubsidy)
ts_medianSubsidy
summary(ts_medianSubsidy)   

#plot trend 
plot(ts_medianSubsidy, main = "Trend of the median subsidy MT", xlab = "Time", 
     ylab = "Median subsidy", col = "blue", lwd = 2, cex.main = 1.25)
text(2016.25, 11500, "Median subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

#linear time trend 

n3 <- length(ts_medianSubsidy)
t3 <- seq(from = 1, to = n3)

linearTrend_median <- lm_robust(ts_medianSubsidy ~ t3)
summary(linearTrend_median)

linearTrend_median_fit <- linearTrend_median$fitted.values

linearTrend_median_fit <- ts(linearTrend_median_fit, start = 2011, end = 2018, frequency = 1)

#plotlineartrend 

plot(ts_medianSubsidy, main = "Trend of the median subsidy", xlab = "Time", 
     ylab = "Median subsidy", col = "blue", lwd = 2, cex.main = 1.25)
lines(linearTrend_median_fit, col = "red", lwd = 1.5)
text(2016.25, 11500, "Median subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

#simple moving average 

detach("package:dplyr")

ts_medianSubsidy_ma3 <- filter(ts_medianSubsidy, filter = rep(1/3,3), sides = 2)
View(ts_medianSubsidy_ma3) 

#plot simple moving average & trend line

mediansub_trend <- plot(ts_medianSubsidy, main = "Trend of the median subsidy", xlab = "Time", 
     ylab = "Median subsidy", col = "blue", lwd = 2, cex.main = 1.25)
lines(linearTrend_median_fit, col = "red", lwd = 1.5)
lines(ts_medianSubsidy_ma3, col = "green", lwd = 1.5)
text(2016.25, 11500, "Median subsidy", adj = 0.3, cex = 0.9)
box(which = "figure")

saveRDS(mediansub_trend, "./ANALYSIS/GRAPHS/PAPER GRAPHS/medianSubTrend.Rds")





#trips 
realTripsSub2011 <- sum(Values2011$realTripsSubsidy, na.rm = TRUE)
realTripsSub2012 <- sum(Values2012$realTripsSubsidy, na.rm = TRUE)
realTripsSub2013 <- sum(Values2013$realTripsSubsidy, na.rm = TRUE)
realTripsSub2014 <- sum(Values2014$realTripsSubsidy, na.rm = TRUE)
realTripsSub2015 <- sum(Values2015$realTripsSubsidy, na.rm = TRUE)
realTripsSub2016 <- sum(Values2016$realTripsSubsidy, na.rm = TRUE)
realTripsSub2017 <- sum(Values2017$realTripsSubsidy, na.rm = TRUE)
realTripsSub2018 <- sum(Values2018$realTripsSubsidy, na.rm = TRUE)

