#Real Subsidy Time Trend

library(dplyr)

#totalvalues
#lunch

Values2011 <- dplyr::filter(mergedData, mergedData$year == 2011)
realSub2011 <- sum(Values2011$realSubsidy, na.rm = TRUE)

Values2012 <- dplyr::filter(mergedData, mergedData$year == 2012)
realSub2012 <- sum(Values2012$realSubsidy, na.rm = TRUE)

Values2013 <- dplyr::filter(mergedData, mergedData$year == 2013)
realSub2013 <- sum(Values2011$realSubsidy, na.rm = TRUE)

Values2014 <- dplyr::filter(mergedData, mergedData$year == 2014)
realSub2014 <- sum(Values2014$realSubsidy, na.rm = TRUE)

Values2015 <- dplyr::filter(mergedData, mergedData$year == 2015)
realSub2015 <- sum(Values2015$realSubsidy, na.rm = TRUE)

Values2016 <- dplyr::filter(mergedData, mergedData$year == 2016)
realSub2016 <- sum(Values2016$realSubsidy, na.rm = TRUE)

Values2017 <- dplyr::filter(mergedData, mergedData$year == 2017)
realSub2017 <- sum(Values2017$realSubsidy, na.rm = TRUE)

Values2018 <- dplyr::filter(mergedData, mergedData$year == 2018)
realSub2018 <- sum(Values2018$realSubsidy, na.rm = TRUE)

##

YearTotal <- c(2011,2012,2013,2014,2015,2016,2017,2018)
RealSubTotal <- c(realSub2011, realSub2012, realSub2013, realSub2014, realSub2015, realSub2016, realSub2017, realSub2018)

# Creating a new data frame
df_realtotsub <- data.frame(YearTotal, RealSubTotal)

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

linearTrend_totrealsub <- lm_robust(ts_totalrealSubsidy ~ t )
summary(linearTrend_totrealsub)

#trips 
realTripsSub2011 <- sum(Values2011$realTripsSubsidy, na.rm = TRUE)
realTripsSub2012 <- sum(Values2012$realTripsSubsidy, na.rm = TRUE)
realTripsSub2013 <- sum(Values2013$realTripsSubsidy, na.rm = TRUE)
realTripsSub2014 <- sum(Values2014$realTripsSubsidy, na.rm = TRUE)
realTripsSub2015 <- sum(Values2015$realTripsSubsidy, na.rm = TRUE)
realTripsSub2016 <- sum(Values2016$realTripsSubsidy, na.rm = TRUE)
realTripsSub2017 <- sum(Values2017$realTripsSubsidy, na.rm = TRUE)
realTripsSub2018 <- sum(Values2018$realTripsSubsidy, na.rm = TRUE)


