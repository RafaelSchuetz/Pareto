#Real Subsidy Time Trend Total & Median

library(dplyr)
library(estimatr)
library(tidyverse)
library(ggplot2)
library(stats)
library(cowplot)

###total trips subsidy real 

realTripsSubTotal <- mergedData %>% 
  group_by(year)%>%
  summarize(total_TripsSubsidy=sum(realTripsSubsidy, na.rm = TRUE))%>%
  filter(!(total_TripsSubsidy == 0.0))


#plot the linear time trend of total real trips subsidy trend with ggplot 
totalRealTripSub <- ggplot(realTripsSubTotal, aes(year, total_TripsSubsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x = "Year", y = "Trips, total")

###median trips subsidy real 

medianRealTripsSubTotal <- mergedData %>%
  group_by(year)%>%
  summarize(Median_TripsSubsidy=median(realTripsSubsidy, na.rm = TRUE))%>%
  filter(!(Median_TripsSubsidy == 0.0))

#plot the linear time trend of median real trips subsidy trend with ggplot 
medianRealTripSub <- ggplot(medianRealTripsSubTotal, aes(year, Median_TripsSubsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y = "Trips, median")

#total lunch subsidy 
realSubTotal <- mergedData %>% 
  group_by(year)%>%
  summarize(total_Subsidy=sum(realSubsidy, na.rm = TRUE))

#plot the linear time trend of total real subsidy trend with ggplot 
totalRealSub <- ggplot(realSubTotal, aes(year, total_Subsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, total")


#median lunch subsidy 

medianRealSubTotal <- mergedData %>%
  group_by(year)%>%
  summarize(median_Subsidy=median(realSubsidy, na.rm = TRUE))
  
#plot the linear time trend of median real trips subsidy trend with ggplot 
medianRealSub <- ggplot(medianRealSubTotal, aes(year, median_Subsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, median")

###save subsidy plots in one graph 

summaryStatistics_Subsidy <- plot_grid(totalRealSub, totalRealTripSub, medianRealSub,medianRealTripSub, 
                      ncol = 2, nrow = 2, align = "vh",
                      labels = c("A", "B", "C", "D"),
                      label_x = 0, label_y = 0, hjust = -1.5, vjust = 
                        -1.5, label_fontface = "plain", label_size = 11)

saveRDS(summaryStatistics_Subsidy, "./ANALYSIS/GRAPHS/PAPER GRAPHS/summaryStatistics_Subsidy.Rds")

###per individual 