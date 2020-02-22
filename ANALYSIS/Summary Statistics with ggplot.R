#Real Subsidy Time Trend Total & Median

library(dplyr)
library(estimatr)
library(tidyverse)
library(ggplot2)
library(stats)
library(cowplot)


realTripsSubTotal <- mergedData %>% 
  group_by(year)%>% summarize(totalTripsSubsidy=sum(realTripsSubsidy, na.rm = TRUE))%>%
  filter(!(totalTripsSubsidy == 0.0))


#plot the linear time trend with ggplot 
ggplot(realTripsSubTotal, aes(year, totalTripsSubsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12)