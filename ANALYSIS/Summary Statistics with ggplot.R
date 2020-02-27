# #Real Subsidy Time Trend Total & Median
# 
# library(dplyr)
# library(estimatr)
# library(tidyverse)
# library(ggplot2)
# library(stats)
# library(cowplot)

share of beneficiaries with improved self-worth

share of beneficiaries with broadened everyday expertise

share of beneficiaries who are less frequently ill 

share of beneficiaries with expanded dietary knowledge 

share of beneficiaries with increased appreciation for a healthy diet 

realSubsidy: subsidy for Meals program in 2015 EUR 

realSubsidyPerBeneficiary: subsidy per beneficiary of Meals program in 2015 EUR

realTripsSubsidy: subsidy for Trips program in 2015 EUR

realTripsSubsidyPerBeneficiary: subsidy per beneficiary of Trips program in 2015 EUR

DGECriteriaNo: index of healthy diet criteria fulfilled in organization's menu

eatersPerMeal: number of beneficiaries of Lunch program 

tripsKidsNo: number of beneficiaries of Trips program







# 
# ###total trips subsidy real 
# 
# realTripsSubTotal <- mergedData %>% 
#   group_by(year)%>%
#   dplyr::summarize(total_TripsSubsidy=sum(realTripsSubsidy, na.rm = TRUE))%>%
#   dplyr::filter(!(total_TripsSubsidy == 0.0))
# 
# 
# #plot the linear time trend of total real trips subsidy trend with ggplot 
# totalRealTripSub <- ggplot(realTripsSubTotal, aes(year, total_TripsSubsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x = "Year", y = "Trips, total")
# 
# ###median trips subsidy real 
# 
# medianRealTripsSubTotal <- mergedData %>%
#   group_by(year)%>%
#   dplyr::summarize(Median_TripsSubsidy=median(realTripsSubsidy, na.rm = TRUE))%>%
#   dplyr::filter(!(Median_TripsSubsidy == 0.0))
# 
# #plot the linear time trend of median real trips subsidy trend with ggplot 
# medianRealTripSub <- ggplot(medianRealTripsSubTotal, aes(year, Median_TripsSubsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y = "Trips, median")
# 
# #total lunch subsidy 
# realSubTotal <- mergedData %>% 
#   group_by(year)%>%
#   dplyr::summarize(total_Subsidy=sum(realSubsidy, na.rm = TRUE))
# 
# #plot the linear time trend of total real subsidy trend with ggplot 
# totalRealSub <- ggplot(realSubTotal, aes(year, total_Subsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, total")
# 
# 
# #median lunch subsidy 
# 
# medianRealSub <- mergedData %>%
#   group_by(year)%>%
#   dplyr::summarize(median_Subsidy=median(realSubsidy, na.rm = TRUE))
#   
# #plot the linear time trend of median real trips subsidy trend with ggplot 
# medianRealSub <- ggplot(medianRealSub, aes(year, median_Subsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, median")
# 
# ###per individual 
# 
# #lunch
# #median
# 
# medianRealSubInd <- mergedData %>%
#   group_by(year)%>%
#   dplyr::summarize(median_SubsidyInd=median(realSubsidyPerBeneficiary, na.rm = TRUE))
# 
# #plot the linear time trend of median real trips subsidy per ind trend with ggplot 
# medianSubInd <- ggplot(medianRealSubInd, aes(year, median_SubsidyInd)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, median grant/beneficiary")
# 
# 
# #trips 
# 
# #median
# medianRealTripsSubInd <- mergedData %>%
#   group_by(year)%>%
#   dplyr::summarize(median_TripsSubsidyInd=median(realTripsSubsidyPerBeneficiary, na.rm = TRUE))%>%
#   dplyr::filter(!(median_TripsSubsidyInd == 0.0))
# 
# #plot the linear time trend of median real trips subsidy per ind trend with ggplot 
# medianTripsSubInd <- ggplot(medianRealTripsSubInd, aes(year, median_TripsSubsidyInd)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Trips, median grant/beneficiary")
# 
# #save in one grid 
# 
# summaryStatistics_Subsidy <- plot_grid(totalRealSub, totalRealTripSub, medianRealSub, medianRealTripSub, medianSubInd, medianTripsSubInd, 
#                                        ncol = 2, nrow = 3, align = "vh",
#                                        labels = "AUTO",
#                                        label_x = 0, label_y = 0, hjust = -3, vjust = 
#                                          -1.5, label_fontface = "plain", label_size = 11)
# 
# saveRDS(summaryStatistics_Subsidy, "./ANALYSIS/GRAPHS/PAPER GRAPHS/summaryStatistics_Subsidy.Rds")
# 
# 
# 
