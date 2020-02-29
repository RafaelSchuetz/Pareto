#Real Subsidy Time Trend Total &Median


# Inititial summary statistics about 

modelVariables <- mergedData %>% 
  dplyr::select(all_of(modelVariablesNames)) %>% 
  data.frame()

modelVariablesSummary <- stargazer(modelVariables)

saveRDS(modelVariablesSummary, './ANALYSIS/Tables/modelVariablesSummary.Rds')

###total trips subsidy real 

realTripsSubTotal <- mergedData %>% 
  group_by(year)%>%
  dplyr::summarize(total_TripsSubsidy=sum(realTripsSubsidy, na.rm = TRUE))%>%
  dplyr::filter(!(total_TripsSubsidy == 0.0))


#plot the linear time trend of total real trips subsidy trend with ggplot 
totalRealTripSub <- ggplot(realTripsSubTotal, aes(year, total_TripsSubsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x = "Year", y = "Trips, total")

saveRDS(totalRealTripSub, './ANALYSIS/Tables/totalRealTripSub.Rds')

###median trips subsidy real 

medianRealTripsSubTotal <- mergedData %>%
  group_by(year)%>%
  dplyr::summarize(Median_TripsSubsidy=median(realTripsSubsidy, na.rm = TRUE))%>%
  dplyr::filter(!(Median_TripsSubsidy == 0.0))

#plot the linear time trend of median real trips subsidy trend with ggplot 
medianRealTripSub <- ggplot(medianRealTripsSubTotal, aes(year, Median_TripsSubsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y = "Trips, median")

saveRDS(medianRealTripSub, './ANALYSIS/Tables/medianRealTripSub.Rds')

#total lunch subsidy 
realSubTotal <- mergedData %>% 
  group_by(year)%>%
  dplyr::summarize(total_Subsidy=sum(realSubsidy, na.rm = TRUE))

#plot the linear time trend of total real subsidy trend with ggplot 
totalRealSub <- ggplot(realSubTotal, aes(year, total_Subsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, total")

saveRDS(totalRealSub, './ANALYSIS/Tables/totalRealSub.Rds')

#median lunch subsidy 

medianRealSub <- mergedData %>%
  group_by(year)%>%
  dplyr::summarize(median_Subsidy=median(realSubsidy, na.rm = TRUE))
  
#plot the linear time trend of median real trips subsidy trend with ggplot 
medianRealSub <- ggplot(medianRealSub, aes(year, median_Subsidy)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, median")

saveRDS(medianRealSub, './ANALYSIS/Tables/medianRealSub.Rds')

###per individual 

#lunch
#median

medianRealSubInd <- mergedData %>%
  group_by(year)%>%
  dplyr::summarize(median_SubsidyInd=median(realSubsidyPerBeneficiary, na.rm = TRUE))

#plot the linear time trend of median real trips subsidy per ind trend with ggplot 
medianSubInd <- ggplot(medianRealSubInd, aes(year, median_SubsidyInd)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Meals, median grant/beneficiary")

saveRDS(medianSubInd, './ANALYSIS/Tables/medianSubInd.Rds')

#trips 

#median
medianRealTripsSubInd <- mergedData %>%
  group_by(year)%>%
  dplyr::summarize(median_TripsSubsidyInd=median(realTripsSubsidyPerBeneficiary, na.rm = TRUE))%>%
  dplyr::filter(!(median_TripsSubsidyInd == 0.0))

#plot the linear time trend of median real trips subsidy per ind trend with ggplot 
medianTripsSubInd <- ggplot(medianRealTripsSubInd, aes(year, median_TripsSubsidyInd)) + geom_line() + geom_smooth(method = "lm") + theme_cowplot(12) + labs(x= "Year", y= "Trips, median grant/beneficiary")

saveRDS(medianTripsSubInd, './ANALYSIS/Tables/medianTripsSubInd.Rds')

#save in one grid 

summaryStatistics_Subsidy <- plot_grid(totalRealSub, totalRealTripSub, medianRealSub, medianRealTripSub, medianSubInd, medianTripsSubInd, 
                                       ncol = 2, nrow = 3, align = "vh",
                                       labels = "AUTO",
                                       label_x = 0, label_y = 0, hjust = -3, vjust = 
                                         -1.5, label_fontface = "plain", label_size = 11)

saveRDS(summaryStatistics_Subsidy, "./ANALYSIS/GRAPHS/PAPER/summaryStatistics_Subsidy.Rds")


#number of beneficiaries & organisations

organisations_beneficiaries <- mergedData%>%
  group_by(year)%>%
  dplyr::summarize(Lunch_KidsNo=sum(eatersPerMealNo, na.rm = TRUE),trips_KidsNo=sum(tripsKidsNo, na.rm = TRUE), Lunch_OrganisationsNo = n_distinct(id))
#trips no
TripsOrgaData <- mergedData %>%
  dplyr::filter(!(tripsKidsNo == 0 | is.na(tripsKidsNo)))

TripsOrganisationsNumber <- TripsOrgaData%>%
  group_by(year)%>%
  dplyr::summarise(Trips_OrganisationNo = n_distinct(id))

#join 
organisationsbeneficaries <- organisations_beneficiaries %>% 
  full_join(TripsOrganisationsNumber) 

organisationsbeneficaries[1,3] <- NA

organisationsbeneficaries <- data.frame(organisationsbeneficaries)

saveRDS(organisationsbeneficaries,"./ANALYSIS/Tables/fundamental_dynamics.Rds")

