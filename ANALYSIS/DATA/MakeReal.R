library(dplyr)

#calculating real values as well as differences in subsidy/request and the amount of years in programme

mergedData <- mergedData %>% 
  mutate(realSubsidy = subsidy/foodPriceIndex*100,
         realSubsidyRequest = subsidyRequest/foodPriceIndex*100,
         realTripsSubsidy = tripsSubsidy/tripsPriceIndex*100,
         realTripsSubsidyRequest = tripsSubsidyRequest/tripsPriceIndex*100,
         realTotalCosts = totalCost/foodPriceIndex*100,
         realTotalBudget = totalBudget/priceIndex*100,
         yearsSupportSince = 2020 - supportSince,
         subsidyDifferenceLunch = subsidyRequest - subsidy)