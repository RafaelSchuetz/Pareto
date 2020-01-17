library(dplyr)

mergedData <- mergedData %>% 
  mutate(realSubsidy = subsidy/foodPriceIndex*100,
         realSubsidyRequest = subsidyRequest/foodPriceIndex*100,
         realTripsSubsidy = tripsSubsidy/tripsPriceIndex*100,
         realTripsSubsidyRequest = tripsSubsidyRequest/tripsPriceIndex*100)