library(dplyr)

#calculating real values as well as differences in subsidy/request and the amount of years in programme

mergedData <- mergedData %>% 
  mutate(realSubsidy = subsidy/PriceIndexFood*100,
         realSubsidyRequest = subsidyRequest/PriceIndexFood*100,
         realTripsSubsidy = tripsSubsidy/PriceIndexTrips*100,
         realTripsSubsidyRequest = tripsSubsidyRequest/PriceIndexTrips*100,
         realTotalCosts = totalCost/PriceIndexFood*100,
         realTotalBudget = totalBudget/priceIndex*100,
         yearsSupportSince = year - supportSince,
         subsidyDifferenceLunch = subsidyRequest - subsidy,
         realSubsidyDifferenceLunch = realSubsidyRequest - realSubsidy, 
         realSubsidyPerBeneficiary = realSubsidy/eatersPerMealNo,
         realTripsSubsidyPerBeneficiary = realTripsSubsidy/tripsKidsNo)