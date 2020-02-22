mergedData <- mergedData %>% 
  mutate(realSubsidyPerBeneficiary = realSubsidy/eatersPerMealNo,
         realTripsSubsidyPerBeneficiary = realTripsSubsidy/tripsKidsNo)
