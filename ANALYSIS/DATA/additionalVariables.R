mergedData <- mergedData %>% 
  mutate(realSubsidyPerBeneficiary = realSubsidy/eatersPerMealNo,
         realTripsSubsidyPerBeneficiary = realTripsSubsidy/tripsKidsNo)

# order by id and year

mergedData <- mergedData %>% 
  arrange(id, year)
