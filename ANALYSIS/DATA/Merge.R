# Load packages
library(dplyr)

# Join data from 2011 to 2018

data201118 <- df_2018 %>% #data2019 %>%
  #full_join(df_2018) %>% 
  full_join(df_2017) %>% 
  full_join(data2016) %>%
  full_join(data2015) %>%
  full_join(data2014) %>%
  full_join(data2013) %>%
  full_join(data2012) %>%
  full_join(data2011)

data201118 <- data201118 %>%
  select(year, everything())
  
# Join the data with Bundesland

dataWithStates <- merge(dataBundeslaender, data201118, all.x = TRUE, all.y = TRUE)

# Einrichtungsnummern, die keinem Bundesland und "Förderung seit" zugeordnet werden können:
# 113 (2011-13), 137 (2011), 190 (2011, 2012), 219 (2011-2016), 226 (2011-13)
# Dabei handelt es sich um Einrichtungen, die aktuell nicht mehr gefördert werden und zu denen 
# somit auch keine Information bezüglich Bundesland und "Förderung seit" vorliegen.

# break anonymization

# withSub2015 <- data2015 %>% 
#   full_join(subsidy2016) 

# withSub2016 <- data2016 %>%
#   full_join(subsidy2017) 

#joinwithstatedata

# dataWithStatesAndSubsidy <- dataWithStates %>%
#   full_join(subsidy2016) %>%
#   full_join(subsidy2017)

mergedData <- dataWithStates
mergedData <- mergedData %>% 
  full_join(priceIndicesWide) %>%
  full_join(totalPriceIndex)