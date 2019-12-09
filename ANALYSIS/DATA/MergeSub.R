withSub2015 <- data2015 %>% 
  full_join(subsidy2016) 

withSub2016 <- data2016 %>%
  full_join(subsidy2017) 

#joinwithstatedata

dataWithStatesAndSub <- dataWithState %>%
  full_join(subsidy2016) %>%
  full_join(subsidy2017)