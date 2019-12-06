withSub2016 <- data2016 %>% 
  full_join(subsidy2016) 

withSub2017 <- df_2017 %>%
  full_join(subsidy2017)