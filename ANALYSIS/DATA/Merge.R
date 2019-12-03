# Load packages
library(dplyr)

# Join data from 2011 to 2019

data201119 <- data2019 %>%
  full_join(df_2018) %>% 
  full_join(df_2017) %>% 
  full_join(data2016) %>%
  full_join(data2015) %>%
  full_join(data2014) %>%
  full_join(data2013) %>%
  full_join(data2012) %>%
  full_join(data2011)

data201119 <- data201119 %>%
  select(year, everything())
  
