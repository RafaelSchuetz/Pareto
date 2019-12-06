library(dplyr)
library(tidyverse)

subsidy2016 <- read.csv("ANALYSIS/DATA/MittagstischSub2016.txt", header=FALSE)

subsidy2017 <- read.csv("ANALYSIS/DATA/MittagstischSub2017.txt", header=FALSE, dec=",")

#rename

subsidy2016 <- subsidy2016 %>% 
  dplyr::rename(city = 'V1',
                organisation = 'V2',
                subsidy = 'V3')

subsidy2017 <- subsidy2017 %>% 
  dplyr::rename(city = 'V1',
                organisation = 'V2',
                subsidy = 'V3')

subsidy2016 <- subsidy2016 %>% add_column(year = 2016)
subsidy2017 <- subsidy2017 %>% add_column(year = 2017)

subsidy2017$subsidy <- as.numeric(subsidy2017$subsidy)

library(dplyr)
subsidy2016 <- subsidy2016 %>%
  mutate('states')

subsidy2016 <- subsidy2016 %>% 
  dplyr::rename(state = '"states"')

subsidy2017 <- subsidy2017 %>%
  mutate('states')

subsidy2017 <- subsidy2017 %>% 
  dplyr::rename(state = '"states"')

subsidy2016[1,5]<- "NRW"
subsidy2016[2,5]<- "Sachsen"
subsidy2016[3,5]<- "Bayern"
subsidy2016[4,5]<- "NRW" 
subsidy2017[5,5]<- "Berlin"
subsidy2016[6,5]<- "Berlin" 
subsidy2016[7,5]<- "Berlin" 
subsidy2016[8,5]<- "Berlin" 
subsidy2016[9,5]<- "NRW" 
subsidy2016[10,5]<- "NRW" 
subsidy2016[11,5]<- "Niedersachen"
subsidy2016[12,5]<- "Bremen"
subsidy2016[13,5]<- "NRW"
subsidy2016[14,5]<- "NRW"
subsidy2016[15,5]<- "Sachsen"
subsidy2016[16,5]<- "Sachsen"
subsidy2016[17,5]<- "NRW"
subsidy2016[18,5]<- "NRW"
subsidy2016[19,5]<- "Brandenburg"
subsidy2016[20,5]<- "Thüringen"
subsidy2016[21,5]<- "NRW"
subsidy2016[22,5]<- "NRW"
subsidy2016[23,5]<- "Hessen"
subsidy2016[24,5]<- "Hessen"
subsidy2016[25,5]<- "Hessen"
subsidy2016[26,5]<- "Sachsen Anhalt"
subsidy2016[27,5]<- "Hamburg"
subsidy2016[28,5]<- "Hamburg"
subsidy2016[29,5]<- "Niedersachsen"



