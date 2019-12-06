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




