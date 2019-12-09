library(dplyr)
library(tidyverse)

subsidy2016 <- read.csv("ANALYSIS/DATA/MittagstischSub2016.txt", header=FALSE)

subsidy2017 <- read.csv("ANALYSIS/DATA/MittagstischSub2017.txt", header=FALSE, stringsAsFactors=FALSE)

#rename

subsidy2016 <- subsidy2016 %>% 
  dplyr::rename(city = 'V1',
                organisation = 'V2',
                subsidy = 'V3')

subsidy2017 <- subsidy2017 %>% 
  dplyr::rename(city = 'V1',
                organisation = 'V2',
                subsidy = 'V3')

subsidy2016 <- subsidy2016 %>% add_column(year = 2015)
subsidy2017 <- subsidy2017 %>% add_column(year = 2016)

subsidy2017$subsidy <- as.numeric(subsidy2017$subsidy)

#newcolumnsforstate

subsidy2016 <- subsidy2016 %>%
  mutate('states')

subsidy2016 <- subsidy2016 %>% 
  dplyr::rename(state = '"states"')

subsidy2017 <- subsidy2017 %>%
  mutate('states')

subsidy2017 <- subsidy2017 %>% 
  dplyr::rename(state = '"states"')

# newcolumnvalues2016

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
subsidy2016[11,5]<- "Niedersachsen"
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
subsidy2016[30,5]<- "Ba-Wü"
subsidy2016[31,5]<- "Hessen"
subsidy2016[32,5]<- "Schleswig-Holstein"
subsidy2016[33,5]<- "NRW"
subsidy2016[34,5]<- "NRW"
subsidy2016[35,5]<- "NRW"
subsidy2016[36,5]<- "Bayern"
subsidy2016[37,5]<- "Sachsen"
subsidy2016[38,5]<- "Schleswig-Holstein"
  subsidy2016[39,5]<- "Ba-Wü"
  subsidy2016[40,5]<- "Ba-Wü"
  subsidy2016[41,5]<- "Sachsen"
  subsidy2016[42,5]<- "Bayern"
  subsidy2016[43,5]<- "Bayern"
  subsidy2016[44,5]<- "Bayern"
  subsidy2016[45,5]<- "Bayern"
  subsidy2016[46,5]<- "Bayern"
  subsidy2016[47,5]<- "Niedersachsen"
  subsidy2016[48,5]<- "Bayern"
  subsidy2016[49,5]<- "Saarland"
  subsidy2016[50,5]<- "Saarland"
  subsidy2016[51,5]<- "Ba-Wü"
  subsidy2016[52,5]<- "Ba-Wü"
  subsidy2016[53,5]<- "Thüringen"
  subsidy2016[54,5]<- "Bayern"
  
  #newcolumnvaluesstates2017
  
  subsidy2017[1,5]<- "NRW"
    subsidy2017[2,5]<- "Sachsen"
    subsidy2017[3,5]<- "Bayern"
    subsidy2017[4,5]<- "NRW"
    subsidy2017[5,5]<- "Berlin"
    subsidy2017[6,5]<- "Berlin"
    subsidy2017[7,5]<- "Berlin"
    subsidy2017[8,5]<- "Berlin"
    subsidy2017[9,5]<- "NRW"
    subsidy2017[10,5]<- "NRW"
    subsidy2017[11,5]<- "Niedersachsen"
    subsidy2017[12,5]<- "Bremen"
    subsidy2017[13,5]<- "Hessen"
    subsidy2017[14,5]<- "NRW"
    subsidy2017[15,5]<- "NRW"
    subsidy2017[16,5]<- "Sachsen"
    subsidy2017[17,5]<- "Sachsen"
    subsidy2017[18,5]<- "NRW"
    subsidy2017[19,5]<- "NRW"
    subsidy2017[20,5]<- "NRW"
    subsidy2017[21,5]<- "Brandenburg"
    subsidy2017[22,5]<- "Thüringen"
    subsidy2017[23,5]<- "NRW"
    subsidy2017[24,5]<- "NRW"
    subsidy2017[25,5]<- "Hessen"
    subsidy2017[26,5]<- "Hessen"
    subsidy2017[27,5]<- "Hessen"
    subsidy2017[28,5]<- "Sachsen-Anhalt"
    subsidy2017[29,5]<- "Hamburg"
    subsidy2017[30,5]<- "Hamburg"
    subsidy2017[31,5]<- "Niedersachsen"
    subsidy2017[32,5]<- "Bayern"
    subsidy2017[33,5]<- "Ba-Wü"
    subsidy2017[34,5]<- "Kassel"
    subsidy2017[35,5]<- "Schleswig-Holstein"
    subsidy2017[36,5]<- "NRW"
    subsidy2017[37,5]<- "NRW"
    subsidy2017[38,5]<- "NRW"
    subsidy2017[39,5]<- "Bayern"
    subsidy2017[40,5]<- "Sachsen"
    subsidy2017[41,5]<- "Schleswig-Holstein"
    subsidy2017[42,5]<- "Ba-Wü"
    subsidy2017[43,5]<- "Ba-Wü"
    subsidy2017[44,5]<- "Sachsen"
    subsidy2017[45,5]<- "Bayern"
    subsidy2017[46,5]<- "Bayern"
    subsidy2017[47,5]<- "Bayern"
    subsidy2017[48,5]<- "Bayern"
    subsidy2017[49,5]<- "Bayern"
    subsidy2017[50,5]<- "Niedersachsen"
    subsidy2017[51,5]<- "Bayern"
    subsidy2017[52,5]<- "MV"
    subsidy2017[53,5]<- "Saarland"
    subsidy2017[54,5]<- "Saarland"
    subsidy2017[55,5]<- "Ba-Wü"
    subsidy2017[56,5]<- "Ba-Wü"
    subsidy2017[57,5]<- "Thüringen"
    subsidy2017[58,5]<- "Bayern"
    




