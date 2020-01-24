library(readxl)
library(dplyr)
library(tidyverse)

headers <- c("CC13", "category", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

priceIndices <- read_excel("./ANALYSIS/DATA/Verbraucherpreisindizes_Deutschland.xlsx", col_names = headers, skip = 8, n_max = 11)

priceIndices$CC13 = NULL

priceIndicesLong <- priceIndices %>%
  gather("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", key = year, value = Index)

priceIndicesLong  <- priceIndicesLong %>%  
  filter(category == "Nahrungsmittel und alkoholfreie Getränke" | category == "Freizeit, Unterhaltung und Kultur")


priceIndicesWide <- priceIndicesLong %>% 
  spread(category, Index)

priceIndicesWide$year <- as.numeric(priceIndicesWide$year)

priceIndicesWide <- priceIndicesWide %>% 
  dplyr::rename(PriceIndexTrips = 'Freizeit, Unterhaltung und Kultur',
                PriceIndexFood = 'Nahrungsmittel und alkoholfreie Getränke')



#allgemeinerPreisindex 

headers <- c("year", "priceIndex", "change")

totalPriceIndex <- read_excel("./ANALYSIS/DATA/Allgemeiner_Verbraucherpreisindex.xlsx", col_names = headers, skip = 25, n_max = 8)
totalPriceIndex <- select(totalPriceIndex, 1:2)
totalPriceIndex$year <- as.numeric(totalPriceIndex$year)