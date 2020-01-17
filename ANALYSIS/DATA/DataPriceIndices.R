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
  dplyr::rename(tripsPriceIndex = 'Freizeit, Unterhaltung und Kultur',
                foodPriceIndex = 'Nahrungsmittel und alkoholfreie Getränke')



