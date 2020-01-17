library(readxl)
library(dplyr)
library(tidyverse)

headers <- c("CC13", "category", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

priceIndices <- read_excel("./ANALYSIS/DATA/Verbraucherpreisindizes_Deutschland.xlsx", col_names = headers, skip = 8, n_max = 11)

foodIndices <- subset(priceIndices, category == "Nahrungsmittel und alkoholfreie Getränke", 2:10)

tripsIndices <- subset(priceIndices, category == "Freizeit, Unterhaltung und Kultur", 2:10)

unorderedPriceIndices <- foodIndices %>% 
  full_join(tripsIndices)

relevIndices <- data.frame("tripIndex" = c(91.7, 92.5, 94.8, 96.1, 100, 100.7, 102.1, 103.4), "foodIndex" = c(91.6, 94.7, 98.3, 99.4, 100, 100.8, 103.6, 106.0), "year" = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

mergedData <- mergedData %>% add_column(tripIndex=NA)
mergedData <- mergedData %>% add_column(foodIndex=NA) 
mergedData <- mergedData %>% add_column(realSubsidy=NA)
mergedData <- mergedData %>% add_column(realSubsidyRequest=NA)
mergedData <- mergedData %>% add_column(realTripsSubsidy=NA)
mergedData <- mergedData %>% add_column(realTripsSubsidyRequest=NA) 

mergedDataReal <- mergedData %>% 
  full_join(relevIndices)