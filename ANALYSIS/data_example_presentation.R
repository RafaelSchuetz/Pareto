library(dplyr)

dataPraesi <- mergedData %>% filter(id == "103" | id == "112") %>% 
  dplyr::select(id, year, eatersPerMealNo, selfworth, tripsNo)

dataPraesi <- dataPraesi[-c(1, 2, 3, 7, 8, 9, 10, 11, 15), ]

library(xtable)

dataExample <- print(xtable(dataPraesi), include.rownames=FALSE)


saveRDS(dataExample, "./ANALYSIS/GRAPHS/PAPER/dataExample.Rds")
