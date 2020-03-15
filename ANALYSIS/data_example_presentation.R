library(dplyr)

dataPraesi <- mergedData %>% filter(id == "103" | id == "112") %>% 
  dplyr::select(id, year, eatersPerMealNo, selfworth, tripsNo)

dataPraesi <- dataPraesi[-c(1, 2, 3, 7, 8, 9, 10, 11, 15, 16), ]

library(xtable)

dataExample <- xtable(dataPraesi)

saveRDS(dataExample, "./ANALYSIS/GRAPHS/PAPER/dataExample.Rds")

print(xtable(dataExample, include.rownames = FALSE,
             caption = "Beispielhafter Datensatz",
             align = "lccccc"))


print(xtable(dataExample), include.rownames = FALSE,
      caption = "Beispielhafter Datensatz",
      align = "lccccc",
      label = "fundamentalDynamics")
