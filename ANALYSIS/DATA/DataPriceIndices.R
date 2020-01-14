library(readxl)

headers <- c("CC13", "category", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

priceIndices <- read_excel("./ANALYSIS/DATA/Verbraucherpreisindizes_Deutschland.xlsx", col_names = headers, skip = 8, n_max = 11)

