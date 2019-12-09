library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)

#taking the means of subs in the different states

#create data set consisting of state and subsidy

dfs <- data.frame(dataWithStatesAndSub$state, dataWithStatesAndSub$subsidy)

dfsAggregated <- aggregate(dfs[,2], list(dfs$dataWithStatesAndSub.state), mean)





