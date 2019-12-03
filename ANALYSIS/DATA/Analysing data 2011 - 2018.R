
# Analysing data 2011 - 2018 ----------------------------------------------

### 1. Time series

# Mean of a variable for every year

# Create a new data set only including the two variables "subsidyReceivedNo" and "year"
df <- data.frame(data201118$subsidy, data201118$year)
View(df)

library(tidyverse)
library(dplyr)

# 1. Möglichkeit: aggregate ()
# Aggregate column 1 of data set df, grouping by df$data201118.year and applying the mean-function
# na.rm = TRUE indicates that NA values are stripped before taking the mean

aggregate(df[,1], list(df$data201118.year), mean, na.rm = TRUE)

# 2. Möglichkeit

# Defining variables "Year" and "Subsidy" as Vektoren by command c()
Year <- c(df$data201118.year)
Subsidy <- c(df$data201118.subsidy)

# Creating a new data frame with the variables "Year" and "Subsidy"
df <- data.frame(Year, Subsidy)


# The operator %>% pass the left hand side of the operator the first argument of the right hand 
# side. Grouping the data set df by the Year with group_by(). The command summarise() create
# a variable "averageSubsidy" which is the mean of "Subsidy".
mean_by_year <- df %>%
  group_by(df$Year) %>% 
  summarise(averagedSubsidy = mean(Subsidy, na.rm = TRUE))

summarise()
mean_by_year <- df %>%
  group_by(df$data201118.year) %>%
  summarise (averaged_subsidy = mean(df$data201118.subsidy, na.rm = TRUE))

mean_by_year <- df %>%
  group_by(df$data201118.year) %>%
  summarise(average = mean(df$data201118.subsidy, na.rm = TRUE))

Mean_by_year <- df %>%
  group_by(df$data201118.year) %>% 
  summarise_at(vars(subsidyReceived_year$data201118.year),average = mean(df$data201118.subsidyReceivedNo, na.rm = TRUE))


subsidyReceived_year %>%
  group_by(subsidyReceived_year$data201118.year) %>%
  summary(subsidyReceived_year$data201118.subsidyReceivedNo)

subsidyReceived_year %>%
  group_by(subsidyReceived_year$data201118.year) %>%
  summarise(average = mean(subsidyReceived_year$data201118.subsidyReceivedNo, na.rm = TRUE))

mean()

mean(dataWithState$subsidyReceivedNo, na.rm = TRUE)


mean


ts.plot(dataWithState$subsidy)

ts_subsidyReceived <- ts(dataWithState$subsidyReceivedNo, start = 2011, end = 2019)
view(ts_subsidyReceived)

ts.plot(ts_subsidyReceived)
