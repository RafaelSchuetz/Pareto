# read packages

library(readxl)
library(tidyverse)

# Gather and cleanup raw data files

# encoding = "UTF8" to avoid error caused by German Umlauts

source(encoding = "UTF8", "./ANALYSIS/DATA/Gather2011_2012.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Data 2013&2014.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Data 2015.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Data 2016.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/2017.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/2018.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Gather2019.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Data Bundeslaender.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/DataPriceIndices.R")

# source(encoding = "UTF8", "./ANALYSIS/DATA/subsidies_Institution-level.R")

# Merge cleaned and augmented data frames into data.frames "lnRealUSFundAnn19502018", "realUSOutp19502018" and "USRecMonth"

source(encoding = "UTF8", "./ANALYSIS/DATA/Merge.R")

# Recode categorical variables falsely coded as numerical

source(encoding = "UTF8", "./ANALYSIS/DATA/categoricalData.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/MakeReal.R")

source(encoding = "UTF8", "./ANALYSIS/DATA/Exclude Outliers Regression.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Standardized Coefficents.R")
