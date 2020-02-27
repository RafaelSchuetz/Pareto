# read packages


library(texreg)
library(readxl)
library(tidyverse)
library(Scale)
library(psych)
library(Hmisc)
library(lattice)
library(survival)
library(Formula)
library(tidyimpute)
library(tidyselect)
library(imputeMissings)
library(zoo)
library(rlang)
library(stargazer)
library(estimatr)
library(ggplot2)
library(stats)
library(cowplot)
library(ggpubr)
library(polycor)
library(lavaan)


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

source(encoding = "UTF8", "./ANALYSIS/DATA/MakeReal.R")

source(encoding = "UTF8", "./ANALYSIS/DATA/Impute.R")

source(encoding = "UTF8", "./ANALYSIS/DATA/Exclude Outliers Regression.R")

# Recode categorical variables falsely coded as numerical

source(encoding = "UTF8", "./ANALYSIS/DATA/categoricalData.R")

# specialVariables has to be after categoricalData

source(encoding = "UTF8", "./ANALYSIS/DATA/specialVariables.R")



# source(encoding = "UTF8", "./ANALYSIS/DATA/Standardized Coefficents.R")

# mergedData$realSubsidy*mergedData$lessIll*0.25
# 
# mergedData$realSubsidyPerBeneficiary*mergedData$lessIll_weighted
# 
# mergedData$realSubsidy/mergedData$eatersPerMealNo
# 
# mergedData$realSubsidyPerBeneficiary
# 
# mergedData$lessIll*0.25*mergedData$eatersPerMealNo
# 
# mergedData$lessIll*0.25*mergedData$tripsKidsNo
# 
# mergedData$lessIll_weighted
# 
# weight(mergedData$lessIll)

# name <- quo_name(enquo(expr))
# if(stringr::str_detect(name, "trips"))

# lm(lessIll_scaled ~ realSubsidy, mergedData)
# lm(lessIll ~ realSubsidyPerBeneficiary, mergedData)
