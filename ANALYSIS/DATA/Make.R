# Gather and cleanup raw data files

source("./ANALYSIS/DATA/Gather2012_2013.R")
source("./Analysis/Data/2014_2015.R")
source("./Analysis/Data/Gather2016.R")
source("./Analysis/Data/Gather2017.R")
source("./Analysis/Data/Gather2020.R")

# Merge cleaned and augmented data frames into data.frames "lnRealUSFundAnn19502018", "realUSOutp19502018" and "USRecMonth"

source("./ANALYSIS/DATA/Merge.R")