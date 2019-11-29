# Gather and cleanup raw data files

# encoding = "UTF8" to avoid error caused by German Umlauts

source(encoding = "UTF8", "./ANALYSIS/DATA/Gather2012_2013.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/2014_2015.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Wirkungsdaten 2016.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Wirkungsdaten 2017.R")
source(encoding = "UTF8", "./ANALYSIS/DATA/Gather2020.R")

# Merge cleaned and augmented data frames into data.frames "lnRealUSFundAnn19502018", "realUSOutp19502018" and "USRecMonth"

source(encoding = "UTF8", "./ANALYSIS/DATA/Merge.R")