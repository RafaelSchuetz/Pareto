#### Make file for the DiD estimations ####

### Joining the required R scripts

# Preparation of defining the treatment and control group
source(encoding = "UTF8", "./ANALYSIS/DATA/control_gruppe.R")

# Definition of the second treatment variable
source(encoding = "UTF8", "./ANALYSIS/DATA/treat_dummy.R")

# Generate dummy variables for every organization ID
source(encoding = "UTF8", "./ANALYSIS/DATA/control_IDFixedEffects.R")

# DiD-estimation and regression tables
source(encoding = "UTF8", "./ANALYSIS/DATA/did_estimation_final.R")



#### Make file for figure 5 and 6 (Section 6.1.3) and figure 7 and 8 (Appendix A4)

# Preparation of defining the treatment and control group
source(encoding = "UTF8", "./ANALYSIS/DATA/control_gruppe.R")

# Time series of different variables
source(encoding = "UTF8", "./ANALYSIS/DATA/advanced_timeseries.R")

# Creating plots
source(encoding = "UTF8", "./ANALYSIS/DATA/advanced_timeseries_ggplot.R")


