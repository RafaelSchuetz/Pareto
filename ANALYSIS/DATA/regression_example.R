########################################################################################
# 'texreg' package addition: definition of an extract function for regression output
#                          from the 'rdrobust' package
# Author: Leonhard Vollmer 
# Date: 2019-06-12 (highly preliminary, please do not share without permission from the author)
########################################################################################

# Based on this info: https://stackoverflow.com/questions/38894044/print-pretty-tables-for-h2o-models-in-r/39135080#39135080 



# Start a new R Session and clear the environment
rm(list = ls())

# install necessary packages

install.packages('devtools')
library(devtools) # load devtools package

devtools::install_github("leifeld/texreg") # get developer versions of packages directly from e.g. github, in this case: https://github.com/leifeld/texreg
library(texreg)



# WRITE AN EXTRACT FUNCTION S.T. RDROBUST OUTPUT IS TRANSFORMED INTO A TEXREG OBJECT

extract.rdrobust <- function(model, 
                             coef.conventional = TRUE, 
                             coef.bias.corrected = FALSE,
                             se.conventional = TRUE,
                             se.robust = FALSE,
                             pv.conventional = TRUE,
                             pv.bias.corrected = FALSE,
                             pv.robust = FALSE,
                             gof.pv.conventional = FALSE,
                             gof.pv.bias.corrected = FALSE,
                             gof.pv.robust = FALSE,
                             N = FALSE,
                             N.left = FALSE,
                             N.right = FALSE,
                             N.h = TRUE,
                             N.h.left = FALSE,
                             N.h.right = FALSE,
                             pol.order = FALSE,
                             bandwidth = TRUE,
                             bandwidth.bias.corrected = FALSE,
                             ...
) {
  
  # Coefficients
  # create empty vectors and subsequently add info from model
  coefnames <- character()
  coefs <- numeric()
  ses <- numeric()
  pvs <- numeric()
  if (coef.conventional == TRUE) {
    coefnames <- c(coefnames, "Conventional")
    coefs <- c(coefs, model$coef[[1]])
  }
  if (coef.bias.corrected == TRUE) {
    coefnames <- c(coefnames, "Bias corrected")
    coefs <- c(coefs, model$coef[[2]])
  } 
  if (se.conventional == TRUE) {
    ses <- c(ses, model$se[[1]])
  }
  if (se.robust == TRUE) {
    ses <- c(ses, model$se[[3]])
  }
  if (pv.conventional == TRUE) {
    pvs <- c(pvs, model$pv[[1]])
  }
  if (pv.bias.corrected == TRUE) {
    pvs <- c(pvs, model$pv[[2]])
  }
  if (pv.robust == TRUE) {
    pvs <- c(pvs, model$pv[[3]])
  }
  
  # Goodness of fit statistics (GOF)
  # create empty GOF vectors and subsequently add GOF statistics from model:
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (gof.pv.conventional == TRUE) {
    pv.c <- model$pv[[1]]
    gof <- c(gof, pv.c)
    gof.names <- c(gof.names, "Conventional p-value")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (gof.pv.bias.corrected == TRUE) {
    pv.bc <- model$pv[[2]]
    gof <- c(gof, pv.bc)
    gof.names <- c(gof.names, "Bias-corrected p-value")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (gof.pv.robust == TRUE) {
    pv.r <- model$pv[[3]]
    gof <- c(gof, pv.r)
    gof.names <- c(gof.names, "Robust p-value")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (N == TRUE) {
    obs <- model$N[[1]] + model$N[[2]]
    gof <- c(gof, obs)
    gof.names <- c(gof.names, "Total observations")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (N.left == TRUE) {
    obs <- model$N[[1]]
    gof <- c(gof, obs)
    gof.names <- c(gof.names, "Total obs. left")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (N.right == TRUE) {
    obs <- model$N[[2]]
    gof <- c(gof, obs)
    gof.names <- c(gof.names, "Total obs. right")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (N.h == TRUE) {
    obs <- model$Nh[[1]] + model$Nh[[2]]
    gof <- c(gof, obs)
    gof.names <- c(gof.names, "Effective observations")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (N.h.left == TRUE) {
    obs <- model$Nh[[1]]
    gof <- c(gof, obs)
    gof.names <- c(gof.names, "Effective obs. left")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (N.h.right == TRUE) {
    obs <- model$Nh[[2]]
    gof <- c(gof, obs)
    gof.names <- c(gof.names, "Effective obs. right")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (pol.order == TRUE) {
    pol.o <- model$p
    gof <- c(gof, pol.o)
    gof.names <- c(gof.names, "Polynomial order")
    gof.decimal <- c(gof.decimal, FALSE)
  }
  if (bandwidth == TRUE) {
    bw.l <- model$bws[[1,1]]
    bw.r <- model$bws[[1,2]]
    if (bw.l == bw.r) {
      gof <- c(gof, bw.l)
      gof.names <- c(gof.names, "Bandwidth")
    }
    if (bw.l != bw.r) {
      gof <- c(gof, bw.l, bw.r)
      gof.names <- c(gof, "Bandwidth left", "Bandwidth, right")
    }
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (bandwidth.bias.corrected == TRUE) {
    bw.l <- model$bws[[2,1]]
    bw.r <- model$bws[[2,2]]
    if (bw.l == bw.r) {
      gof <- c(gof, bw.l)
      gof.names <- c(gof.names, "Bandwidth, bias corrected")
    }
    if (bw.l != bw.r) {
      gof <- c(gof, bw.l, bw.r)
      gof.names <- c(gof, "BW left, bias corrected", "BW, right, bias corrected")
    }
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  
  # create texreg object
  tr <- createTexreg(
    coef.names = coefnames, 
    coef = coefs, 
    se = ses,
    pvalues = pvs,
    gof.names = gof.names, 
    gof = gof, 
    gof.decimal = gof.decimal
  )
  return(tr)
}


# register new extract function so that texreg knows what to do
setMethod("extract", signature = className("rdrobust", "rdrobust"), 
          definition = extract.rdrobust)







########################################################################################
# REPLICATION SCRIPT FOR PONS & TRICAUD (2018)
########################################################################################


# 1. Preparations ---------------------------------------------------------

# Start a new R Session and clear the environment
rm(list = ls())

########################################################################################
# NOTE: remember that you need to run the accompanying script ('texreg_rdrobust_extract_function_LV.R')
#       in the same R session before you start here
########################################################################################


# Install packages 
# You only have to do this once; if you've already installed it, simply jump to the next step
install.packages('tidyverse') 
install.packages('rdrobust')
install.packages('rdd')
install.packages('haven') 
install.packages('labelled')
install.packages('ggplot2')
install.packages('ggpubr')
install.packages('stargazer')
install.packages('devtools')
install.packages('pander')

# NOTE: for the installation of 'texreg' see the Rscript 'texreg_rdrobust_extract_function_LV.R'


# Load packes (so you can use them in your code)
library('tidyverse')
library('rdrobust') # to run RDDs
library('rdd') # another package to run RDDs
library('haven') # to import Stata files
library('labelled') # to deal with variable labels from imported Stata files
library('ggplot2') # useful plotting functions
library('ggpubr') # addon to ggplot2 for 'publication-ready graphics'
library('stargazer') # to produce regression tables
library('texreg') # to produce regression tables


# Load the data
# This step is folder-structure specific
# It is always advisable to use 'relative paths', which facilitate collaboration
# 'R Projects' are handy: the folder in which they sit is automatically defined as your 'working directory'
# You can check what your current working directory is by running:
getwd()
# If you wish to manually set your working directory, use e.g.:
setwd('C:/Users/Jonathan Kirschner/Documents/Studium/4. Semester/SCSS/session_3/SCSS_lecture_3')
# ... but as I said, using R Project already conveniently sets a sensible wd

# Finally load the data ...
df <- read_dta('SCSS_lecture_3/pons_tricaud_replication/pons_tricaud_analysis.dta')

# ... this dataset has 670 variables -- we'll need just a tiny fraction of those
df <- df %>% 
  select(year, election_type, id_district, id_unique, 
         treatment, running,
         prop_registered_turnout_R2,
         prop_registered_blanknull_R2,
         prop_registered_candvotes_R2,
         prop_registered_votes_top2_R2)


# Maybe save the variable names and labels in a separate data frame for (later) inspection
varslabels <- as_tibble(var_label(df, unlist = T)) %>% 
  rownames_to_column() %>% 
  rename(variable = rowname,
         label = value)



# 2. Figure 1 -- First Stage ----------------------------------------------

# let's understand the data: how many elections have a 3rd candidate
summary(df$treatment)
sum(df$treatment == 1)
sum(df$treatment == 0)

summary(df$running)
hist(df$running)    

ggplot(df, aes(x=running)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 0.005) +
  geom_vline(aes(xintercept=0),
             color="red", linetype="dashed", size=0.5)


# Let's re-create Figure 1 in the paper, which illustrates the first stage: 
# does the instrument predict treatment?    


?rdplot

rdplot(y = df$treatment, x = df$running, p = 1, nbins = c(20, 20),
       title = "", y.label = "Treatment status", x.label = "Running variable",
       x.lim = c(-.1, .1), y.lim = c(0, 1)
)    



# 3. Figure 2 -- Impact on participation and candidate votes --------------

# To combine graphs
layout(matrix(c(1,1,2,2,0,3,3,0), nrow = 2, ncol = 4, byrow = TRUE))


# Outcome 1: "Turnout 2nd round" -- variable name: prop_registered_turnout_R2
summary(df$prop_registered_turnout_R2)

# Plot
rdplot(y = df$prop_registered_turnout_R2, x = df$running, p = 2, nbins = c(30, 30),
       title = "", y.label = "Turnout 2nd round", x.label = "Running variable",
       x.lim = c(-.15, .15), y.lim = c(.2, 1)
)



# Outcome 2: "Null and blank votes 2nd round" -- variable name: prop_registered_blanknull_R2     
summary(df$prop_registered_blanknull_R2)

# Plot
rdplot(y = df$prop_registered_blanknull_R2, x = df$running, p = 2, nbins = c(30, 30),
       title = "", y.label = "Null and blank votes 2nd round", 
       x.label = "Running variable",
       x.lim = c(-.15, .15), y.lim = c(0, 0.8)
)


# Outcome 3: "Null and blank votes 2nd round" -- variable name: prop_registered_blanknull_R2     
summary(df$prop_registered_candvotes_R2)

# Plot
rdplot(y = df$prop_registered_candvotes_R2, x = df$running, p = 2, nbins = c(30, 30),
       title = "", y.label = "Candidate votes 2nd round", 
       x.label = "Running variable",
       x.lim = c(-.15, .15), y.lim = c(.2, 1)
)



# 4. Figure 3 -- Impact on votes going to the top two candidates ----------
layout(matrix(c(1), ncol = 1, nrow = 1))

summary(df$prop_registered_votes_top2_R2)
hist(df$prop_registered_votes_top2_R2)

rdplot(y = df$prop_registered_votes_top2_R2, x = df$running, p = 2, nbins = c(30, 30),
       title = "", y.label = "Top2 candidates 2nd round", 
       x.label = "Running variable",
       x.lim = c(-.15, .15), y.lim = c(.2, 1)
)

?rdplot

# 5. Table V -- Impact on votes going to the top two candidates -----------

# using xtable

?rdrobust

t5_1 <- rdrobust(y = df$prop_registered_votes_top2_R2, x = df$running, 
                 fuzzy = df$treatment)

t5_1

summary(t5_1)

# COMPLICATION: calculate the IK bandwidth using 'IKbndwidth' from the 'rdd' package 
# in the 'rdrobust' package you can't calculate the IK bandwidth anymore
?IKbandwidth

t5_ik <- IKbandwidth(X = df$running, Y = df$prop_registered_votes_top2_R2)

t5_2 <- rdrobust(y = df$prop_registered_votes_top2_R2, x = df$running, 
                 fuzzy = df$treatment, h = t5_ik)

summary(t5_2)

t5_3 <- rdrobust(y = df$prop_registered_votes_top2_R2, x = df$running, 
                 fuzzy = df$treatment, p = 2)

summary(t5_3)

t5_4 <- rdrobust(y = df$prop_registered_votes_top2_R2, x = df$running, 
                 fuzzy = df$treatment, h = t5_ik, p = 2)

summary(t5_4)

?rdrobust
?texreg

t5_1$bws
t5_1$bws[[2,1]]
t5_1$bws[[2,2]]

# means left of threshold
# model 1
mean_1 <- mean(df$prop_registered_votes_top2_R2[df$running < 0 & df$running > -t5_1$bws[[1,1]]])

# model 2
mean_2 <- mean(df$prop_registered_votes_top2_R2[df$running < 0 & df$running > -t5_ik])

# model 3
mean_3 <- mean(df$prop_registered_votes_top2_R2[df$running < 0 & df$running > -t5_3$bws[[1,1]]])

# model 4
mean_4 <- mean(df$prop_registered_votes_top2_R2[df$running < 0 & df$running > -t5_ik])


# how does texreg/screenreg/htmlreg work?
?texreg

screenreg(t5_1)

screenreg(list(t5_1, t5_2, t5_3, t5_4))

# to understand some of the options used below (e.g. 'pv.robust') you'll have to
# look at the accompanying code I sent you that allows texreg to interpret the
# output of rdrobust estimations (cf. texreg_rdrobust_extract_function_LV.R)

htmlreg(list(t5_1, t5_2, t5_3, t5_4), 
        stars = c(0.01, 0.05, 0.1),
        digits = 3,
        pv.conventional = F,
        pv.robust = T,
        pol.order = T,
        gof.pv.robust = T,
        custom.coef.names = c("3rd present"),
        custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
        custom.gof.rows = list("Band. method" = c("MSERD", "IK", "MSERD", "IK"), # this won't work for those who haven't installed the developer version, as the 'custom.gof
                               "Mean, left of threshold" = c(mean_1, mean_2, mean_3, mean_4)), # this won't work for those who haven't installed the developer version
        reorder.gof = c(3,4,5,6,1,2),
        file = "pons_tricaud_replication/table_V.html"
)




# no read-made multicolumn option yet -- but they are working on it :(
# but there is a workaround if you work with latex


