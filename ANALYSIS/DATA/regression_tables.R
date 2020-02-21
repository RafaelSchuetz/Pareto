#Regressionstable erstellen

library(stargazer)
#Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
#R package version 5.2.2. https://CRAN.R-project.org/package=stargazer 

lm1_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF)

lm1_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF)

#cc


stargazer(lm1_selfworth, lm1_skills,
          title = "Results",
          align = TRUE,
          type = 'text')


### 1. Regression: Simple linear regression

# Selfworth as dependent variable
lm1_selfworth <- lm_robust(dfcEF$selfworth ~ dfcEF$treatEF)
summary(lm1_selfworth)

# Day-To-Day-Skills as dependent variable
lm1_skills <- lm_robust(dfcEF$dayToDaySkills ~ dfcEF$treatEF)
summary(lm1_skills)


### 2. Regression: Linear regression with year fixed effects

# Selfworth as dependent variable
lm2_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                    + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                    + dfcEF$dummy_2017 + dfcEF$dummy_2018)
summary(lm2_selfworth)

# Day-To-Day-Skills as dependent variable
lm2_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                 + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                 + dfcEF$dummy_2017 + dfcEF$dummy_2018)
summary(lm2_skills)


### 3. Regression: Linear regression with id fixed effects

# Selfworth as dependent variable 
lm3_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id)
summary(lm3_selfworth)

# Day-To-Day-Skills as dependent variable
lm3_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id)
summary(lm3_skills)


### 4. Regression: Linear Regression with id FE and year FE

# Selfworth as dependent variable
lm4_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012 
                    + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                    + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018)
summary(lm4_selfworth)

# Day-To-Day-Skills as dependent variable
lm4_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012
                 + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                 + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018)
summary(lm4_skills)


