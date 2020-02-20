### Regressionen

library(stats)
library(car)
library(estimatr)
library(dplyr)

# 1. Regression: Simple linear regression
# Die Zielvariable y wird nur auf die Treatment-Variable "treatEF" regressiert

lm1_selfworth <- lm_robust(dfcEF$selfworth ~ dfc$treatEF)
summary(lm1_selfworth)

lm1_skills <- lm_robust(dfcEF$dayToDaySkills ~ dfc$treatEF)
summary(lm1_skills)

# 2. Regression: Lineare Regression with year-fixed effects 
# Die lineare Regression wird um year-fixed effects erweitert, indem die Dummy-Variablen für die
# die verschiedenen Jahre berücksichtigt werden.

as.numeric(dfcEF$dummy_2011)

c(dfcEF$dummy_2011) %>% mutate_if(is.character, as.numeric)

lm2_selfworth <- lm_robust((dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2012))
summary(lm2_selfworth)

lm2_selfworth <- lm_robust(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2011 + dfcEF$dummy_2012
                           + dfcEF$dummy_2013 + dfcEF$dummy_2014, dfcEF$dummy_2015, dfcEF$dummy_2016
                           + dfcEF$dummy_2017, data = dfcEF)
summary(lm2_se)



