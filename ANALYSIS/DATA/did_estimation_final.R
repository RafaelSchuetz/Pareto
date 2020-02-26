### DiD-Schätzung final ###

# Laden der benötigten Packages

library(estimatr)
library(robustbase)
library(tidyverse)
library(sandwich)
library(lmtest)
library(broom)
library(zoo)
library(dplyr)
library(stargazer)
library(lfe)
library(Matrix)
library(texreg)


#### Everyday expertise ####


#### Erste Definition der Treatment Variable ####

# Verwendeter Datensatz: dfcEF2

# DiD-Estimation mit id fixed effects and year fixed effects

lmdid_dayToDaySkills_1 <-  felm(dfcEF2$dayToDaySkills_scaled ~ dfcEF2$treatEF + dfcEF2$id + dfcEF2$year)

summary(lmdid_dayToDaySkills_1)
summary(lmdid_dayToDaySkills_1, robust = TRUE)

RSE_1 = coef(summary(lmdid_dayToDaySkills_1, robust = TRUE))[, "Robust s.e"]
RpValue_1 = coef(summary(lmdid_dayToDaySkills_1, robust = TRUE))[, "Pr(>|t|)"]


# Mit organization-specific Kontrollvariablen

lmdid_dayToDaySkills_2 <-  felm(dfcEF2$dayToDaySkills_scaled ~ dfcEF2$treatEF + dfcEF2$id + dfcEF2$year
                                + dfcEF2$subsidy + dfcEF2$totalCost + dfcEF2$weeklyCooks_scaled)

summary(lmdid_dayToDaySkills_2)
summary(lmdid_dayToDaySkills_2, robust = TRUE)

RSE_2 = coef(summary(lmdid_dayToDaySkills_2, robust = TRUE))[, "Robust s.e"]
RpValue_2 = coef(summary(lmdid_dayToDaySkills_2, robust = TRUE))[, "Pr(>|t|)"]



#### Zweite Definition der Treatment Variable ####

# Verwendeter Datensatz: dfcEF

# DiD-Estimation mit id fixed effects and year fixed effects

lmdid_dayToDaySkills_3 <-  felm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year)

summary(lmdid_dayToDaySkills_3)
summary(lmdid_dayToDaySkills_3, robust = TRUE)

RSE_3 = coef(summary(lmdid_dayToDaySkills_3, robust = TRUE))[, "Robust s.e"]
RpValue_3 = coef(summary(lmdid_dayToDaySkills_3, robust = TRUE))[, "Pr(>|t|)"]


# Mit organization-specific Kontrollvariablen

lmdid_dayToDaySkills_4 <-  felm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id+ dfcEF$year
                                + dfcEF$subsidy + dfcEF$totalCost + dfcEF$weeklyCooks_scaled)

summary(lmdid_dayToDaySkills_4)
summary(lmdid_dayToDaySkills_4, robust = TRUE)

RSE_4 = coef(summary(lmdid_dayToDaySkills_4, robust = TRUE))[, "Robust s.e"]
RpValue_4 = coef(summary(lmdid_dayToDaySkills_4, robust = TRUE))[, "Pr(>|t|)"]










lmdid_test1 <- texreg(lmdid_dayToDaySkills_1, override.se = list(RSE_1), override.pvalues = list(RpValue_1),
                      caption = "linear regression",
                      custom.model.names = "Everyday Expertise",
                      omit.coef = 'id|year')

saveRDS(lmdid_test1, './ANALYSIS/Tables/lmdidtest.Rds')

print(readRDS('./ANALYSIS/Tables/lmdidtest.Rds'))




# screenreg(list(lmdid_dayToDaySkills_1, lmdid_dayToDaySkills_2),
#                      override.se = list(RSE_1, RSE_2), override.pvalues = list(RpValue_1, RpValue_2),
#      caption = "linear regression",
#       omit.coef = 'id|year')






