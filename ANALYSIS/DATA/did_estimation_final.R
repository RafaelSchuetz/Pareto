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


#### Vorarbeiten ####

# Plausibilisierung für den Treatment-Dummy
dfcEF_check <- dfcEF

dfcEF_check <- dfcEF_check %>% 
  dplyr :: select(id, treatEF)

# Ändern des Variablen-Name von 'subsidy' zu 'funding', um den Regression-Table zu erstellen
dfcEF <- dfcEF %>% 
  dplyr::rename(funding = 'subsidy')

# Teilen der in Euro angegebenen Kontroll-Variablen 'subsidy' und 'totalCost' durch 1000, damit die 
# Regressionskoeffizienten leichter interpretiert werden können
dfcEF$funding <- (dfcEF$funding/1000)
dfcEF$totalCost <- (dfcEF$totalCost/1000)

# Änderung des Datentyps
dfcEF$id <- as.factor(dfcEF$id)
dfcEF$year <- as.factor(dfcEF$year)
dfcEF$treatEF <- as.numeric(dfcEF$treatEF)


#### Treatment Variable: Variante 2 #### ____________________________________________________________

#### Everyday expertise ####

# DiD-Estimation mit id fixed effects and year fixed effects
lmdid_dayToDaySkills_3 <-  felm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year)

summary(lmdid_dayToDaySkills_3)
summary(lmdid_dayToDaySkills_3, robust = TRUE)

RSE_3 = coef(summary(lmdid_dayToDaySkills_3, robust = TRUE))[, "Robust s.e"]
RpValue_3 = coef(summary(lmdid_dayToDaySkills_3, robust = TRUE))[, "Pr(>|t|)"]


# Mit organization-specific Kontrollvariablen
lmdid_dayToDaySkills_4 <-  felm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id+ dfcEF$year
                                + dfcEF$funding + dfcEF$totalCost + dfcEF$weeklyCooks_scaled)

summary(lmdid_dayToDaySkills_4)
summary(lmdid_dayToDaySkills_4, robust = TRUE)

RSE_4 = coef(summary(lmdid_dayToDaySkills_4, robust = TRUE))[, "Robust s.e"]
RpValue_4 = coef(summary(lmdid_dayToDaySkills_4, robust = TRUE))[, "Pr(>|t|)"]


#### Selfworth #### ___________________________________________________________________________________

# DiD-Estimation mit id fixed effects and year fixed effects
lmdid_selfworth_3 <-  felm(dfcEF$selfworth_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year)

summary(lmdid_selfworth_3)
summary(lmdid_selfworth_3, robust = TRUE)

RSE_selfworth_3 = coef(summary(lmdid_selfworth_3, robust = TRUE))[, "Robust s.e"]
RpValue_selfworth_3 = coef(summary(lmdid_selfworth_3, robust = TRUE))[, "Pr(>|t|)"]

# Mit organization-specific Kontrollvariablen
lmdid_selfworth_4 <-  felm(dfcEF$selfworth_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year
                           + dfcEF$funding + dfcEF$totalCost + dfcEF$weeklyCooks_scaled)

summary(lmdid_selfworth_4)
summary(lmdid_selfworth_4, robust = TRUE)

RSE_selfworth_4 = coef(summary(lmdid_selfworth_4, robust = TRUE))[, "Robust s.e"]
RpValue_selfworth_4 = coef(summary(lmdid_selfworth_4, robust = TRUE))[, "Pr(>|t|)"]




#### Treatment-Variable: Variante 1 #### ______________________________________________________________

# Sobald eine Einrichtung i das Treatment erhält, ist der Treatment-Dummy für die folgenden Perioden 
# immer gleich 1 (= Einmal Treatment, immer Treatment)

# Identifizieren aller Einrichtungen, bei denen der Treatment-Status von TreatEF = 1 auf TreatEF = 0 wechselt

dfcEF_check <- dfcEF

dfcEF_check <- dfcEF_check %>% 
  dplyr :: select(id, treatEF)

# Identifizierte Einrichtungen mit folgenden ID's: 113, 141, 191, 221, 226 und 282

# Sobald eine Einrichtung das Treatment erhält, gilt TreatEF = 1 für alle Folgejahre
# Für die identifizierten Einrichtungen wird der Treatment-Status von TreatEF = 0 auf TreatEF = 1 für alle
# Jahre ab dem ersten Treatment geändert.

dfcEF [67, "treatEF"] <- 1
dfcEF [168, "treatEF"] <- 1
dfcEF [169, "treatEF"] <- 1
dfcEF [225, "treatEF"] <- 1
dfcEF [226, "treatEF"] <- 1
dfcEF [227, "treatEF"] <- 1
dfcEF [228, "treatEF"] <- 1
dfcEF [229, "treatEF"] <- 1
dfcEF [230, "treatEF"] <- 1
dfcEF [330, "treatEF"] <- 1
dfcEF [331, "treatEF"] <- 1
dfcEF [335, "treatEF"] <- 1
dfcEF [389, "treatEF"] <- 1
dfcEF [390, "treatEF"] <- 1
dfcEF [391, "treatEF"] <- 1


#### Everyday expertise ####

lmdid_dayToDaySkills_1 <-  felm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year)

summary(lmdid_dayToDaySkills_1)
summary(lmdid_dayToDaySkills_1, robust = TRUE)

RSE_1 = coef(summary(lmdid_dayToDaySkills_1, robust = TRUE))[, "Robust s.e"]
RpValue_1 = coef(summary(lmdid_dayToDaySkills_1, robust = TRUE))[, "Pr(>|t|)"]


# Mit organization-specific Kontrollvariablen

lmdid_dayToDaySkills_2 <-  felm(dfcEF$dayToDaySkills_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year
                                + dfcEF$funding + dfcEF$totalCost + dfcEF$weeklyCooks_scaled)

summary(lmdid_dayToDaySkills_2)
summary(lmdid_dayToDaySkills_2, robust = TRUE)

RSE_2 = coef(summary(lmdid_dayToDaySkills_2, robust = TRUE))[, "Robust s.e"]
RpValue_2 = coef(summary(lmdid_dayToDaySkills_2, robust = TRUE))[, "Pr(>|t|)"]


#### Selfworth #### ___________________________________________________________________________________

# DiD-Estimation mit id fixed effects and year fixed effects

lmdid_selfworth_1 <-  felm(dfcEF$selfworth_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year)

summary(lmdid_selfworth_1)
summary(lmdid_selfworth_1, robust = TRUE)

RSE_selfworth_1 = coef(summary(lmdid_selfworth_1, robust = TRUE))[, "Robust s.e"]
RpValue_selfworth_1 = coef(summary(lmdid_selfworth_1, robust = TRUE))[, "Pr(>|t|)"]


# Mit organization-specific Kontrollvariablen
lmdid_selfworth_2 <-  felm(dfcEF$selfworth_scaled ~ dfcEF$treatEF + dfcEF$id + dfcEF$year
                           + dfcEF$funding + dfcEF$totalCost + dfcEF$weeklyCooks_scaled)

summary(lmdid_selfworth_2)
summary(lmdid_selfworth_2, robust = TRUE)

RSE_selfworth_2 = coef(summary(lmdid_selfworth_2, robust = TRUE))[, "Robust s.e"]
RpValue_selfworth_2 = coef(summary(lmdid_selfworth_2, robust = TRUE))[, "Pr(>|t|)"]










# screenreg(list(lmdid_dayToDaySkills_1, lmdid_dayToDaySkills_2),
#                      override.se = list(RSE_1, RSE_2), override.pvalues = list(RpValue_1, RpValue_2),
#      caption = "linear regression",
#       omit.coef = 'id|year')



### erstellen der Regressions Tabellen ####

### DaytoDayskills ####

screenreg(
  list(lmdid_dayToDaySkills_1, lmdid_dayToDaySkills_2, lmdid_dayToDaySkills_3, lmdid_dayToDaySkills_4),
  override.se = list(RSE_1, RSE_2, RSE_3, RSE_4),
  override.pvalues = list(RpValue_1, RpValue_2, RpValue_3, RpValue_4),
  #groups = list('first' = 3:67, 'second' = 68:75),
  custom.model.names = c('(1)', '(2)', '(3)', '(4)'),
  custom.coef.names = c('treatEF','subsidy', 'totalCost', 'weeklyCooks'),
  stars = c(0.01, 0.05, 0.1),
  omit.coef = 'id|year|Intercept',
  digits = 3,
  #dann noch die Observations manuell hinzufügen, dann kann ich den Namen zu Number of observations ändern
  custom.gof.rows = list('ID fixed effects' = c('yes', 'yes', 'yes', 'yes'), 'Year fixed effects' = c('yes', 'yes', 'yes', 'yes'),
                         'Number of observations' = c('428', '410', '428', '410'),
                         'R$^2$' = c('0.475', '0.490', '0.476', '0.491')),
  include.adjrs = FALSE,
  #entferne das, damit ich selber manuell erstellen kann (schönere Formulierung dann) 
  include.nobs = FALSE,
  reorder.gof = c(3,1,2,4,5, 6)
)


### selfworth ####

screenreg(
  list(lmdid_selfworth_1, lmdid_selfworth_2, lmdid_selfworth_3, lmdid_selfworth_4),
  override.se = list(RSE_selfworth_1, RSE_selfworth_2, RSE_selfworth_3, RSE_selfworth_4),
  override.pvalues = list(RpValue_selfworth_1, RpValue_selfworth_2, RpValue_selfworth_3, RpValue_selfworth_4),
  #groups = list('first' = 3:67, 'second' = 68:75),
  custom.model.names = c('(1)', '(2)', '(3)', '(4)'),
  #custom.coef.names = c('Intercept', 'treatEF', 'totalCost', 'weeklyCooks'),
  stars = c(0.01, 0.05, 0.1),
  omit.coef = 'id|year',
  digits = 3,
  #dann noch die Observations manuell hinzufügen, dann kann ich den Namen zu observations ändern
  custom.gof.rows = list('ID FE' = c('yes', 'yes', 'yes', 'yes'), 'Year FE' = c('yes', 'yes', 'yes', 'yes')),
  #reorder.gof = c(3, 1, 2, 4, 5, 6, 7),
  include.adjrs = FALSE
)
