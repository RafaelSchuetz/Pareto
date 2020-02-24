### DiD-Schätzungen

# Erstellung von Interaktionstermen, in denen die Treatment-Variable mit den Jahres-Dummies interagiert werden
### DID Term (Interaktionsterm erstellen) ####

library(dplyr)
library(stargazer)

#dfcEF <- dfcEF %>% 
#  mutate(
#    treat_2011 = treatEF*dummy_2011,
#    treat_2012 = treatEF*dummy_2012,
#    treat_2013 = treatEF*dummy_2012,
#    treat_2014 = treatEF*dummy_2012,
#    treat_2015 = treatEF*dummy_2012,
#    treat_2016 = treatEF*dummy_2012,
#    treat_2017 = treatEF*dummy_2012,
#    treat_2012 = treatEF*dummy_2012
#  )

dfcEF <- dfcEF %>% 
  mutate(
    treat_2011 = ifelse(treatEF == 1 & dummy_2011 == 1, 1, 0),
    treat_2012 = ifelse(treatEF == 1 & dummy_2012 == 1, 1, 0),
    treat_2013 = ifelse(treatEF == 1 & dummy_2013 == 1, 1, 0),
    treat_2014 = ifelse(treatEF == 1 & dummy_2014 == 1, 1, 0),
    treat_2015 = ifelse(treatEF == 1 & dummy_2015 == 1, 1, 0),
    treat_2016 = ifelse(treatEF == 1 & dummy_2015 == 1, 1, 0),
    treat_2017 = ifelse(treatEF == 1 & dummy_2015 == 1, 1, 0),
    treat_2018 = ifelse(treatEF == 1 & dummy_2015 == 1, 1, 0)
  )

#id als factor definieren
dfcEF$id <- as.factor(dfcEF$id)
dfcEF$year <- as.factor(dfcEF$year)
dfcEF$treatEF <- as.factor(dfcEF$treatEF)

### DID Regression ####

#treatEF ist der treatment Effekt
#year ist der Year fixed effect (Zeitfixedeffect)
#id ist der ID fixed effect
#treat_2011 ist beispielsweise der Interaktionsterm zwischen 2011 und der Treatment Variable

lmdid <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$year + (dfcEF$year*dfcEF$treatEF))

lm_did_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$year + dfcEF$treat_2011 + dfcEF$treat_2012 +
                         dfcEF$treat_2013 + dfcEF$treat_2014 + dfcEF$treat_2015 + dfcEF$treat_2016 + 
                         dfcEF$treat_2017 + dfcEF$treat_2018)
summary(lm_did_selfworth)

summary(lmdid)

alias(lm_did_selfworth)


### Regression for DID Estimation ####
library(sandwich)
library(estimatr)
library(robustbase)

#lineares Modell mit robusten Standardfehlern und fixed effects (time und ID)
lmdid2 <- lm_robust(dfcEF$dayToDaySkills ~ dfcEF$treatEF,
                    fixed_effects = ~ dfcEF$id + dfcEF$year, se_type = "HC1")

lmdid3 <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id + dfcEF$year )
  
summary(lmdid2)
summary(lmdid3)

library(texreg)

texreg(lmdid2)

#treat EF misst den Treatment Effekt in den jeweiligen Zeitperioden
#Also 0011111 oder 011111 oder 00000011
#id ist der ID fixed effect
#year misst den year fixed effect
#beides als fixed effect, weil die Variable als factor definiert ist


### Regression Results Table ####

table_did_selfworth <- stargazer(lmdid3,
                                 omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
                                         'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
                                         'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
                                         'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
                                         'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
                                         'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
                                         'id687', 'year2012', 'year2013', 'year2014', 'year2015', 'year2016', 'year2017',
                                         'year2018'),
                                 add.lines = list(c('ID fixed effects', 'Yes'),
                                                  c('Year fixed effects', 'Yes')),
                                 type = 'text')













