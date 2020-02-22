#Regressionstable erstellen

library(stargazer)
#Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
#R package version 5.2.2. https://CRAN.R-project.org/package=stargazer 

### 1. Regression: Simple linear regression

#selfworth dependent variable
lm1_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF)

#daytoday skills as dependent variable
lm1_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF)


### 2. Regression: Linear regression with year fixed effects

#selfworth as dependent variable
lm2_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                    + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                    + dfcEF$dummy_2017 + dfcEF$dummy_2018)

#daytodayskills as dependent variable 
lm2_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                 + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                 + dfcEF$dummy_2017 + dfcEF$dummy_2018)

### 3. Regression: Linear regression with id fixed effects

# Selfworth as dependent variable 
lm3_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id)

# Day-To-Day-Skills as dependent variable
lm3_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id)


### 4. Regression: Linear Regression with id FE and year FE

# Selfworth as dependent variable
lm4_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012 
                    + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                    + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018)

# Day-To-Day-Skills as dependent variable
lm4_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012
                 + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                 + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018)


### 5. Alle LMs als RDS speichern

saveRDS(lm1_selfworth, file = "./ANALYSIs/Tables/lm1_selfworth.rds")
saveRDS(lm1_skills, file = "./ANALYSIs/Tables/lm1_skills.rds")

saveRDS(lm2_selfworth, file = "./ANALYSIs/Tables/lm2_selfworth.rds")
saveRDS(lm2_skills, file = "./ANALYSIs/Tables/lm2_skills.rds")

saveRDS(lm3_selfworth, file = "./ANALYSIs/Tables/lm3_selfworth.rds")
saveRDS(lm3_skills, file = "./ANALYSIs/Tables/lm3_skills.rds")

saveRDS(lm4_selfworth, file = "./ANALYSIs/Tables/lm4_selfworth.rds")
saveRDS(lm4_skills, file = "./ANALYSIs/Tables/lm4_skills.rds")


### 6. Output Tables erstellen

#selfworth als dependent variable 
table1_selfworth <- stargazer(lm1_selfworth,
                    lm2_selfworth,
                    lm3_selfworth,
                    lm4_selfworth,
          title = "Regression Results",
          order = c('Constant', 'treatEF'),
          omit = c('id104',
                   'id105',
                   'id106',
                   'id108',
                   'id109',
                   'id111',
                   'id112',
                   'id113',
                   'id114',
                   'id118',
                   'id122',
                   'id123',
                   'id124',
                   'id125',
                   'id130',
                   'id131',
                   'id132',
                   'id133',
                   'id136',
                   'id137',
                   'id139',
                   'id141',
                   'id142',
                   'id165',
                   'id186',
                   'id187',
                   'id188',
                   'id189',
                   'id190',
                   'id191',
                   'id192',
                   'id193',
                   'id194',
                   'id209',
                   'id213',
                   'id214',
                   'id215',
                   'id216',
                   'id217',
                   'id218',
                   'id219',
                   'id220',
                   'id221',
                   'id226',
                   'id233',
                   'id249',
                   'id255',
                   'id269',
                   'id270',
                   'id281',
                   'id282',
                   'id403',
                   'id404',
                   'id417',
                   'id418',
                   'id437',
                   'id482',
                   'id483',
                   'id599',
                   'id600',
                   'id601',
                   'id602',
                   'id623',
                   'id684',
                   'id685',
                   'id686',
                   'id687',
                   'dummy_20121',
                   'dummy_20131',
                   'dummy_20141',
                   'dummy_20151',
                   'dummy_20161',
                   'dummy_20171',
                   'dummy_20181'
                  ),
          add.lines = list(c("ID fixed effects", 'No', 'No', 'Yes', 'Yes'),
                           c('Time fixed effects', 'No', 'Yes', 'No', 'Yes')),
          align = TRUE,
          type = 'text')


#daytoday skills als dependent variable
table1_skills <- stargazer(lm1_skills,
                           lm2_skills,
                           lm3_skills,
                           lm4_skills,
                           title = "Regression Results",
                           order = c('Constant', 'treatEF'),
                           omit = c('id104',
                                    'id105',
                                    'id106',
                                    'id108',
                                    'id109',
                                    'id111',
                                    'id112',
                                    'id113',
                                    'id114',
                                    'id118',
                                    'id122',
                                    'id123',
                                    'id124',
                                    'id125',
                                    'id130',
                                    'id131',
                                    'id132',
                                    'id133',
                                    'id136',
                                    'id137',
                                    'id139',
                                    'id141',
                                    'id142',
                                    'id165',
                                    'id186',
                                    'id187',
                                    'id188',
                                    'id189',
                                    'id190',
                                    'id191',
                                    'id192',
                                    'id193',
                                    'id194',
                                    'id209',
                                    'id213',
                                    'id214',
                                    'id215',
                                    'id216',
                                    'id217',
                                    'id218',
                                    'id219',
                                    'id220',
                                    'id221',
                                    'id226',
                                    'id233',
                                    'id249',
                                    'id255',
                                    'id269',
                                    'id270',
                                    'id281',
                                    'id282',
                                    'id403',
                                    'id404',
                                    'id417',
                                    'id418',
                                    'id437',
                                    'id482',
                                    'id483',
                                    'id599',
                                    'id600',
                                    'id601',
                                    'id602',
                                    'id623',
                                    'id684',
                                    'id685',
                                    'id686',
                                    'id687',
                                    'dummy_20121',
                                    'dummy_20131',
                                    'dummy_20141',
                                    'dummy_20151',
                                    'dummy_20161',
                                    'dummy_20171',
                                    'dummy_20181'
                           ),
                           add.lines = list(c("ID fixed effects", 'No', 'No', 'Yes', 'Yes'),
                                            c('Time fixed effects', 'No', 'Yes', 'No', 'Yes')),
                           align = TRUE,
                           type = 'text')



