#Regressionstable erstellen

library(stargazer)
#Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables.
#R package version 5.2.2. https://CRAN.R-project.org/package=stargazer 

#Wir fügen immer die Kontrollvariablen hinzu, die wir in dem RScript control_variables rausgefunden haben

### 1. Regression: Simple linear regression ohne und mit controls ####

#selfworth dependent variable
lm1_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF)

lm1_selfworth_controls <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dayToDaySkills + dfcEF$tasksLunch 
                    +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                    + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                    + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                    +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                    +dfcEF$enoughFood)

lm1_selfworth_controls_nobad <- lm(dfcEF$selfworth ~ dfcEF$treatEF+ dfcEF$tasksLunch 
                             +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                             + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                             + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                             +dfcEF$enoughFood)

#daytoday skills as dependent variable
lm1_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF)

lm1_skills_controls <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$selfworth + dfcEF$DGECriteriaNo 
                 +dfcEF$subsidy + dfcEF$tasksLunch + dfcEF$monthlyCooks + dfcEF$weeklyCooks
                 +dfcEF$shoppers + dfcEF$easyDishes + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy
                 +dfcEF$foodCulture + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                 +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                 +dfcEF$enoughFood)


#wenn wir moreindependent und moreopen weglassen, dann ist der Effekt signifikant auf dem 1% niveau
#bad control??


### 2. Regression: Linear regression with year fixed effects ####

#selfworth as dependent variable
lm2_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                    + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                    + dfcEF$dummy_2017 + dfcEF$dummy_2018 )

#damit das funktioniert, müssen alle Dummies als numeric kategorisiert sein
#wenn factor/character dann eine Fehlermeldung
lm2_selfworth_controls <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                    + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                    + dfcEF$dummy_2017 + dfcEF$dummy_2018 + dfcEF$dayToDaySkills + dfcEF$tasksLunch 
                    +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                    + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                    + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                    +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                    +dfcEF$enoughFood)

lm2_selfworth_controls_nobad <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                             + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                             + dfcEF$dummy_2017 + dfcEF$dummy_2018 + dfcEF$tasksLunch 
                             +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                             + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                             + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                             +dfcEF$enoughFood)

#daytodayskills as dependent variable 
lm2_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                 + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                 + dfcEF$dummy_2017 + dfcEF$dummy_2018 )

lm2_skills_controls <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$dummy_2012 + dfcEF$dummy_2013
                 + dfcEF$dummy_2014 + dfcEF$dummy_2015 + dfcEF$dummy_2016 
                 + dfcEF$dummy_2017 + dfcEF$dummy_2018 + dfcEF$selfworth + dfcEF$DGECriteriaNo 
                 +dfcEF$subsidy + dfcEF$tasksLunch + dfcEF$monthlyCooks + dfcEF$weeklyCooks
                 +dfcEF$shoppers + dfcEF$easyDishes + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy
                 +dfcEF$foodCulture + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                 +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                 +dfcEF$enoughFood)


### 3. Regression: Linear regression with id fixed effects ####

# Selfworth as dependent variable 
lm3_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id)
                    
lm3_selfworth_controls <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$dayToDaySkills + dfcEF$tasksLunch 
                             +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                             + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                             + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                             +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                             +dfcEF$enoughFood)

lm3_selfworth_controls_nobad <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$tasksLunch 
                             +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                             + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                             + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                             +dfcEF$enoughFood)

# Day-To-Day-Skills as dependent variable
lm3_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id)
                 
lm3_skills_controls <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id + dfcEF$selfworth + dfcEF$DGECriteriaNo 
                          +dfcEF$subsidy + dfcEF$tasksLunch + dfcEF$monthlyCooks + dfcEF$weeklyCooks
                          +dfcEF$shoppers + dfcEF$easyDishes + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy
                          +dfcEF$foodCulture + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                          +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                          +dfcEF$enoughFood)


### 4. Regression: Linear Regression with id FE and year FE ####

# Selfworth as dependent variable
lm4_selfworth <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012 
                    + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                    + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018)

lm4_selfworth_controls <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012 
                    + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                    + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018
                    + dfcEF$dayToDaySkills + dfcEF$tasksLunch 
                    +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                    + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                    + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                    +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                    +dfcEF$enoughFood)

lm4_selfworth_controls_nobad <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012 
                             + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                             + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018
                             + dfcEF$tasksLunch 
                             +dfcEF$monthlyCooks + dfcEF$weeklyCooks + dfcEF$shoppers + dfcEF$easyDishes
                             + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy + dfcEF$foodCulture
                             + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                             +dfcEF$enoughFood)

# Day-To-Day-Skills as dependent variable
lm4_skills <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012
                 + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                 + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018)
                 
lm4_skills_controls <- lm(dfcEF$dayToDaySkills ~ dfcEF$treatEF + dfcEF$id + dfcEF$dummy_2012
                 + dfcEF$dummy_2013 + dfcEF$dummy_2014 + dfcEF$dummy_2015 
                 + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018
                 + dfcEF$selfworth + dfcEF$DGECriteriaNo 
                 +dfcEF$subsidy + dfcEF$tasksLunch + dfcEF$monthlyCooks + dfcEF$weeklyCooks
                 +dfcEF$shoppers + dfcEF$easyDishes + dfcEF$dietaryKnowledge + dfcEF$appreciateHealthy
                 +dfcEF$foodCulture + dfcEF$moreConcentrated + dfcEF$moreBalanced + dfcEF$moreIndependent
                 +dfcEF$moreOpen + dfcEF$moreConfidence + dfcEF$addressProblems + dfcEF$proud
                 +dfcEF$enoughFood)

### 5. Alle LMs als RDS speichern ####

saveRDS(lm1_selfworth, file = "./ANALYSIS/Tables/lm1_selfworth.Rds")
saveRDS(lm1_selfworth_controls, file = "./ANALYSIS/Tables/lm1_selfworth_controls.Rds")
saveRDS(lm1_selfworth_controls_nobad, file = "./ANALYSIS/Tables/lm1_selfworth_controls_nobad.Rds")
saveRDS(lm1_skills, file = "./ANALYSIS/Tables/lm1_skills.Rds")
saveRDS(lm1_skills_controls, file = "./ANALYSIS/Tables/lm1_skills_controls.Rds")

saveRDS(lm2_selfworth, file = "./ANALYSIS/Tables/lm2_selfworth.Rds")
saveRDS(lm2_selfworth_controls, file = "./ANALYSIS/Tables/lm2_selfworth_controls.Rds")
saveRDS(lm2_selfworth_controls_nobad, file = "./ANALYSIS/Tables/lm2_selfworth_controls_nobad.Rds")
saveRDS(lm2_skills, file = "./ANALYSIS/Tables/lm2_skills.Rds")
saveRDS(lm2_skills_controls, file = "./ANALYSIS/Tables/lm2_skills_controls.Rds")

saveRDS(lm3_selfworth, file = "./ANALYSIS/Tables/lm3_selfworth.Rds")
saveRDS(lm3_selfworth_controls, file = "./ANALYSIS/Tables/lm3_selfworth_controls.Rds")
saveRDS(lm3_selfworth_controls_nobad, file = "./ANALYSIS/Tables/lm3_selfworth_controls_nobad.Rds")
saveRDS(lm3_skills, file = "./ANALYSIS/Tables/lm3_skills.Rds")
saveRDS(lm3_skills_controls, file = "./ANALYSIS/Tables/lm3_skills_controls.Rds")

saveRDS(lm4_selfworth, file = "./ANALYSIS/Tables/lm4_selfworth.Rds")
saveRDS(lm4_selfworth_controls, file = "./ANALYSIS/Tables/lm4_selfworth_controls.Rds")
saveRDS(lm4_selfworth_controls_nobad, file = "./ANALYSIS/Tables/lm4_selfworth_controls_nobad.Rds")
saveRDS(lm4_skills, file = "./ANALYSIS/Tables/lm4_skills.Rds")
saveRDS(lm4_skills_controls, file = "./ANALYSIS/Tables/lm4_skills_controls.Rds")

# ### 6. Output Tables erstellen ####
# 
# ### selfworth als dependent variable ####
# table1_selfworth <- stargazer(lm1_selfworth,
#                     lm2_selfworth,
#                     lm3_selfworth,
#                     lm4_selfworth,
#           title = "Regression Results",
#           order = c('Constant', 'treatEF'),
#           omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
#                    'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
#                    'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
#                    'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
#                    'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
#                    'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
#                    'id687','dummy_2012','dummy_2013','dummy_2014','dummy_2015','dummy_2016','dummy_2017',
#                    'dummy_2018', 'id'
#                   ),
#           add.lines = list(c("ID fixed effects", 'No', 'No', 'Yes', 'Yes'),
#                            c('Time fixed effects', 'No', 'Yes', 'No', 'Yes')),
#           align = TRUE)
# 
# 
# 
# 
# ### selfworth als dependent variable mit controls ####
# 
# table1_selfworth_controls <- stargazer(lm1_selfworth_controls,
#                               lm2_selfworth_controls,
#                               lm3_selfworth_controls,
#                               lm4_selfworth_controls,
#                               title = "Regression Results",
#                               order = c('Constant', 'treatEF'),
#                               omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
#                                        'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
#                                        'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
#                                        'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
#                                        'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
#                                        'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
#                                        'id687','dummy_2012','dummy_2013','dummy_2014','dummy_2015','dummy_2016','dummy_2017',
#                                        'dummy_2018', 'id'
#                               ),
#                               add.lines = list(c("ID fixed effects", 'No', 'No', 'Yes', 'Yes'),
#                                                c('Time fixed effects', 'No', 'Yes', 'No', 'Yes')),
#                               align = TRUE,
#                               type = 'text')
# 
# ### selfworth als dependent variable mit controls aber ohne bad controls ####
# 
# table1_selfworth_controls_nobad <- stargazer(lm1_selfworth_controls_nobad,
#                                        lm2_selfworth_controls_nobad,
#                                        lm3_selfworth_controls_nobad,
#                                        lm4_selfworth_controls_nobad,
#                                        title = "Regression Results",
#                                        order = c('Constant', 'treatEF'),
#                                        omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
#                                                 'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
#                                                 'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
#                                                 'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
#                                                 'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
#                                                 'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
#                                                 'id687','dummy_2012','dummy_2013','dummy_2014','dummy_2015','dummy_2016','dummy_2017',
#                                                 'dummy_2018', 'id'
#                                        ),
#                                        add.lines = list(c("ID fixed effects", 'No', 'No', 'Yes', 'Yes'),
#                                                         c('Time fixed effects', 'No', 'Yes', 'No', 'Yes')),
#                                        align = TRUE,
#                                        type = 'text')
# 
# ### daytoday skills als dependent variable ####
# 
# table1_skills <- stargazer(lm1_skills,
#                            lm2_skills,
#                            lm3_skills,
#                            lm4_skills,
#                            title = "Regression Results",
#                            order = c('Constant', 'treatEF'),
#                            omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
#                                     'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
#                                     'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
#                                     'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
#                                     'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
#                                     'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
#                                     'id687','dummy_2012','dummy_2013','dummy_2014','dummy_2015','dummy_2016','dummy_2017',
#                                     'dummy_2018', 'id'
#                            ),
#                            add.lines = list(c("ID fixed effects", 'No', 'No', 'Yes', 'Yes'),
#                                             c('Time fixed effects', 'No', 'Yes', 'No', 'Yes')),
#                            align = TRUE,
#                            type = 'text')
# 
# ### day to day skills als dependent variable mit controls ####
# 
# table1_skills_controls <- stargazer(lm1_skills_controls,
#                            lm2_skills_controls,
#                            lm3_skills_controls,
#                            lm4_skills_controls,
#                            title = "Regression Results",
#                            order = c('Constant', 'treatEF'),
#                            omit = c('id104','id105','id106','id108','id109','id111','id112','id113','id114','id118','id122',
#                                     'id123','id124','id125','id130','id131','id132','id133','id136','id137','id139','id141',
#                                     'id142','id165','id186','id187','id188','id189','id190','id191','id192','id193','id194',
#                                     'id209','id213','id214','id215','id216','id217','id218','id219','id220','id221','id226',
#                                     'id233','id249','id255','id269','id270','id281','id282','id403','id404','id417','id418',
#                                     'id437','id482','id483','id599','id600','id601','id602','id623','id684','id685','id686',
#                                     'id687','dummy_2012','dummy_2013','dummy_2014','dummy_2015','dummy_2016','dummy_2017',
#                                     'dummy_2018'
#                            ),
#                            add.lines = list(c("ID fixed effects", 'No', 'No', 'Yes', 'Yes'),
#                                             c('Time fixed effects', 'No', 'Yes', 'No', 'Yes')),
#                            align = TRUE,
#                            type = 'latex')
# 
# 