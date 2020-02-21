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