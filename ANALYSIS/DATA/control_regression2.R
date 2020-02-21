###Regression erstellen
library(estimatr)
library(car)

#Ziel: Einfluss vom Entdeckerfond auf selfworth messen

#1. Simple Regression

modelSimple <- lm_robust(dfcEF$selfworth ~ dfcEF$treatEF, data=dfcEF)

summary(modelSimple)


#2. Regression mit Zeit Fixed Effects

#2.1 dummy_2018 weglassen, um multikolliniarität zu verhindern

modelTime <- lm_robust(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2011 + dfcEF$dummy_2012 
                + dfcEF$dummy_2013 + dfcEF$dummy_2014
                + dfcEF$dummy_2015 + dfcEF$dummy_2016 + dfcEF$dummy_2017, data = dfcEF)

summary(modelTime)

#2.2 Beta0 weglassen, alle dummys einauen

modelTime2 <- lm_robust(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy_2011 + dfcEF$dummy_2012 
                       + dfcEF$dummy_2013 + dfcEF$dummy_2014
                       + dfcEF$dummy_2015 + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018 - 1, data = dfcEF)

summary(modelTime2)

#3. Regression mit Einrichtungs-Fixed Effects

#3.1 dummy103 weglassen um multikollinearität zu verhindern

modelID <- lm(dfcEF$selfworth ~ dfcEF$treatEF   + dfcEF$dummy104 + dfcEF$dummy105 + dfcEF$dummy106 + dfcEF$dummy108
                     + dfcEF$dummy109 + dfcEF$dummy111 + dfcEF$dummy112 + dfcEF$dummy113 + dfcEF$dummy114 + dfcEF$dummy118
                     + dfcEF$dummy122 + dfcEF$dummy123 + dfcEF$dummy124 + dfcEF$dummy125 + dfcEF$dummy130 + dfcEF$dummy131
                     + dfcEF$dummy132 + dfcEF$dummy133 + dfcEF$dummy136 + dfcEF$dummy137 + dfcEF$dummy139 + dfcEF$dummy141
                     + dfcEF$dummy142 + dfcEF$dummy165 + dfcEF$dummy186 + dfcEF$dummy187 + dfcEF$dummy188 + dfcEF$dummy189
                     + dfcEF$dummy190 + dfcEF$dummy191 + dfcEF$dummy192 + dfcEF$dummy193 + dfcEF$dummy194 + dfcEF$dummy209
                     + dfcEF$dummy213 + dfcEF$dummy214 + dfcEF$dummy215 + dfcEF$dummy216 + dfcEF$dummy217 + dfcEF$dummy218
                     + dfcEF$dummy219 + dfcEF$dummy220 + dfcEF$dummy221 + dfcEF$dummy226 + dfcEF$dummy233 + dfcEF$dummy249
                     + dfcEF$dummy255 + dfcEF$dummy269 + dfcEF$dummy270 + dfcEF$dummy281 + dfcEF$dummy282 + dfcEF$dummy403
                     + dfcEF$dummy404 + dfcEF$dummy417 + dfcEF$dummy418 + dfcEF$dummy437 + dfcEF$dummy482 + dfcEF$dummy483
                     + dfcEF$dummy599 + dfcEF$dummy600 + dfcEF$dummy601 + dfcEF$dummy602 + dfcEF$dummy623 + dfcEF$dummy663
                     + dfcEF$dummy664 + dfcEF$dummy665 + dfcEF$dummy666 + dfcEF$dummy667 + dfcEF$dummy684 + dfcEF$dummy685
                     + dfcEF$dummy686 + dfcEF$dummy687 )

summary(modelID)

#3.2 Beta0 weglassen und alle Dummys einbauen


modelID2 <- lm_robust(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy103 + dfcEF$dummy104 + dfcEF$dummy105 + dfcEF$dummy106 + dfcEF$dummy108
                     + dfcEF$dummy109 + dfcEF$dummy111 + dfcEF$dummy112 + dfcEF$dummy113 + dfcEF$dummy114 + dfcEF$dummy118
                     + dfcEF$dummy122 + dfcEF$dummy123 + dfcEF$dummy124 + dfcEF$dummy125 + dfcEF$dummy130 + dfcEF$dummy131
                     + dfcEF$dummy132 + dfcEF$dummy133 + dfcEF$dummy136 + dfcEF$dummy137 + dfcEF$dummy139 + dfcEF$dummy141
                     + dfcEF$dummy142 + dfcEF$dummy165 + dfcEF$dummy186 + dfcEF$dummy187 + dfcEF$dummy188 + dfcEF$dummy189
                     + dfcEF$dummy190 + dfcEF$dummy191 + dfcEF$dummy192 + dfcEF$dummy193 + dfcEF$dummy194 + dfcEF$dummy209
                     + dfcEF$dummy213 + dfcEF$dummy214 + dfcEF$dummy215 + dfcEF$dummy216 + dfcEF$dummy217 + dfcEF$dummy218
                     + dfcEF$dummy219 + dfcEF$dummy220 + dfcEF$dummy221 + dfcEF$dummy226 + dfcEF$dummy233 + dfcEF$dummy249
                     + dfcEF$dummy255 + dfcEF$dummy269 + dfcEF$dummy270 + dfcEF$dummy281 + dfcEF$dummy282 + dfcEF$dummy403
                     + dfcEF$dummy404 + dfcEF$dummy417 + dfcEF$dummy418 + dfcEF$dummy437 + dfcEF$dummy482 + dfcEF$dummy483
                     + dfcEF$dummy599 + dfcEF$dummy600 + dfcEF$dummy601 + dfcEF$dummy602 + dfcEF$dummy623 + dfcEF$dummy663
                     + dfcEF$dummy664 + dfcEF$dummy665 + dfcEF$dummy666 + dfcEF$dummy667 + dfcEF$dummy684 + dfcEF$dummy685
                     + dfcEF$dummy686 + dfcEF$dummy687 - 1 )
summary(modelID2)

#4. Regression mit ID und Time FE, ohne Beta0

modelfinal <- lm(dfcEF$selfworth ~ dfcEF$treatEF + dfcEF$dummy103 + dfcEF$dummy104 + dfcEF$dummy105 + dfcEF$dummy106 + dfcEF$dummy108
                      + dfcEF$dummy109 + dfcEF$dummy111 + dfcEF$dummy112 + dfcEF$dummy113 + dfcEF$dummy114 + dfcEF$dummy118
                      + dfcEF$dummy122 + dfcEF$dummy123 + dfcEF$dummy124 + dfcEF$dummy125 + dfcEF$dummy130 + dfcEF$dummy131
                      + dfcEF$dummy132 + dfcEF$dummy133 + dfcEF$dummy136 + dfcEF$dummy137 + dfcEF$dummy139 + dfcEF$dummy141
                      + dfcEF$dummy142 + dfcEF$dummy165 + dfcEF$dummy186 + dfcEF$dummy187 + dfcEF$dummy188 + dfcEF$dummy189
                      + dfcEF$dummy190 + dfcEF$dummy191 + dfcEF$dummy192 + dfcEF$dummy193 + dfcEF$dummy194 + dfcEF$dummy209
                      + dfcEF$dummy213 + dfcEF$dummy214 + dfcEF$dummy215 + dfcEF$dummy216 + dfcEF$dummy217 + dfcEF$dummy218
                      + dfcEF$dummy219 + dfcEF$dummy220 + dfcEF$dummy221 + dfcEF$dummy226 + dfcEF$dummy233 + dfcEF$dummy249
                      + dfcEF$dummy255 + dfcEF$dummy269 + dfcEF$dummy270 + dfcEF$dummy281 + dfcEF$dummy282 + dfcEF$dummy403
                      + dfcEF$dummy404 + dfcEF$dummy417 + dfcEF$dummy418 + dfcEF$dummy437 + dfcEF$dummy482 + dfcEF$dummy483
                      + dfcEF$dummy599 + dfcEF$dummy600 + dfcEF$dummy601 + dfcEF$dummy602 + dfcEF$dummy623 + dfcEF$dummy663
                      + dfcEF$dummy664 + dfcEF$dummy665 + dfcEF$dummy666 + dfcEF$dummy667 + dfcEF$dummy684 + dfcEF$dummy685
                      + dfcEF$dummy686 + dfcEF$dummy687 
                      + dfcEF$dummy_2011 + dfcEF$dummy_2012 
                      + dfcEF$dummy_2013 + dfcEF$dummy_2014
                      + dfcEF$dummy_2015 + dfcEF$dummy_2016 + dfcEF$dummy_2017 + dfcEF$dummy_2018 - 1, data = dfcEF )
summary(modelfinal)

