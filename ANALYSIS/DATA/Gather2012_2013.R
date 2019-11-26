#Datensätze 2012 und 2013 in R importieren und bearbeiten


# Packages Laden ----------------------------------------------------------

library(readxl)
library(tidyverse)


# Datensätze aus Excel in R ziehen ----------------------------------------


daten2012 <- read_excel("Studium/5. Semester/Pareto/Daten/wirkungsdaten.xlsx", sheet = 11) 

daten2013 <- read_excel("Studium/5. Semester/Pareto/Daten/wirkungsdaten.xlsx", sheet = 10)


# Datenüberblick ----------------------------------------------------------

#means anschauen

mean(daten2012$Kinder, na.rm = TRUE)
mean(daten2012$`Kochen 1x Monat`,na.rm = TRUE)

#Histogramme

hist(daten2013$`Kochen 1 Woche`)
hist(daten2012$`Kochen 1 Woche`)

#Plots rumprobieren

ggplot(data = daten2012, 
       aes(daten2012$`Wissen erweitert`, daten2012$`seltener krank`))

head(daten2012$Kinder)

plot(daten2013$`schätzen gesunde Ernährung`, daten2013$`Wissen erweitert`)

ggplot(data = daten2012, 
       aes(x= daten2012$`Selbstwertgefühl gestärkt`, y= daten2012$Mittagsmahlzeiten))

?ggplot

plot(daten2012$Mittagsmahlzeiten, daten2012$`Selbstwertgefühl gestärkt`)

plot(daten2012$Mittagsmahlzeiten, daten2012$Kinder)

plot(daten2012$`Wissen erweitert`, daten2012$`seltener krank`)

boxplot(daten2012$Kinder, daten2012$Mittagsmahlzeiten, outline = FALSE)

barplot(data=daten2012$`Wissen erweitert`, height = 1, horiz = TRUE, main = "Wissen erweitert")

plot(density(daten2012$`Wissen erweitert`))

model <- lm(daten2012$`Wissen erweitert` ~ daten2012$einkaufen, data = daten2012)
summary(model)

ggplot(data = daten2013, aes(x = daten2013$`Aktivitäten 2013`, y = daten2013$`Kinder 2013`)) +
  geom_col() +
  labs(title = "Aktivitäten und Kinder 2013",
       x = "Aktivitäten", y = "Kinder")

boxplot(daten2013$MT_Kinder, horizontal = TRUE,
        outline = FALSE)






