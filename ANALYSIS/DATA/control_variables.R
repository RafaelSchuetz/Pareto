### Kontrollvariablen für die DiD-Estimations

# Die id-fixed effects kontrollieren bereits für alle zeit-invarianten Eigenschaften, die 
# zwischen den Einrichtungen variieren (z.B. Bundesland, Große / Kleine Stadt). Die time-fixed
# effects kontrollieren für Eigenschaften, die über die Zeit variieren, aber für die Einrichtungen
# konstant sind (z.B. allgemeiner Zeittrend). Aus diesem Grund werden nur Kontrollvariablen 
# ermittelt, die über die Zeit variieren.

### Ermitteln relevanter zeit-varianter Kontrollvariablen

# Methode: Korrelation zwischen den relevanten Variablen und möglichen Kontrollvariablen aus
# den Mittagstisch Outcomes

# Erstellen eines Teildatensatzes, der die Zielvariablen (selfworth & dayToDaySkills), 
# Treatment-Dummy und alle Variablen für den Mittagstisch umfasst
corMT_controls <- subset(dfcEF, select = c('selfworth',
                                           'dayToDaySkills',
                                           'treatEF',
                                           "eatersPerMealNo", "newKidsNo", "cateringNo",
                                           "mealsInInstitutionNo", "mealsNo",  "breakfastsNo",
                                           "lunchesNo", "snacksNo", "dinnersNo", "offersPerWeekNo",
                                           "weeksOfferedNo", "daysOfferedNo", "DGECriteriaNo", "totalCost",
                                           "subsidyRequest", "subsidy", "migrantBackgroundShare", "refugeesShare",
                                           "unemploymentShare", "multipleChildrenHouseholdShare", "singleParentShare",
                                           "povertyShare", "participateMore", "tasksLunch", "monthlyCooks", "weeklyCooks",
                                           "shoppers", "ownIdeas", "stayLonger", "easyDishes", "dietaryKnowledge",
                                           "appreciateHealthy", "foodCulture", "influenceHome", "cookAtHome", "askRecipes",
                                           "moreConcentrated", "moreBalanced", "lessIll", "moreIndependent",
                                           "betterTeamwork", "betterReading", "betterNumbers", "betterGrades",
                                           "moreRegularSchoolVisits", "moreOpen", "moreConfidence",
                                           "addressProblems", "proud", "success", "abiturNo", "mittlereReifeNo", "hauptschuleNo",
                                           "noDegreeNo", "trainingStartedNo", "trainingCompletedNo", "enoughFood",
                                           "enoughStaffLunch", "enoughStaffActivities", "qualitySatisfies", "regionalProducts",
                                           "cultureReligion", "unsweetenedDrinks"))

# Erstellen einer Korrelationsmatrix anhand des Befehls rcorr() aus dem Package 'Hmisc'

library(Hmisc)

correlation_matrix_controls <- rcorr(as.matrix(corMT_controls))

# Speichern der p-Werte aus der Korrelationsmatrix in p
# Speicher der Korrelationskoeffizienten aus der Korrelationsmatrix in R
p <- correlation_matrix_controls$P
R <- correlation_matrix_controls$r

# Definition der Notation für das Signifikanzniveau in Sterne
# Wichtig ist, dass die Lehrzeichen bei den Sternen eingehalten werden
stars_significance <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", 
                      ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))

# Kürzen der Korrelationsmatrix auf zwei Kommazahlen
R <- format(round(cbind(rep(-1.11, ncol(corMT_controls)), R), 2))[,-1]

# Generierung einer neuen Matrix, welche die Korellationskoeffizienten mit den dazugehörigen 
# Sterne für das Signifikanzniveau enthält
Rnew <- matrix(paste(R, stars_significance, sep=""), ncol=ncol(corMT_controls))
diag(Rnew) <- paste(diag(R), " ", sep="")
rownames(Rnew) <- colnames(corMT_controls)
colnames(Rnew) <- paste(colnames(corMT_controls), "", sep="")


# Auswählen der relevanten Spalten der neuen Korrelationsmatrix

library(dplyr)

Rnew <- Rnew [, 1:3]
view(Rnew)


# Kontrollvariablen für die Regression mit "selfworth" als Zielvariable:
# Alle Variablen des Mittagstischs, die sowohl mit der Zielvariable "selfworth" als auch mit der
# Treatment-Variable "TreatEF" korreliert sind und der Korrelationskoeffizient statistisch
# signifikant ist, werden als Kontrollvariablen in Betracht gezogen:

# dayToDaySkills, tasksLunch, monthlyCooks, weeklyCooks, shoopers, easyDishes, dietaryKnowledge,
# appreciateHealthy, foodCulture, moreConcentrated, moreBalanced, moreIndependent, moreOpen, 
# moreConfidence, adressProblems, proud, enoughFood


# Kontrollvariablen für die Regression mit "dayToDaySkills" als Zielvariable:
# # Alle Variablen des Mittagstischs, die sowohl mit der Zielvariable "dayToDaySkills" als auch
# mit der Treatment-Variable "TreatEF" korreliert sind und der Korrelationskoeffizient statistisch
# signifikant ist, werden als Kontrollvariablen in Betracht gezogen:

# selfworth, DGECriterialNo, subsidy, tasksLunch, monthlyCooks, weeklyCooks, shoppers, easyDishes,
# dietaryKnowledge, appreciateHealthy, foodCulture, moreConcentrated, moreBalanced,
# moreIndependent, moreOpen, moreConfidence, adressProblems, proud, enoughFood
