### Difference in difference estimator unter Verwendung der Mittelwerte

# Pre-Periode (= vor dem Treatment): 2011

data_pre_control <- subset(data_control, data_control$year == "2011") 
data_pre_treat <- subset(data_treat, data_treat$year == "2011")

# Post-Periode (= nach dem Treatment): 2012 - 2018

data_post_control <- subset(data_control, data_control$year != "2011")
data_post_treat <- subset(data_treat, data_treat$year != "2011")

### Mittelwerte für Treatment- und Kontrollgruppe in Pre- und Post-Periode bilden

# Mittelwert für "dayToDaySkills" in der Kontrollgruppe in der Pre-Periode:
mean_pre_control <- mean(data_pre_control$dayToDaySkills, na.rm = TRUE)

# Mittelwert für "dayToDaySkills" in der Treatmentgruppe in der Pre-Periode:
mean_pre_treat <- mean(data_pre_treat$dayToDaySkills, na.rm = TRUE)

# Mittelwert für "dayToDaySkills" in der Kontrollgruppe in der Post-Periode:
mean_post_control <- mean(data_post_control$dayToDaySkills, na.rm = TRUE)

# Mittelwert für "dayToDaySkills" in der Treatmentgruppe in der Post-Periode:
mean_post_treat <- mean(data_post_treat$dayToDaySkills, na.rm = TRUE)

### Berechnung des doppelten Differenzenschätzers

DiD <- (mean_post_treat - mean_pre_treat) - (mean_post_control - mean_pre_control)

DiD <- (mean_post_treat - mean_post_control) - (mean_pre_treat - mean_pre_control)



