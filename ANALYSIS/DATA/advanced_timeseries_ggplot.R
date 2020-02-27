library(ggfortify)

#use fortify to convert ts to ggplot

df_dayToDaySkills_treat <- fortify(ts_dayToDaySkills_treat)

#look if it works

ggplot(df_dayToDaySkills_treat, aes(Index, Data)) + geom_line()

df_dayToDaySkills_control <- fortify(ts_dayToDaySkills_control) 
ggplot(df_dayToDaySkills_control, aes(Index, Data)) + geom_line()

#selfworth

df_selfworth_treat <- fortify(ts_selfworth_treat) 
ggplot(df_dayToDaySkills_treat, aes(Index, Data)) + geom_line()

df_selfworth_control <- fortify(ts_selfworth_control) 
ggplot(df_selfworth_control, aes(Index, Data)) + geom_line()

#weeklycooks

df_weeklyCooks_treat <- fortify(ts_weeklyCooks_treat) 
ggplot(df_weeklyCooks_treat, aes(Index, Data)) + geom_line()

df_weeklyCooks_control <- fortify(ts_weeklyCooks_control) 
ggplot(df_weeklyCooks_control, aes(Index, Data)) + geom_line()

#monthlyCooks

df_monthlyCooks_treat <- fortify(ts_monthlyCooks_treat) 
ggplot(df_dayToDaySkills_treat, aes(Index, Data)) + geom_line()

df_monthlyCooks_control <- fortify(ts_monthlyCooks_control) 
ggplot(df_monthlyCooks_control, aes(Index, Data)) + geom_line()

#rename the variables





#erstellen der ggplots

#dayToDaySkills

ggplot(df_dayToDaySkills_treat, aes(Index, Data)) + geom_line() + geom_line(data = df_dayToDaySkills_control)






