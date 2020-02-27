library(ggfortify)

#use fortify to convert ts to ggplot

df_dayToDaySkills_treat <- fortify(ts_dayToDaySkills_treat)

# rename 

df_dayToDaySkills_treat <- df_dayToDaySkills_treat %>% 
  rename(Year = Index, EverydayExpertise = Data)

#look if it works

plot_dayToDaySkills_treat <- ggplot(df_dayToDaySkills_treat, aes(Year, Data)) + geom_line()

df_dayToDaySkills_control <- fortify(ts_dayToDaySkills_control) 
df_dayToDaySkills_control <- df_dayToDaySkills_control %>% 
  rename(Year = Index, EverydayExpertise = Data)
ggplot(df_dayToDaySkills_control, aes(Year, EverydayExpertise)) + geom_line()

#selfworth

df_selfworth_treat <- fortify(ts_selfworth_treat)
df_selfworth_treat <- df_selfworth_treat %>% 
  rename(Year = Index, EverydayExpertise = Data)
ggplot(df_dayToDaySkills_treat, aes(Year, EverydayExpertise)) + geom_line()

df_selfworth_control <- fortify(ts_selfworth_control)
df_selfworth_control <- df_selfworth_control %>% 
  rename(Year = Index, EverydayExpertise = Data)
ggplot(df_selfworth_control, aes(Year, EverydayExpertise)) + geom_line()

#weeklycooks

df_weeklyCooks_treat <- fortify(ts_weeklyCooks_treat) 
df_weeklyCooks_treat <- df_weeklyCooks_treat %>% 
  rename(Year = Index, EverydayExpertise = Data)
ggplot(df_weeklyCooks_treat, aes(Year, EverydayExpertise)) + geom_line()

df_weeklyCooks_control <- fortify(ts_weeklyCooks_control)
df_weeklyCooks_control <- df_weeklyCooks_control %>% 
  rename(Year = Index, EverydayExpertise = Data)
ggplot(df_weeklyCooks_control, aes(Year, EverydayExpertise)) + geom_line()

#monthlyCooks

df_monthlyCooks_treat <- fortify(ts_monthlyCooks_treat) 
df_monthlyCooks_treat <- df_monthlyCooks_treat %>% 
  rename(Year = Index, EverydayExpertise = Data)
ggplot(df_dayToDaySkills_treat, aes(Year, EverydayExpertise)) + geom_line()

df_monthlyCooks_control <- fortify(ts_monthlyCooks_control) 
df_monthlyCooks_control <- df_monthlyCooks_control %>% 
  rename(Year = Index, EverydayExpertise = Data)
ggplot(df_monthlyCooks_control, aes(Year, EverydayExpertise)) + geom_line()


#erstellen der ggplots

#dayToDaySkills

ggplot(df_dayToDaySkills_treat, aes(Year, EverydayExpertise)) + geom_line() + geom_line(data = df_dayToDaySkills_control)


<<<<<<< HEAD
plot_dayToDaySkills_treat <- ggplot(df_dayToDaySkills_treat, aes(Year, Data)) + geom_line() 
=======
>>>>>>> b634cdd7ad05b18d864605ea08276740354b73fe




