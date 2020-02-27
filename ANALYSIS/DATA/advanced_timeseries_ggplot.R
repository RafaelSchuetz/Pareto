#davor durchlaufen lassen: makegather, control_group, advanced_timeseries

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
library(ggpubr)

#dayToDaySkills

ggplot(df_dayToDaySkills_treat, aes(Year, EverydayExpertise)) + 
  geom_point() + geom_line(color = "grey0") + 
  geom_point(data = df_dayToDaySkills_control, color = "grey") + 
   geom_line(data = df_dayToDaySkills_control, linetype = "dotted", color = "grey10") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "grey0") +
  geom_smooth(data =df_dayToDaySkills_control, method="lm", se = FALSE, color = "grey") +
  ggtitle("Trend of everyday expertise") + ylab("Everyday Expertise")
  


#versuch, legende hinzuzufügen


ggplot(df_dayToDaySkills_treat, aes(Year, EverydayExpertise)) + 
  geom_point() + 
  geom_line(color = "grey0", aes(x=Year, y=EverydayExpertise, color="Treatment")) + 
  geom_point(data = df_dayToDaySkills_control, color = "grey") + 
  geom_line(data = df_dayToDaySkills_control, 
            linetype = "dotted", color = "grey10", aes(x=Year, y=EverydayExpertise, color="Control")) +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "grey0") +
  geom_smooth(data =df_dayToDaySkills_control, method="lm", se = FALSE, color = "grey") +
  ggtitle("Trend of everyday expertise") + ylab("Everyday Expertise") +
  scale_color_discrete(name="legend")



  













