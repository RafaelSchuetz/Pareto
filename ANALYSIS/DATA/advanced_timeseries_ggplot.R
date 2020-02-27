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

###dayToDaySkills

ggplot(df_dayToDaySkills_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_dayToDaySkills_control, color = "blue") + 
  geom_line(data = df_dayToDaySkills_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_dayToDaySkills_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Everyday Expertise") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Everyday Expertise")
  
##speichern

plot_dtds <- ggplot(df_dayToDaySkills_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_dayToDaySkills_control, color = "blue") + 
  geom_line(data = df_dayToDaySkills_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_dayToDaySkills_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Everyday Expertise") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Everyday Expertise")

###selfworth


ggplot(df_selfworth_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_selfworth_control, color = "blue") + 
  geom_line(data = df_selfworth_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_selfworth_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Selfworth") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Selfworth")

##save

plot_selfworth <- ggplot(df_selfworth_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_selfworth_control, color = "blue") + 
  geom_line(data = df_selfworth_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_selfworth_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Selfworth") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Selfworth")



###weeklycooks

ggplot(df_weeklyCooks_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_weeklyCooks_control, color = "blue") + 
  geom_line(data = df_weeklyCooks_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_weeklyCooks_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Weekly Cooks") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Weekly Cooks")



##save

plot_weeklyCooks <- ggplot(df_weeklyCooks_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_weeklyCooks_control, color = "blue") + 
  geom_line(data = df_weeklyCooks_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_weeklyCooks_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Weekly Cooks") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Weekly Cooks")


###monthlycooks


ggplot(df_monthlyCooks_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_monthlyCooks_control, color = "blue") + 
  geom_line(data = df_monthlyCooks_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_monthlyCooks_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Monthly Cooks") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Monthly Cooks")

##save

plot_monthlyCooks <- ggplot(df_monthlyCooks_treat, aes(Year, EverydayExpertise)) + 
  geom_point(color="red") + geom_line(color = "red") + 
  geom_point(data = df_monthlyCooks_control, color = "blue") + 
  geom_line(data = df_monthlyCooks_control, linetype = "dotted", color = "blue") +
  grids(axis = c("x"), color = "grey92", size = NULL, linetype = NULL) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_smooth(data =df_monthlyCooks_control, method="lm", se = FALSE, color = "blue") +
  ggtitle("Trend Of Monthly Cooks") +
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Monthly Cooks")

plot_dtds
plot_selfworth
plot_weeklyCooks
plot_monthlyCooks

saveRDS(plot_dtds, "./ANALYSIS/GRAPHS/plot_dtds.Rds")
saveRDS(plot_selfworth, "./ANALYSIS/GRAPHS/plot_selfworth.Rds")
saveRDS(plot_weeklyCooks, "./ANALYSIS/GRAPHS/plot_weeklyCooks.Rds")
saveRDS(plot_monthlyCooks, "./ANALYSIS/GRAPHS/plot_monthlyCooks.Rds")
