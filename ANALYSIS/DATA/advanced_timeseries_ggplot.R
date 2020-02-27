library(ggfortify)

df_dayToDaySkills_treat <- fortify(ts_dayToDaySkills_treat)
# ts to data.frame --------------------------------------------------------

#für dayToDaySkills:

# ts_reshape(ts_dayToDaySkills_treat, type = "wide", frequency = NULL)

#Error in ts_reshape(ts_dayToDaySkills_treat, type = "wide", frequency = NULL) : 
#The frequency of the series is invalid, the function support only 'weekly', 'monthly' or 'quarterly' frequencies

df_dayToDaySkills_treat <- data.frame(Y=as.matrix(ts_dayToDaySkills_treat), date=time(ts_dayToDaySkills_treat))

# class(df_dayToDaySkills_treat)

#immernoch ts

plot_dayToDaySkills_treat <- ggplot(df_dayToDaySkills_treat, aes(Index, Data)) + geom_line()
