# use package hdm to perform double selection: OLS regression after feature selection
# https://insightr.wordpress.com/2017/08/04/the-package-hdm-for-double-selection-inference-with-a-simple-example/
library(hdm)

dataset <- mergedData %>% 
  select_if(is.numeric)

dataset <- mergedData %>% 
  dplyr::select(participateMore, 
                # tasksLunch,
                # monthlyCooks,
                # weeklyCooks,
                # shoppers,
                # ownIdeas,
                # stayLonger,
                # easyDishes,
                # dietaryKnowledge,
                # appreciateHealthy,
                # foodCulture,
                # influenceHome,
                # cookAtHome,
                # askRecipes,
                # moreConcentrated,
                # moreBalanced,
                lessIll,
                dayToDaySkills,
                moreIndependent,
                betterTeamwork,
                betterReading,
                betterNumbers,
                betterGrades,
                moreRegularSchoolVisits,
                selfworth,
                moreOpen,
                moreConfidence,
                addressProblems,
                proud,
                DGECriteriaNo,
                realSubsidy) %>% 
  drop_na()

fm = paste("lessIll ~", paste(colnames(select(dataset, -"lessIll")), collapse="+"))
fm = as.formula(fm)

DS=rlassoEffects(fm, I = ~DGECriteriaNo, method = "double selection", data=dataset)

lasso.effect = rlassoEffects(as.matrix(dataset), lessIll, index=3)

library(hdm); library(ggplot2)
set.seed(1)
n = 100 #sample size
p = 100 # number of variables
s = 3 # nubmer of non-zero variables
X = matrix(rnorm(n*p), ncol=p)
colnames(X) <- paste("X", 1:p, sep="")
beta = c(rep(3,s), rep(0,p-s))
y = 1 + X%*%beta + rnorm(n)
data = data.frame(cbind(y,X))
colnames(data)[1] <- "y"
fm = paste("y ~", paste(colnames(X), collapse="+"))
fm = as.formula(fm)                 
lasso.effect = rlassoEffects(X, y, index=c(1,2,3,50))
lasso.effect = rlassoEffects(fm, I = ~ X1 + X2 + X3 + X50, data=data)
print(lasso.effect)
summary(lasso.effect)
confint(lasso.effect)
plot(lasso.effect)

###model: OLS
##influence of DGEcriterium on health relevant variables

#DGEandLessIll

#DGEandAppreciateHealthy 

#DGEandDietaryKnowledge

#expandedModelLessIll





##approximating Chance Equality with the following proxies 

#dayTodaySkillsAndSubsidy 

#selfworthAndSubsidy

