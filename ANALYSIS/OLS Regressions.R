# use package hdm to perform double selection: OLS regression after feature selection
# https://insightr.wordpress.com/2017/08/04/the-package-hdm-for-double-selection-inference-with-a-simple-example/

library(hdm)


dataset <- mergedData %>% 
  select_if(is.numeric)

# drop_na drops rows containing missing values

# select rows with year in which DGECriteriaNo was recorded

dataset <- mergedDataImputeMode %>% 
  filter(year %in% c(2018, 2017, 2016, 2014))
  # dplyr::select(#participateMore, 
  #               # tasksLunch,
  #               # monthlyCooks,
  #               # weeklyCooks,
  #               # shoppers,
  #               # ownIdeas,
  #               # stayLonger,
  #               # easyDishes,
  #               # dietaryKnowledge,
  #               #appreciateHealthy,
  #               # foodCulture,
  #               # influenceHome,
  #               # cookAtHome,
  #               # askRecipes,
  #               # moreConcentrated,
  #               # moreBalanced,
  #               # lessIll,
  #               dayToDaySkills,
  #               moreIndependent,
  #               betterTeamwork,
  #               betterReading,
  #               betterNumbers,
  #               betterGrades,
  #               moreRegularSchoolVisits,
  #               selfworth,
  #               moreOpen,
  #               moreConfidence,
  #               addressProblems,
  #               proud,
  #               DGECriteriaNo,
  #               realSubsidy) %>% 
  # drop_na()

NAsPerVariableMergedData <- mergedData %>% 
  summarise_all(list(~ sum(is.na(.)))) %>% 
  arrange(.)

datasetNewName <- dataset %>% 
  rename(X3 = realSubsidy)

fmNewName = paste("participateMore ~", paste(colnames(select(datasetNewName, -"participateMore")), collapse="+"))
fmNewName = as.formula(fmNewName)

fmOldName = paste("participateMore ~", paste(colnames(select(dataset, -"participateMore")), collapse="+"))
fmOldName = as.formula(fmOldName)

#dataset <- join(dataset, )

# rlassoEffect performs double selection

xOldName <- as.matrix(dataset %>% 
  select(., -lessIll))
yOldName <- as.matrix(dataset$lessIll)
dOldName <- as.matrix(dataset$DGECriteriaNo)

DSOldName = rlassoEffect(xOldName, yOldName, dOldName)

summary(DSOldName)
DSNewName = rlassoEffects(fmNewName, I = ~ DGECriteriaNo + dayToDaySkills + X3, data=datasetNewName)
DSOldName = rlassoEffects(fmOldName, I = ~ DGECriteriaNo + dayToDaySkills + realSubsidy, data=dataset)

# loop for regressions with varying outcome and features

flexibleRegression <- function(x, y) {
  #y <- x$y
  xOldName <- as.matrix(x %>% 
                          select(., - !!y))
  yOldName <- as.matrix(x %>% 
                          select(., !!y))
  dOldName <- as.matrix(dataset$DGECriteriaNo)
  return(xOldName)
  return(yOldName)
  #rlassoEffect(xOldName, yOldName, dOldName)
}

DSflexibleTest <- flexibleRegression(dataset, "lessIll")

dataset %>% 
  map(~ flexibleRegression(.x, .))

lasso.effect = rlassoEffects(as.matrix(dataset), lessIll, index=3)

library(hdm); library(ggplot2)
set.seed(1)
n = 38 #sample size
p = 20 # number of variables
s = 3 # nubmer of non-zero variables
X = matrix(rnorm(n*p), ncol=p)
#X[1,1] = NA
colnames(X) <- paste("X", 1:p, sep="")
beta = c(rep(3,s), rep(0,p-s))
y = 1 + X%*%beta + rnorm(n)
data = data.frame(cbind(y,X))
data <- data %>% 
mutate(X1 = dataset$DGECriteriaNo, X2 = dataset$realSubsidy, X3 = dataset$lessIll, X4 = dataset$dayToDaySkills)
dataOrdinal <- data %>% 
  rename(DGECriteriaNo = X1, realSubsidy = X2, lessIll = X3) # , dayToDaySkills = X4
dataVerbal <- data %>% 
  rename(DGECriteriaNo = X1, realSubsidy = X2, lessIll = X3, dayToDaySkills = X4) 
colnames(dataOrdinal)[1] <- "y"
colnames(dataVerbal)[1] <- "y"
fmOrdinal = paste("y ~", paste(colnames(subset(dataOrdinal, select = -y)), collapse="+"))
fmOrdinal = as.formula(fmOrdinal) 
fmVerbal = paste("y ~", paste(colnames(subset(dataVerbal, select = -y)), collapse="+"))
fmVerbal = as.formula(fmVerbal)  
#lasso.effect = rlassoEffects(X, y, index=c(1,2,3))
lasso.effect = rlassoEffects(fmOrdinal, I = ~ DGECriteriaNo + realSubsidy + lessIll + X4, data=dataOrdinal)
lasso.effect = rlassoEffects(fmVerbal, I = ~ DGECriteriaNo + realSubsidy + lessIll + dayToDaySkills, data=dataVerbal)
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

