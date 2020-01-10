###inspectunfluenceofDGEcriterium

library(VGAM)

view(mergedData)

plot(mergedData$DGECriteriaNo, mergedData$lessIll, xlab="lessIll", ylab="DGE", col="darkblue", main="DGE_lessIll")
plot(mergedData$DGECriteriaNo, mergedData$dietaryKnowledge, xlab="dietaryKnowledge", ylab="DGE", col="darkblue", main="DGE_dietaryKnowledge")
plot(mergedData$DGECriteriaNo, mergedData$seasonalFoodstuff, xlab = "seasonalFood", ylab="DGE",col="darkblue", main = "DGE_seasonalFood")
plot(mergedData$DGECriteriaNo, mergedData$organicFoodstuff, xlab = "organicFood", ylab= "DGE",col="darkblue", main = "DGE_organicFood")
plot(mergedData$DGECriteriaNo, mergedData$appreciateHealthy, xlab = "appreciateHealthy", ylab= "DGE", col="darkblue", main = "DGE_appreciateHealthy")
plot(mergedData$DGECriteriaNo, mergedData$tasksLunch, xlab= "tasksLunch", ylab = "DGE",col="darkblue", main= "DGE_tasksLunch")

##proportionalOddsModel 

DGE_lessIll.vglm = vglm(lessIll ~  DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_lessIll.vglm)

DGE_dietaryKnowledge.vglm = vglm(dietaryKnowledge ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_dietaryKnowledge.vglm)

DGE_seasonalFoodstuff.vglm = vglm(seasonalFoodstuff ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_seasonalFoodstuff.vglm)

DGE_organicFoodstuff.vglm = vglm(organicFoodstuff ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_organicFoodstuff.vglm)

DGE_appreciateHealthy.vglm = vglm(appreciateHealthy ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_appreciateHealthy.vglm)

DGE_tasksLunch.vglm = vglm(tasksLunch ~ DGECriteriaNo, data = mergedData, family = propodds)
summary(DGE_tasksLunch.vglm)


##residualplots
##interpretationofpropodds??
##darstellung