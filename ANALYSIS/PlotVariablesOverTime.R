#inspectinfluenceofvariablesovertime

library(ggplot2)
library(cowplot)
library(purrr)
library(dplyr)

#setthevariables

response= names(mergedData)[5:119]
expl= names(mergedData)[4]

response = set_names(response)
response

expl = set_names(expl)
expl

#createfunctionHEREGGPLOT

plotOutcomeOverTime = function(x, y){
  ggplot(mergedData, aes_string(x = x, y = y) ) +
    geom_point() + 
    geom_smooth(method = "loess", se = FALSE, color = "grey74") + 
    theme_bw() 
}

#checkifthefunctionworks

plotOutcomeOverTime("year", "lessIll")

#mapthefunction

YearlyOutcome <- map(mergedData, ~plotOutcomeOverTime("year", "lessIll"))

all_yearPlots = map(response,
                ~map(expl, plotDGEOutcome, y = .x) )

#printtheplots #allvaribalescanbeplotted #examples

all_yearPlots$age[1:2]
all_yearPlots$eatersPerMealNo[1:2]
all_yearPlots$newKidsNo[1:2]
all_yearPlots$cateringNo[1:2]
