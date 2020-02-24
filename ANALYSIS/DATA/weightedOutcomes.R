library(dplyr)

weightOutcome <- function(expr, df, weight) {
  expr <- enquo(expr)
  weight <- enquo(weight)
  name <- paste0(quo_name(expr), "_weighted")
  
  mutate(df,
         !! name := (!! weight)*0.25*(!! expr)
  )
}

testWeighted <- weightOutcome(lessIll, mergedData, eatersPerMealNo)
