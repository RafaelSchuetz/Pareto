### DiD-Schätzungen

# Erstellung von Interaktionstermen, in denen die Treatment-Variable mit den Jahres-Dummies interagiert werden

dfcEF$treat_2011 <- (dfcEF$treatEF*dfcEF$dummy_2011)

dfcEF$treatEF <- as.numeric(dfcEF$treatEF)

dfcEF$treat_2012 <- (dfcEF$treatEF*dfcEF$dummy_2012)
