# Function to calculate R2 values for logistic regression
# From Field et al (2012) Discovering Statistics Using R, 
# SAGE Publications, p.334

logisticPseudoR2s <- function(LogModel){
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1 - exp(-(nullDev - dev)/modelN)
  R.n <- R.cs / (1-(exp(-(nullDev / modelN))))
  cat('Pseudo R^2 for logistic regression\n')
  cat('Hosmer and Lemeshow R^2   ', round(R.l, 3), '\n')
  cat('Cox and Snell R^2         ', round(R.cs, 3), '\n')
  cat('Nagelkerke R^2            ', round(R.n, 3), '\n')
}