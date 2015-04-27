# Function to print results of logistic regression model

print_stats <- function(model.1){
  chi.stats(model.1)
  logisticPseudoR2s(model.1)
  cat('Odds ratios:\n')
  print(round(exp(model.1$coefficients),3))
  cat('Confidence intervals:\n')
  print(round(exp(confint(model.1, level = .99)), 3))
}
