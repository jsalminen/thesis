# Function to calculate chi-square statistics
# Adapted from Field et al (2012) Discovering Statistics Using R, 
# SAGE Publications, p.332

chi.stats <- function(glm.model){
  modelChi <- glm.model$null.deviance - glm.model$deviance
  chidf <- glm.model$df.null - glm.model$df.residual
  chisq.prob <- 1 - pchisq(modelChi, chidf)
  cat('modelChi      ', modelChi, '\n')
  cat('chidf         ', chidf, '\n')
  cat('chisq.prob    ', chisq.prob, '\n')
}