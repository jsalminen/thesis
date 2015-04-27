simulation <- function(df, samples = 1000){
  totalPrinted <- c()
  avgScore <- c()
  SD <- c()
  for( i in 1:samples){
    df$random <- runif(nrow(df))
    df$simPrint[df$random <= df$predicted.probabilities] <- 1
    df$simPrint[df$random > df$predicted.probabilities] <- 0
    totalPrinted[i] <- sum(df$simPrint)
    avgScore[i] <- mean(df$avg.score[df$simPrint == 1])
    SD[i] <- sd(df$avg.score[df$simPrint == 1])
  }
  result <- data.frame(totalPrinted = totalPrinted, avgScore = avgScore, SD = SD)
  return(result)
}

baseline <- function(train, test, samples = 1000){
  totalPrinted <- c()
  avgScore <- c()
  SD <- c()
  real.printed <- sum(as.numeric(train$printed == "Printed"))
  predicted.probabilities <- real.printed/nrow(train)
  for( i in 1:samples){
    test$random <- runif(nrow(test))
    test$simPrint[test$random <= predicted.probabilities] <- 1
    test$simPrint[test$random > predicted.probabilities] <- 0
    totalPrinted[i] <- sum(test$simPrint)
    avgScore[i] <- mean(test$avg.score[test$simPrint == 1])
    SD[i] <- sd(test$avg.score[test$simPrint == 1])
  }
  result <- data.frame(totalPrinted = totalPrinted, avgScore = avgScore, SD = SD)
  return(result)
}
