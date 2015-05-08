
require(ggplot2)

SimPlotPrints <- function(df, simResult, title= ""){
  n_printed <- sum(df$printed == "Printed")
  linetext = paste('Observed value = ', n_printed)
  simPlot <- ggplot(simResult)
  simPlot + geom_histogram(aes(totalPrinted), color = 'black', 
                           fill = 'white', binwidth = 5) + 
    geom_vline(xintercept = n_printed, color = 'black', size = 2) + 
    labs(x = 'Number of printed designs', y = 'Count', 
         title = title, size = 4) 
    #geom_text(x = 78, y = 2550, label = linetext)
}

SimPlotScore <- function(df, simResult, title = ""){
  avg.score <- round(mean(df$avg.score[df$printed == "Printed"]), 2)
  linetext = paste('Observed value = ', avg.score)
  simPlot <- ggplot(simResult)
  simPlot + geom_histogram(aes(avgScore), color = 'black', fill = 'white') + 
    geom_vline(xintercept = mean(df$avg.score[df$printed == "Printed"]), 
               color = 'black', size = 2) + 
    labs(x = 'Mean average score', y = 'Count', 
         title = title, size = 4)
    #geom_text(x = 3.25, y = 1030, label = linetext, size = 7)
}

SimPlotSD <- function(df, simResult, title = ""){
  std.dev <- round(sd(df$avg.score[df$printed == "Printed"]),2)
  linetext = paste('Observed value = ', std.dev)
  simPlot <- ggplot(simResult)
  simPlot + geom_histogram(aes(SD), color = 'black', fill = 'white') + 
    geom_vline(xintercept = sd(df$avg.score[df$printed == "Printed"]), 
               color = 'black', size = 2) +
    labs(x = 'Standard deviation', y = 'Count', 
         title = title, size = 4)
    #geom_text(x = 0.5, y = 1030, label = linetext, size = 7)
}


