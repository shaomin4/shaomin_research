source("R/myImagePlotV2.R")
#source("R/termProbPlot.R")
monitorTerm = function(term, periods, numOfPeriods, numOfTopics, p_term_topic_list){
  prob_divergence = list()
  prob_divergence$probVectors = matrix(NA, ncol = numOfPeriods, nrow = numOfTopics)
  
  for(i in 1:numOfPeriods){
    prob_divergence$probVectors[, i] = p_term_topic_list[[i]][ term,]
  }
  GJSD = rep(NA, numOfPeriods - 1)
  GJSD_kt = rep(NA, numOfPeriods - 1)
  for(i in 2:numOfPeriods){
    GJSD[i - 1] = ykang::gjsdivergence(prob_divergence$probVectors[, 1:i])
    # GJSD threshold
    GJSD_kt[i - 1] = ykang::expectedGJSD(0.01, i * numOfTopics, numOfTopics, i)
  }
  prob_divergence$GJSD = GJSD  
  prob_divergence$GJSD_kt = GJSD_kt
  
  #dev.off()
  g <- myImagePlot(prob_divergence$probVectors, title = term, yLabels = paste("Topic_", 1:numOfTopics, sep = ""), xLabels = periods)
  print(g)
  return(prob_divergence)
}