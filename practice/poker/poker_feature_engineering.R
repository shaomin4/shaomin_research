#' Title
#'
#' @param formula 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
library(doMC)

poker_feature_engineering <- function(formula = NULL, data = NULL, parallel_cores = 1){
  # Response variable name
  resVar = all.vars(formula)[1]
  
  predVar = setdiff(colnames(data), resVar)
  
  data_label = data[,resVar]
  
  numOfData = nrow(data)
  
  registerDoMC(parallel_cores)
  
  data <- foreach(idx_set = split(seq(numOfData),
                                  ceiling(seq_along(seq(numOfData))/(numOfData/parallel_cores))),
                  .combine = rbind, .verbose = F) %dopar% {
                    cards <- t(apply(data[idx_set ,predVar], 1, function(x){
                      sort_card = sort(x[c(2, 4, 6, 8, 10)], decreasing = T)
                      order_card = order(x[c(2, 4, 6, 8, 10)], decreasing = T)
                      order_card = c(2, 4, 6, 8, 10)[order_card] - 1
                      result = unlist(sapply(1:5, function(i) return(c(x[order_card[i]], sort_card[i]))))
                      return(result)
                    }))
                  }
  
  registerDoSEQ()
  
  data = data.frame(data_label, data)
  
  data$T1 = data$X2 - data$X4
  data$T2 = data$X4 - data$X6
  data$T3 = data$X6 - data$X8
  data$T4 = data$X8 - data$X10
  
  return(data)
}