#' Calculate Mutual Information of Join Probabilities of 2 Discrete Random Variables
#' @description Calculate the Mutual Information (given a BASE. 2 by default) of a 
#' matrix with joint probabilities of 2 discrete random variables
#' @author Yihuang Kang
#' @param jointProbMatrix A joint probability matrix (matrix object).
#' @param base The base of log()
#' @return Return Mutual Information(MI) ,  
#' @references http://en.wikipedia.org/wiki/Mutual_information
#' @examples jm <- matrix(c(0.1, 0.2, 0.3, 0.1, 0.1, 0.2), ncol = 2 ,nrow = 3)
#' discreteMI(jm)

source("R/inf2eps.R")

discreteMI <- function(jointProbMatrix, base = 2){
  # Check if all row sums of the join probabilities in the matrix are equal to 1.
  if(!(all.equal.numeric(sum(jointProbMatrix), 1.0, tolerance = 1e-6))) {
    stop("discreteMI(): Sum of the jointProbMatrix is not 1!")
  }
  
  rowMarginalProb <- matrix(base::rowSums(jointProbMatrix), ncol = 1)
  colMarginalProb <- matrix(base::colSums(jointProbMatrix), nrow = 1)
  
  # p(x) * p(y)
  Px_by_Py <- rowMarginalProb %*% colMarginalProb
  
  temp <- log(jointProbMatrix / Px_by_Py, base)
  # Replace those Inf/-Inf with eps
  temp <- inf2eps(temp)
    
  MI <- sum(jointProbMatrix * temp)
  
  return(MI)
  
}
