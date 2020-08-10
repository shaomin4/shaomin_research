#' k-nary Generalized Jensen-Shannon Divergence for a set of discrete probability vectors
#' @description Calculate k-nary Generalized Jensen-Shannon Divergence for a set of discrete probability vectors
#' This function requires the shannonentropy() in the package "ykang".
#' @author Yihuang Kang
#' @param pVecs A k by m matrix or data frame with m column discrete probability vectors
#' with k components. Note that the sum of the cell probabilities in each column must be 1. 
#' @param wVec A weight vector that contains m weights for m column provbabilty vector.
#' If wVec is not provided, all vecotors are equally-weighted. e.g. 1/m for m vecotrs. 
#' @return Return the GJSD between 0 and 1
#' @examples gjsdivergence(matrix(c(0.5, 0.5, 0.3, 0.7, 0.1, 0.9), ncol = 3 , nrow = 2))

source("R/shannon_entropy.R")
 
gjsdivergence <- function(pVecs, wVec = NA){
  
  # Get k and m for the matrix
  k <- dim(pVecs)[1]
  m <- dim(pVecs)[2]  

  # piH is a temporary row vector that contains pi * H(P), where H() is k-nary Shannon Entropy 
  pi_H <- vector(length = m)
  
  # H_pi is a temporary row vector that contains H(pi * P), where H() is k-nary Shannon Entropy   
  H_pi <- vector(length = m)
  
  if(!any(is.na(wVec))){      
    # Check if the weight vector is a vector, and summed up to 1
    stopifnot(is.vector(wVec), sum(wVec) == 1)
  } else {
    # wVec is NA. Set all vectors equally-weighted
    wVec <- vector(length = m)
    wVec[] <- 1 / m 
  }
  
  # Check if each column are summed up to 1
  if(!(is.matrix(pVecs) | is.data.frame(pVecs))){
    stop('pVecs must be a matrix or data frame!')
  } else {
    if(!all(sapply(colSums(pVecs), FUN = function(x){all.equal.numeric(x,1)} == "TRUE")))
    {
      stop('At least one of the sum(s) of the column vector does not equal to 1.\nPlease check the matrix/data frame')  
      print('colSums(pVecs)')
      print(colSums(pVecs))
    }       
  }
  
  pi_H <- sum(wVec * apply(pVecs, MARGIN = 2, FUN = shannon_entropy))
  H_pi <- shannon_entropy(rowSums(sweep(x = pVecs, MARGIN = 2, STATS = wVec, FUN = "*")))

  return(H_pi - pi_H)

}
