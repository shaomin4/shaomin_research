#' Compute discrete k-nary Shannon Entropy
#'
#' @author Yihuang Kang
#' @param pVec A discrete probability distribution vector
#' @param k The "base" used in the calculation of the entropy
#' @description Given the base k, compute the discrete k-nary Shannon Entropy.
#' If the base k  is not provided, the number of components in the vecor will be used. 
#' @return k-nary Shannon Entropy
#' @examples  shannonentropy(c(0.5, 0.5))
#' shannonentropy( c(1/3,1/3,1/3), k = 2)
#' 
shannonentropy = function(pVec, k=length(pVec)){
  
  # Stop if the sum of the probability vector is not equal to 1
  if( round(sum(pVec),digits=10) != 1){
    stop("ykang::shannonentropy(): The sum of the probability vector is not equal to 1 !!" );
  }
  
  # Return 0 if there is only 1 component in the vector
  if(length(pVec) == 1 ){
    warning("There is only 1 component in the vector. 0 is returned. ")
    return(0);
  }  
  
  # entropy
  e = 0; 
  
  # a temporary vector
  tmpVec = c(NA);   
  length(tmpVec) = k;
  
  # Set all cells to 0
  tmpVec[] = 0; 
  
  # Only consider those with non-zero prob idx
  probidx = which(pVec != 0);
  
  tmpVec[probidx] = -1 * pVec[probidx] * log(x=pVec[probidx], base=k);
  
  e = sum(tmpVec);
  return(e);
  
}
