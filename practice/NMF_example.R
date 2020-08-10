#' Non-negative Matrix Factorization (NMF) of a given matrix V 
#' @description Perform NMF on a given matrix V, where V ~= W %*% H
#' @author Yihuang Kang
#' @param V Original matrix with all positive cell values
#' @param k rank 
#' @param numOfIter Number of max iterations. 1000 by default.
#' @param method The method of NMF. Now only support Lee’s method and use Frobenius norm in the objective function
#' @return Return a list with W & H matrices, distance/error, and number of iterations  ,  
#' @references https://en.wikipedia.org/wiki/Non-negative_matrix_factorization
#' @examples  m = matrix( round(runif(60, 1, 5)), ncol = 6);
#' m[1:3, 1:3]  = m[1:3, 1:3]  + 10; m[8:10, 4:6]  = m[8:10, 4:6]  + 10;
#' nmf_m = nmffit(V, k = 2, numOfIter = 1000)
#'  
nmffit = function(V, k, numOfIter = 1000,  method = "lee"){
  
  # Dimension of original matrix
  n = nrow(V); p = ncol(V)
  # Initializing W and H matrices, with random 0-max value of V matrix.
  W = matrix(runif(n * k, 0, max(V)), nrow = n, ncol = k)
  H = matrix(runif(k * p, 0, max(V)), nrow = k, ncol = p)  
  
  # For those cell values < EPS, impute with EPS
  H[H < .Machine$double.eps] = .Machine$double.eps;
  W[W < .Machine$double.eps] = .Machine$double.eps;
  
  
  # Objective/error function
  objFun = function(V, W, H){
    return( norm(V - (W %*% H), type = "F") )
  }  
  # Current and new Frobenius norms of V
  FNormV = newFNormV = objFun(V, W, H);
  H = (H * ( t(W) %*% V)) / ( t(W) %*% W %*% H )
  W = W * (V %*% t(H) ) / ( W %*% H %*% t(H))
  
  for(i in 1:numOfIter){
    
    H[H < .Machine$double.eps] = .Machine$double.eps;
    W[W < .Machine$double.eps] = .Machine$double.eps;
    
    H = H * ( (t(W) %*% V) / (t(W) %*% W %*% H) )
    W = W * ( (V %*% t(H)) / ( W %*% H %*% t(H)) )    
    
    newFNormV = objFun(V, W, H)
    
    print(paste("Error =", round(newFNormV, 4),  "Iteration = ", i))
    # If distance/error converage
    if(all.equal.numeric( objFun(V, W, H) , FNormV, tolerance = sqrt(.Machine$double.eps)) == T) break
    else FNormV = newFNormV;
  }
  
  return(list("W" = W, "H" = H, "NumberOfIterations" = i))
  
}#end of function




#NMF package

# default NMF algorithm
result = nmf(m,2)
# W & H matrix
w = basis(result)
h = coef(result)

nmfAlgorithm()
nmf(m, 3, method = "lee", seed=777)



