#' Calculate topic distances and topic assignment of NMF object using Hungarian algorithm 
#' 
#' @description Using Hungarian algorithm to compute 2 P(terms | topic) of transposed H_hat matrix in 2 NMF objects
#' Here we use ykang::gjsdivergence() and clue::solve_LSAP()
#' @author Yihuang Kang
#' @param t_H_hat_1 transposed H_hat matrix 1
#' @param t_H_hat_2 transposed H_hat matrix 2
#' @return Return a topic distance upper matrix along with the "to" topic number vetor 
#' 
hungarianAssignNMF <- function(t_H_hat_1, t_H_hat_2){
  

  #print(dim(t_H_hat_1))
  #print(dim(t_H_hat_2))
  # Number of topics, k
  k = ncol(t_H_hat_1) 
  
  distMatrix = matrix(data = NA, nrow = k, ncol = k)
  colnames(distMatrix) = rownames(distMatrix) = colnames(t_H_hat_1)
  
  # Calculate JSDs
  
  for(i in 1:k){
    for(j in 1:k){
      #print(paste("i = ", i, "j = ", j))
      distMatrix[i, j] = ykang::gjsdivergence( cbind(t_H_hat_1[,i], t_H_hat_2[,j]) )
    }
  }
  print(distMatrix)
  return( as.vector(clue::solve_LSAP(distMatrix, maximum = F) ))
}
