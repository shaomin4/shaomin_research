# Gradient descent for linear models
lm_GD = function(y, x, eta = NULL,  numOfIter = 1000){
  if(is.null(eta)) eta = abs(max(y) - min(x)) / 1000
  
  print(paste("eta =", eta))
  
  x = as.matrix(cbind(1, x), ncol = 1)
  y = as.matrix(y, ncol = 1)
  
  # Initializing beta_0 and beta_1
  beta = as.matrix(rnorm(2, 0, 1), ncol = 1)
  
  # learning rate = eta
  gradFun = function(x, y, beta) {
    return(  1/nrow(x)    *t(x) %*% (y - (x %*% beta))  )
  }
  # gradient descent
  for(i in 1:numOfIter) {
    delta = eta * gradFun(x, y, beta)
    beta = beta + delta
    print(paste("delta = ", delta))
    print(paste("beta = ", beta))    
    if(abs(min(delta)) <= 10^-10) {
      print(paste("Stop at iteration", i)); break
    }
  }
  return(beta)
}

lm_GD(y = mtcars$mpg, x = mtcars$wt, eta = 0.1)
lm( mpg ~ wt, mtcars )
