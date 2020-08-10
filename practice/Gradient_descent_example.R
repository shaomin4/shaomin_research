lm(mpg ~ wt, data= mtcars)

y = mtcars$mpg
x = mtcars$wt
x = cbind(1,x)
theta = matrix(c(0,0), nrow=2)
m <- nrow(mtcars)

costFun = function(x, y, theta){
  return( sum((x %*% theta - y)^2)/ (2*m) )
}
costFun(x,y,theta)

cost = costFun(x,y,theta)

alpha <- 0.01
numOfIter <- 150000

for(i in 1:numOfIter){
  theta = theta - alpha * (t(x) %*% (x %*% theta - y) / length(y))
  NumberOfIterations = i
  if(all.equal.numeric( costFun(x,y,theta) , cost, tolerance = sqrt(.Machine$double.eps)) == T) break
  else cost = costFun(x,y,theta)
  print(cost)
}
theta
NumberOfIterations
