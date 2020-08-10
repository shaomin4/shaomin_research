library(mxnet)
# create a 2-by-3 matrix on cpu
a <- mx.nd.zeros(c(2, 3))
# create a 2-by-3 matrix on cpu
b <- mx.nd.zeros(c(2, 3), mx.cpu())
# create a 2-by-3 matrix on gpu 0, if you have CUDA enabled.
c <- mx.nd.zeros(c(2, 3), mx.gpu(0))

#initialize NDArray in various ways
a <- mx.nd.ones(c(4, 4))
b <- mx.rnorm(c(4, 5))
c <- mx.nd.array(1:5)#array,vector or matrix
t <- matrix(1:6, nrow = 2, ncol = 3)
c <- mx.nd.array(t)
as.array(c)

#Basic Operations
a <- mx.nd.ones(c(2, 2)) * 2
b <- mx.nd.ones(c(2, 2)) / 8

#need to move ndarray to same divice
a <- mx.nd.ones(c(2, 3)) * 2
b <- mx.nd.ones(c(2, 3), mx.gpu()) / 8
#a+b will error here because they are not in same divice
#use mx.nd.copyto function to move to same place
c <- mx.nd.copyto(a, mx.gpu()) * b
as.array(c)

#save ndarray in disk with list type
a <- mx.nd.ones(c(2, 3))
mx.nd.save(a, "temp.ndarray")
b <- mx.nd.load("temp.ndarray")
b[[1]]

