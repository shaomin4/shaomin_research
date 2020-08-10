library(dplyr)
library(tidyr)

a <- c(1,1,1,2,2,2,3,3,3)
b <- sample(1:5,9,replace = T)
c <- c(1,2,5,2,3,4,1,2,3)
s <- data.frame(a,b,c)


s %>% spread(c,b)
# nmfgpu4R environment variables
Sys.setenv(CUDA_PATH = "/usr/local/cuda")
Sys.setenv(CUDA_ROOT = "/usr/local/cuda")
Sys.setenv(CUDA_HOME = "/usr/local/cuda")
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/usr/local/cuda/bin", sep = ":"))
# NMFGPU_ROOT for .1.5 server
Sys.setenv(NMFGPU_ROOT = "/usr/local/nmfgpu")
# Initialize the library
library(nmfgpu4R)
nmfgpu4R.init()
# Choose GPU card we'd like
chooseDevice(0)

v <-  matrix(c(1,21,31,2,22,23,0,0,33), nrow=3)
s <- matrix(c(10,2,3,20,2,3,30,2,3), nrow=3)
# fit an NMF model
x <- nmf(v, 2)

predict(x, s)
