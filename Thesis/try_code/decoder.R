z_mean <- dtm[1, ] %*% w[[1]] %*% w[[3]]
z_var <-  dtm[1, ] %*% w[[1]] %*% w[[5]]
z <- sapply(seq(10), function(X) rnorm(1, mean = z_mean[X], sd = sqrt(z_var[X])))

x_head <- z %*% w[[7]] %*% w[[9]]


