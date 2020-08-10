# plumber
library(plumber)
r <- plumb("plumber.R")
r$run(host = "192.168.1.5", port = 5556)
