library(httr)
library(plumber)

h2o_host <- "192.168.1.100"
h2o_port <- "54321"

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}


#' Get CPU ticks
#' @param nodeIdx node index
#' @get /WaterMeterCpuTicks/<id>
#' Echo JSON text
function(id) {
  r <- GET(paste0(h2o_host, ":", h2o_port, "/3/WaterMeterCpuTicks/", id))
  content <- content(r, "text", encoding="UTF-8")
  return(jsonlite::fromJSON(content))
  # res$body <- content
}

#' Get cloud status
#' json text
#' @get /Cloud
#' Echo JSON text
function(){
  r <- GET(paste0(h2o_host, ":", h2o_port, "/3/Cloud"))
  content <- content(r, "text", encoding="UTF-8")
  return(jsonlite::fromJSON(content))
  # res$body <- content
}

#' Plot a histogram
#' @png
#' @get /plot
function(){
  rand <- rnorm(100)
  hist(rand)
}

#' Return the sum of two numbers
#' @param a The first number to add
#' @param b The second number to add
#' @get /sum
function(a, b){
  as.numeric(a) + as.numeric(b)
}
