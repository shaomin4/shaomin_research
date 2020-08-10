#' Load Document--term Matrix and Transfer into a Sparse Matrix
#' @description 
#' @author Shengtai Huang
#' @param file 
#'
#' @return
#' @export
#'
#' @examples

library(tm)
source("R/tf_idf.R")

load_dtm <- function(file = NULL, tfIdf = TRUE) {
  
  data_name <- load(file)
  dtm <- as.matrix(eval(parse(text = data_name)), sparse = T)
  if (tfIdf) {
    dtm <- tf_idf(dtm)
  }
  dtm <- dtm[rowSums(dtm) > 0, colSums(dtm) > 0]
  return(dtm)
}