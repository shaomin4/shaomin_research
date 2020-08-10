#' Load Data from content_data
#' @description The data in content_data are stored in binary format, so we need to
#' unserialize them.
#' @author Shengtai Huang
#' @param file A character string giving the name of the file to load.
#' @return A decompressed object from given "file".
#' @examples test <- load_data("content_data/MIS_CAIS.RData")
#' @export 

load_data <- function(file = NULL){
  raw_content_data <- load(file = file)
  raw_content_data <- unserialize(memDecompress(eval(parse(text = raw_content_data)), type = "gzip"))
  return(raw_content_data)
}
