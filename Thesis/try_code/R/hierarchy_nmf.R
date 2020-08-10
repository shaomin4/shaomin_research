#' Hierarchical Non-negative Matrix Factoriztion (NMF)
#' @description Do NMF Hierarchically
#' @author Shengtai Huang
#' @param dtm A \code{DocumentTermMatrix}.
#' @param k Factorization parameter (The number of topics).
#' @param layerNum The number of layers.
#' @return A list of nmfgpu objects
#' @export
#' @examples result <- hierarchy_nmf(dtm = dtm, k = c(200, 100, 50, 20, 10), layerNum = length(k))

source("R/GPU_envir_vars.R")

hierarchy_nmf <- function(dtm, k = c(200, 100, 50, 20, 10), ...) {
  
  # Store all NMF objects into a list
  nmfList <- list()
  
  # Hierarchy NMF
  for (i in seq(length(k))) {
    print(paste("NMF Layer", i))
    nmfList[[i]] <- nmf(dtm, k[i], ...)

    # We use H matrix in NMF for decomposition in next layer
    dtm <- nmfList[[i]]$H
  }
  
  return(dtm)
}
