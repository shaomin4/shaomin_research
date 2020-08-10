source("R/gjsdivergence.R")

topic_dist <- function(t_H_hat_1, t_H_hat_2) {
  dist_matrix <- apply(t_H_hat_1, 2, function(h1_col) {
    jsd_vec <- apply(t_H_hat_2, 2, function(h2_col) {
      return(ykang::gjsdivergence(pVecs = cbind(h1_col, h2_col)))
    })
    return(jsd_vec)
  })
  
  print(dist_matrix)
  
  return(list("dist_matrix" = dist_matrix,
              "similar_topic" = apply(dist_matrix, 2, which.min)))
}
