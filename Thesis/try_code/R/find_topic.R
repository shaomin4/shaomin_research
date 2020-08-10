#' Find Top-N Terms From Topic-term Matrix
#' @param topic_term A topic-term matrix
#' @param top_num The number of top terms.
#' @return Return 2 matrices. \{term\} contains top-n terms of each topic.
#' \{value\} contains the weight of the corresponding top-n terms.
#' @export
#' @examples find_topic(topic_term = topic_term, top_num = 10)

find_topic <- function(topic_term, top_num){
  
  # Get top-n terms and its corresponding weight
  topics <- lapply(seq(ncol(topic_term)), function(i) {
    # Sort by weight in decreasing order and keep only top-n
    term_order <- order(topic_term[, i], decreasing = T)[1:top_num]
    # top-n terms
    term <- rownames(topic_term)[term_order]
    # Weights of top-n terms
    value <- topic_term[term_order, i]
    
    term <- term[value > 0]
    value <- value[value > 0]
    return(list("term" = term, "value" = value))
  })
  
  # Gather the top-n terms of each topic
  term <- Reduce(x = lapply(topics, function(x) x$term), f = cbind)
  
  # Gather the weights of the top-n terms
  value <- Reduce(x = lapply(topics, function(x) x$value), f = cbind)
  
  # Number topics
  colnames(term) <- paste0("topic_", seq(ncol(topic_term)))
  colnames(value) <- paste0("topic_", seq(ncol(topic_term)))
  
  return(list("term" = term, "value" = value))
}
