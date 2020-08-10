#' Compute Tf-idf
#' @description When \code{DocumentTermMatrix} is generated, elements are term frequency.
#' However, some words are common but not important; therefore, tf-idf are used. Tf stands for
#' term frequency and idf represents term frequency in each document. The hypothesis is that a
#' word is important if it has high idf (often reffered in a document) and low tf (may not be
#' a common word), but this measurement works well practically has no strong theory support.
#' @author Yihuang Kang
#' @param dtm A Document--term matrix in matrix type
#' @return A matrix with each element is a tf-idf value.
#' @export
#' @references https://en.wikipedia.org/wiki/Tf%E2%80%93idf
#' @examples dtm <- tf_idf(dtm)
tf_idf <- function(dtm){
  # Term frequencies / total term frequencies in a document
  tf <- dtm / rowSums(dtm)
  # Number of documents that contains the term
  idf <- log((nrow(dtm) + 1) / (apply(dtm, MARGIN = 2, function(termCol) sum(termCol > 0)) + 1 ))
  tfidf <- tf * idf[col(tf)]
  # If we cannot find any term/keyword of the dictionary in the corpus
  tfidf[is.nan(tfidf)] <- 0
  
  return(tfidf)
}