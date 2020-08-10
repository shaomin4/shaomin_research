#' Tokenizer for Dictionary
#' @description \code{ngram_tokenizer} is a closure function. Users can give a variable
#' \code{n} for length of wanted words.
#' @param n Length of words. The default is 2.
#' @return A vector of words, each element length = n
#' @importFrom NLP ngrams
#' @examples tokenizer(x) <- ngram_tokenizer(3)
#' x <- "aa bb cc"
#' token <- tokenizer(x)

ngram_tokenizer <- function(n = 2){
  return(function(x){
    unlist(lapply(NLP::ngrams(x = words(x), n = n), paste, collapse = " "), use.names = F)
  })
}
