#' Transfer Content into A Corpus
#' @description Get contents from raw contents data directly, conbining into a vector and
#' output as a document-term matrix.
#' @author Shengtai Huang
#' @param contents A list which is the content of each data.
#' @return An object of class DocumentTermMatrix containing a sparse document-term
#' matrix.
#' @import tm
#' @examples contents <- content2Corpus(data[[1]]$content)
#' @export

library(tm)

content2Corpus <- function(contents = NULL){
  
  # Only id and content are needed
  contentId <- contents$id
  contents <- contents$content
  
  # Transfer contents from list to vector
  contents <- paste0(contents, collapse = " ")
  
  # Some replacement in contents
  contents <- gsub(pattern = "[[:cntrl:]]+", replacement = " ", x = contents)
  contents <- gsub(pattern = "[[:punct:]]+", replacement = "", x = contents)
  contents <- gsub(pattern = "ﬁ", replacement = "fi", x = contents)
  contents <- gsub(pattern = "ﬂ", replacement = "fl", x = contents)
  
  # Transfer contents from vector to VCorpus
  contents <- VCorpus(VectorSource(contents))
  
  # Set id (for rownames of dtm)
  meta(contents[[1]], tag = "id", type = "corpus") <- contentId
  contents <- tm_map(contents, stripWhitespace)
  contents <- tm_map(contents, content_transformer(tolower))
  word_replace <- mapping_table[mapping_table$keep == 1,]
  for(i in seq(nrow(word_replace))){
    if(grepl(pattern = paste0("^", word_replace[i, 1], "$"), x = contents)[1]){
      contents <- gsub(pattern = paste0("^", word_replace[i, 1], "$"),
                       replacement = paste0("^", word_replace[i, 2], "$"), x = contents)
    }
  }
  
  return(contents)
}