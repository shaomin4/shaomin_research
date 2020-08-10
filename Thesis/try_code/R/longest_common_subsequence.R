#' Find the longest common subsequence (LCS) of two sequences
#'  
#' @description To find the longest common subsequence(LCS) of 2 sequences.
#' The sequences don't have to be in the same size/length. If more than 1 LCS 
#' with the same length are found, the first occurrence of LCS will return. 
#' 
#' @author Yihuang Kang
#' @param seq1 Sequence 1 as a character vector
#' @param seq2 Sequence 2 as a character vector
#' @return Return a character LCS
#' @references http://en.wikipedia.org/wiki/Longest_common_subsequence_problem
#' http://rosettacode.org/wiki/Longest_common_subsequence#C.23
LCS = function(seq1, seq2){
  # Check if two sequence are character vector
  stopifnot(is.vector(seq1), is.vector(seq2), is.character(seq1), is.character(seq2))
  
  lcs = NULL
  # Convert 2 sequence into character vectors
  s1 = unlist(strsplit(seq1, split = ""))
  s2 = unlist(strsplit(seq2, split = ""))
  
  lenS1 = length(s1)
  lenS2 = length(s2)
  
  # LCS Length matrix
  #lenMatrix = Matrix::Matrix(data=0,nrow= lenS1 + 1 , ncol= lenS2 + 1, sparse=F);
  lenMatrix = matrix(data = 0,nrow = lenS1 + 1 , ncol = lenS2 + 1)
  #print(object.size(lenMatrix));
  
  for(i in 1:lenS1) {
    for(j in 1:lenS2) {
      lenMatrix[i + 1, j + 1] = 
        ifelse(identical(s1[i], s2[j]), lenMatrix[i, j] + 1, max(lenMatrix[i + 1, j], lenMatrix[i, j + 1]))
    }
  }

  # x & y as temporary indices
  x = lenS1
  y = lenS2
  
  while(x != 0 & y != 0){
    if(lenMatrix[x + 1, y + 1] == lenMatrix[x, y + 1]) {
      x = x - 1
    } else if(lenMatrix[x + 1, y + 1] == lenMatrix[x + 1,y]) {
      y = y - 1
    } else {
      lcs = rbind(lcs, s1[x])
      x = x - 1
      y = y - 1
    }
  }
  # Reverse to get the correct order of LCS
  lcs = rev(lcs)
  
  return(paste0(lcs, collapse = ""))
  # return(max(nchar))
  
}#end of function
