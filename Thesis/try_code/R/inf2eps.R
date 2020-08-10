#' Replace the Inf/-Inf with positive/negative eps (smallest positive floating-point number ) 
#' of a object.
#'  
#' @description Replace the Inf/-Inf with positive/negative eps (smallest positive floating-point number ) 
#' of a object. Similar to the “eps” constant in MATLAB (“Floating-point relative accuracy”)
#' @author Yihuang Kang
#' @param x Any object with Inf/-Inf
#' @return Return the object with replaced positive or negaive eps

inf2eps = function(x)
{
  tryCatch({
    x[x == Inf]  =  .Machine$double.eps;
    x[x == -Inf] = -.Machine$double.eps;
  }, error=function(cond) {
    message('Error in ykang::inf2eps():');
    message(cond); }
  );
  return(x);
  
}#end of function
