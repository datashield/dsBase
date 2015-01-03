#' 
#' @title Counts the number of missing values
#' @description this function just counts the number of missing entries 
#' in a vector. 
#' @param xvect a vector
#' @return an integer, the number of missing values
#' @author Gaye, A.
#' @export
#'
numNaDS <- function(xvect){
  
  out <- length(which(is.na(xvect)))
  return (out)
  
}
