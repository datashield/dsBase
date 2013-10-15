#' 
#' @title checks for NAs in a vector
#' @param xvect a numeric or factor vector
#' @return a factor
#' @author Burton, P.; Gaye, A.
#' @export
#' 
nais1.ds <- function(xvect){
  
  indx <- is.na(xvect)*1
  return(factor(indx))
  
}