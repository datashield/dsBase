#' Computes the variance of a variable by another variable
#' 
#' @param a a numerical vector
#' @export
#' 

var.a.by.b <- function(a,b){
  tapply(a,b,var,na.rm=TRUE)
}
