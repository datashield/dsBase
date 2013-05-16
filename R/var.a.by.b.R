#' Computes the variance of a variable by another variable
#' 
#' @param a a numerical vector
#' @export
#' 

var.a.by.b <- function(a,b){
  base::tapply(a,b,stats::var,na.rm=TRUE)
}
