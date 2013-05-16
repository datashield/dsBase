#' Computes the mean of a variable by another variable
#' 
#' @param a a numerical vector
#' @param b a numerical vector
#' @export
#' 

mean.a.by.b <- function(a,b){
  tapply(a,b,mean,na.rm=TRUE)
}
