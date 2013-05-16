#' Counts number of observations by category
#'
#' @param a a numerical vector
#' @param b a numerical vector
#' @export
#' 

N.a.by.b <- function(a,b){
  a.cons<-a-a+1 #vector with 1 when a present, NA otherwise
  tapply(a.cons,b,sum,na.rm=TRUE)
}

