#' Provides quantiles and mean information without maximum and minimum
#' 
#' @param a a numerical vector
#' @export
#' 

quantile.mean.ds <- function (a) {
  quantile.obj<-c(quantile(a,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95),na.rm=TRUE),mean(a,na.rm=TRUE))
  names(quantile.obj)<-c("5%","10","25%","50%","75%","90%","95%","Mean")
  quantile.obj
}