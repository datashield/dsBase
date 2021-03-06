#' 
#' @title Generates quantiles and mean information without maximum and minimum
#' @description the probabilities 5%, 10%, 25%, 50%, 75%, 90%, 95% and the mean
#' are used to compute the corresponding quantiles.
#' @param xvect a numerical vector
#' @return a numeric vector that represents the sample quantiles 
#' @export
#' @author Burton, P.; Gaye, A.
#' 
quantileMeanDS <- function (xvect) {
  
  # check if the input vector is valid (i.e. meets DataSHIELD criteria)
  check <- isValidDS(xvect)
  
  if(check){
    # if the input vector is valid 
    qq <- stats::quantile(xvect,c(0.05,0.1,0.25,0.5,0.75,0.9,0.95), na.rm=TRUE)
    mm <- mean(xvect,na.rm=TRUE)
    quantile.obj <- c(qq, mm)
    names(quantile.obj) <- c("5%","10%","25%","50%","75%","90%","95%","Mean")    
  }else{
    quantile.obj <- NA
  }
  
  return(quantile.obj)
}
