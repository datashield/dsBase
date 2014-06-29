#' 
#' @title returns the minimum and maximum of a numeric vector
#' @description this function is similar to R function \code{range} but instead to not return 
#' the real minimum and maximum, the computed values are multiplied by a very small random number. 
#' @param xvect a numerical 
#' @return  a numeric vector which contains the minimum and the maximum
#' @author Gaye, A.
#' @export
#'
rangeDS <- function(xvect){
  
  # print an error message if the input vector is not a numeric
  if(!(is.numeric(xvect))){
    stop("The input vector is not a numeric!")
  }else{
    rr <- c(min(xvect, na.rm=TRUE), max(xvect, na.rm=TRUE))
    random1 <- runif(1, 0.95, 1)
    random2 <- runif(1, 1, 1.05)
    output <- c(rr[1]*random1, rr[2]*random2)
  }
  return (output)
}