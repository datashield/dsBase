#'
#' @title Computes the variance of vector with more than 4 entries
#' @description Calculates the variance.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned. 
#' @param xvect a vector
#' @return a numeric, the variance
#' @author Gaye, A.
#' @export
#'
varDS <- function (xvect) {
  
  # check if the input vector is valid (i.e. meets DataSHIELD privacy criteria)
  check <- isValidDS(xvect)
  
  if(check){
    result <- var(xvect, na.rm=TRUE) 
  }else{
    result <- NA
  }
  
  return(result)
}