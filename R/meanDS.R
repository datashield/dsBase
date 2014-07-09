#'
#' @title Computes statistical mean of vector with more than 4 entries
#' @description Calculates the mean value.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned. 
#' @param xvect a vector
#' @return a numeric, the statistical mean
#' @author Gaye, A.
#' @export
#'
meanDS <- function (xvect) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  if(length(xvect) < nfilter){
    result <- NA
  }else{
    result <- mean(xvect, na.rm=TRUE) 
  }
  return(result)
}
