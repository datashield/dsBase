#'
#' @title Concatenates objects into a vector or list
#' @description This function is similar to the R base function 'c'.
#' @details Unlike the R base function 'c' on vector or list of certain 
#' length are allowed as output
#' @param objs a list which contains the the objects to concatenate.
#' @return a vector or list
#' @author Gaye, A.
#' @export
#' 
cDS <- function (objs) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  x <-  unlist(objs)

  # check if the output is valid and output accordingly
  if(length(x) < nfilter){
    if(length(x == 0)){
      x <- c()
    }else{
      x <- rep(NA, length(x))
    }
  }

  return(x)
}
