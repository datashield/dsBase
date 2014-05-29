#'
#' @title Combines object into a vector or list
#' @description This function is similar to the R base function 'c'.
#' @details Unlike the R base function 'c' on vector or list of certain 
#' length are allowed as output
#' @param objs a list which contains the names of the objects to combine.
#' @return a vector or list
#' @author Gaye, A.
#' @export
#' 
#'
cDS <- function (objs) {
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  mtx <-  eval(parse(text=objs[[1]]))
  for(i in 2:length(objs)){
    oo <- eval(parse(text=objs[[i]]))
    mtx <- c(mtx, oo)
  }

  # check if the output is valid and output accordingly
  if(length(mtx) < nfilter){
    if(length(mtx == 0)){
      mtx <- NA
    }else{
      mtx <- rep(NA, length(mtx))
    }
  }
  
  return(mtx)
}
