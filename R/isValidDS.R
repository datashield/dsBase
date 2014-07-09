#'
#' @title Checks if an input is valid
#' @description Tells if an object on the server side is valid.
#' @details This function checks if an object is valid.
#' @param obj, a vector (numeric, integer, factor, character), data.frame or matrix
#' @return a boolean, TRUE if input is valid or FALSE if not.
#' @author Gaye, A.
#'@export
#'
isValidDS <- function(obj) {
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- setFilterDS()
  
  if(class(obj) == "character" | class(obj) == "integer" | class(obj) == "logical" | class(obj) == "numeric") {
    if(length(obj) > 0 & length(obj)  < nfilter) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }else{
    if(class(obj) == "factor"){
      tt <- tabulate(obj)
      xx <- which(tt > 0 & tt < nfilter)
      if(length(xx) > 0) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }else{
      if(class(obj) == "data.frame" | class(obj) == "matrix"){
        if(dim(obj)[1] < nfilter){
          return(FALSE)
        }else{
          return(TRUE)
        }
      }else{
        return(FALSE)
      }
    }
  } 

}