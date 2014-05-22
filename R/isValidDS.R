#'
#' @title Checks if an input is valid
#' @description Internal function
#' @details This function checks if an input variable is valid. A numeric variable
#' is not valid if it has > 0 and < 5 observations and a factor variable is not valid if
#' any of its levels (classes) has a count of between 1 and 4.
#' @param obj, a vector (numeric, integer, factor, character), data.frame or matrix
#' @return a boolean TRUE if input is valid or stops the process otherwise
#' @author Gaye, A.
#'
.isValidDS <- function(obj) {
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- dsbase:::.setFilterDS()
  
  if(class(obj) == "numeric" | class(obj) == "integer" | class(obj) == "character") {
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