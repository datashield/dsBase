#'
#' @title Checks if an input is valid
#' @description Internal function
#' @details This function checks if an input variable is valid. A numeric variable
#' is not valid if it has > 0 and < 5 observations and a factor variable is not valid if
#' any of its levels (classes) has a count of between 1 and 4.
#' @param xvect a numeric or factor vector.
#' @return a boolean TRUE if input is valid or stops the process otherwise
#' @author Gaye, A.
#'
.isValidDS <- function(xvect) {
  
  # this filter sets the minimum number of observations that are allowed 
  nfilter <- dsbase:::.setFilterDS()
  
  if(class(xvect) == "numeric") {
    if(length(xvect) > 0 & length(xvect)  < nfilter) {
      stop("The input vector is not valid!\nThe variable has between 1 and 4 observations.\n\n")
    } else {
      return(TRUE)
    }
  }else{
    if(class(xvect) == "factor"){
      tt <- tabulate(xvect)
      xx <- which(tt > 0 & tt < nfilter)
      if(length(xx) > 0) {
        return(FALSE)
        stop("The input vector is not valid!\nOne or more levels have a count of between 1 and 4.\n\n")
      } else {
        return(TRUE)
      }
    }else{
      stop("\n\nThe variable is not a numeric or a factor!\n\n")
    }
  } 
  

}