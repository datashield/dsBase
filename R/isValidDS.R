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

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################
  
  if(any(c("character", "integer", "logical", "numeric") %in% class(obj))) {
    if(length(obj) > 0 & length(obj)  < nfilter.tab) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }else{
    if("factor" %in% class(obj)){
      tt <- tabulate(obj)
      xx <- which(tt > 0 & tt < nfilter.tab)
      if(length(xx) > 0) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }else{
      if(any(c("data.frame", "matrix") %in% class(obj))) {
        if(dim(obj)[1] > 0 & dim(obj)[1] < nfilter.tab){
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
