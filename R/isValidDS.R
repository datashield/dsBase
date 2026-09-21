#'
#' @title Checks if an input is valid
#' @description Tells if an object on the server side is valid.
#' @details This function checks if an object is valid.
#' @param x a character string, the name of a vector (numeric, integer, factor, character,
#' logical), data.frame or matrix
#' @return a list with \code{valid}, a boolean that is TRUE if the input is valid or FALSE
#' if not, and \code{class}, the class of the input object for client-side consistency
#' checking
#' @author Gaye, A.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#'@export
#'
isValidDS <- function(x) {

  obj <- .loadServersideObject(x)
  .checkClass(
    obj = obj,
    obj_name = x,
    permitted_classes = c("character", "factor", "integer", "logical", "numeric", "data.frame", "matrix")
  )

  list(valid = .checkDisclosureSize(obj), class = class(obj))
}

#' Check a Server-Side Object Meets the Disclosure Size Threshold
#'
#' @param obj The object to check: a vector, data.frame or matrix.
#' @return A boolean, TRUE if the object meets the threshold or FALSE if not.
#' @noRd
.checkDisclosureSize <- function(obj) {
  
  # this filter sets the minimum number of observations that are allowed 

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
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
