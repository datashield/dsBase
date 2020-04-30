#'
#' @title Returns the levels of a factor vector
#' @description This function is similar to R function \code{levels}.
#' @details The function returns the levels of the input vector or list.
#' @param x a factor vector
#' @return a list, the factor levels present in the vector
#' @author Alex Westerberg, for DataSHIELD Development Team
#' @export
#'
levelsDS <- function(x){
  
  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab<-as.numeric(thr$nfilter.tab)
  #nfilter.glm<-as.numeric(thr$nfilter.glm)
  nfilter.subset<-as.numeric(thr$nfilter.subset)
  #nfilter.string<-as.numeric(thr$nfilter.string)
  #############################################################
  
  # find the levels of the input vector
  out <- levels(x)
  # out.totN <- length(x)
  
  # return output levels
  return(out)
  
}
#AGGREGATE FUNCTION
# levelsDS
