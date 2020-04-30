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
  input.length <- length(x)
  output.length <- length(out)
  studysideMessage <- "VALID ANALYSIS"
  
  if(input.length < nfilter.tab){
    out <- NA
    studysideMessage <- "FAILED: Input less than nfilter.tab"
  }
  if(output.length < nfilter.subset){
    out <- NA
    studysideMessage <- "FAILED: Result less than nfilter.subset"
  }
  
  out.obj <- list(Levels=out,ValidityMessage=studysideMessage)
  return(out.obj)
}
#AGGREGATE FUNCTION
# levelsDS
