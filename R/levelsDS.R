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
  #MODULE 1: CAPTURE THE nfilter SETTINGS                     #
  thr <- listDisclosureSettingsDS()                           #
  #nfilter.tab <- as.numeric(thr$nfilter.tab)                 #
  #nfilter.glm <- as.numeric(thr$nfilter.glm)                 #
  #nfilter.subset <- as.numeric(thr$nfilter.subset)           #
  #nfilter.string <- as.numeric(thr$nfilter.string)           #
  #nfilter.stringShort <- as.numeric(thr$nfilter.stringShort) #
  #nfilter.kNN <- as.numeric(thr$nfilter.kNN)                 #
  #nfilter.noise <- as.numeric(thr$nfilter.noise)             #
  nfilter.levels <- as.numeric(thr$nfilter.levels)            #
  #############################################################
  
  # find the levels of the input vector
  out <- levels(x)
  input.length     <- length(x)
  output.length    <- length(out)
  studysideMessage <- "VALID ANALYSIS"

  if((input.length * nfilter.levels) < output.length) {
    out <- NA
    studysideMessage <- "FAILED: Result length less than nfilter.levels of input length."
    stop(studysideMessage, call. = FALSE)
  }
  
  out.obj <- list(Levels=out,ValidityMessage=studysideMessage)
  return(out.obj)
}
#AGGREGATE FUNCTION
# levelsDS
