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
  
  # Check Permissive Privacy Control Level.
  dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'avocado'))
  
  # this filter sets the minimum number of observations that are allowed 

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################
  
  x <-  unlist(objs)

  # check if the output is valid and output accordingly
  if(length(x) < nfilter.tab){
    if(length(x) == 0){
      x <- c()
    }else{
      x <- rep(NA, length(x))
    }
  }

  return(x)
}
