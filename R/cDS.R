#'
#' @title Concatenates objects into a vector or list
#' @description This function is similar to the R base function 'c'.
#' @details Unlike the R base function 'c' on vector or list of certain 
#' length are allowed as output
#' @param x.names a character vector of object names to concatenate.
#' @return a vector or list
#' @author Gaye, A.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
cDS <- function (x.names) {

  # Check Permissive Privacy Control Level.
  dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'avocado'))

  # this filter sets the minimum number of observations that are allowed

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #############################################################

  objs <- list()
  for (i in seq_along(x.names)) {
    objs[[i]] <- .loadServersideObject(x.names[i])
  }
  x <- unlist(objs)

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
