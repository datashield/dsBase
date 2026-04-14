#'
#' @title Returns the levels of a factor vector
#' @description This function is similar to R function \code{levels}.
#' @details The function returns the levels of the input vector or list.
#' @param x a factor vector
#' @return a list with one element: \code{Levels} (the factor levels present
#'   in the vector)
#' @author Alex Westerberg, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
levelsDS <- function(x){

  x.val <- .loadServersideObject(x)
  .checkClass(obj = x.val, obj_name = x, permitted_classes = "factor")

  # Check Permissive Privacy Control Level.
  dsBase::checkPermissivePrivacyControlLevel(c('permissive', 'banana', 'carrot'))

  ##################################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS                          #
  thr <- dsBase::listDisclosureSettingsDS()                        #
  nfilter.levels.density <- as.numeric(thr$nfilter.levels.density) #
  ##################################################################

  # find the levels of the input vector
  out <- levels(x.val)
  input.length     <- length(x.val)
  output.length    <- length(out)

  if((input.length * nfilter.levels.density) < output.length) {
    stop("FAILED: Result length less than nfilter.levels.density of input length.", call. = FALSE)
  }

  out.obj <- list(Levels=out)
  return(out.obj)
}
#AGGREGATE FUNCTION
# levelsDS
