#'
#' @title Computes the variance of vector
#' @description Calculates the variance.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned.
#' @param x a character string, the name of a numeric or integer vector
#' @return a list, with the sum of the input variable, the sum of squares of the input variable,
#' the number of missing values, the number of valid values, the number of total length of the
#' variable, and \code{class}, the class of the input object for client-side
#' consistency checking
#' @author Amadou Gaye, Demetris Avraam, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
varDS <- function(x){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  xvect <- .loadServersideObject(x)
  .checkClass(obj = xvect, obj_name = x, permitted_classes = c("numeric", "integer"))

  out.sum <- sum(xvect, na.rm=TRUE)
  out.sumSquares <- sum(xvect^2, na.rm=TRUE)
  out.numNa <- length(which(is.na(xvect)))
  out.totN <- length(xvect)
  out.validN <- out.totN-out.numNa
  if((out.validN != 0) && (out.validN < nfilter.tab)){
    stop("FAILED: Nvalid less than nfilter.tab", call. = FALSE)
  }

  out.obj <- list(Sum=out.sum,SumOfSquares=out.sumSquares,Nmissing=out.numNa,Nvalid=out.validN,Ntotal=out.totN,class=class(xvect))
  return(out.obj)

}
#AGGREGATE FUNCTION
# varDS
