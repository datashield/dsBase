#'
#' @title Computes statistical mean of a vectores
#' @description Calculates the mean value.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned.
#' @param xvect a vector
#' @return a numeric, the statistical mean
#' @author Gaye A, Burton PR
#' @export
#'
meanDS <- function(xvect){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  out.mean <- mean(xvect, na.rm=TRUE)
  out.numNa <- length(which(is.na(xvect)))
  out.totN <- length(xvect)
  out.validN <- out.totN-out.numNa
  studysideMessage <- "VALID ANALYSIS"

  if((out.validN != 0) && (out.validN < nfilter.tab)){
    out.mean <- NA
    stop("FAILED: Nvalid less than nfilter.tab", call. = FALSE)
  }

  out.obj <- list(EstimatedMean=out.mean,Nmissing=out.numNa,Nvalid=out.validN,Ntotal=out.totN,ValidityMessage=studysideMessage)
  return(out.obj)

}
#AGGREGATE FUNCTION
# meanDS
