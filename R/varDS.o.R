#'
#' @title Computes the variance of vector
#' @description Calculates the variance.
#' @details if the length of input vector is less than the set filter
#' a missing value is returned. 
#' @param xvect a vector
#' @return a list, with the sum of the input variable, the sum of squares of the input variable,
#' the number of missing values, the number of valid values, the number of total lenght of the
#' variable, and a study message indicating whether the number of valid is less than the
#' disclosure threshold
#' @author Amadou Gaye, Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
varDS.o <- function(xvect){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS.o()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #nfilter.glm <- as.numeric(thr$nfilter.glm)
  #nfilter.subset <- as.numeric(thr$nfilter.subset)
  #nfilter.string <- as.numeric(thr$nfilter.string)
  #############################################################

  out.sum <- sum(xvect, na.rm=TRUE)
  out.sumSquares <- sum(xvect^2, na.rm=TRUE)
  out.numNa <- length(which(is.na(xvect)))
  out.totN <- length(xvect)
  out.validN <- out.totN-out.numNa
  studysideMessage <- "VALID ANALYSIS" 
  
  if(out.validN < nfilter.tab){
    out.sum <- NA
    out.sumSquares <- NA
    studysideMessage <- "FAILED: Nvalid less than nfilter.tab"
  }   
  
  out.obj <- list(Sum=out.sum,SumOfSquares=out.sumSquares,Nmissing=out.numNa,Nvalid=out.validN,Ntotal=out.totN,ValidityMessage=studysideMessage)  
  return(out.obj)	
  
}
#AGGREGATE FUNCTION
# varDS.o  
