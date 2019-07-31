#'
#' @title Returns the length of a vector or list
#' @description This function is similar to R function \code{length}.
#' @details The function returns the length of the input vector or list.
#' @param x a string character, the name of a vector or list
#' @return a numeric, the number of elements of the input vector or list.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#' 
lengthDS.o <- function(x){

 #############################################################
 #MODULE 1: CAPTURE THE nfilter SETTINGS
 #thr <- listDisclosureSettingsDS.o()
 #nfilter.tab<-as.numeric(thr$nfilter.tab)
 #nfilter.glm<-as.numeric(thr$nfilter.glm)
 #nfilter.subset<-as.numeric(thr$nfilter.subset)
 #nfilter.string<-as.numeric(thr$nfilter.string)
 #############################################################

  # find the length of the input vector or list
  out <- length(x)

  # return output length
  return(out)

}
#AGGREGATE FUNCTION
# lengthDS.o
