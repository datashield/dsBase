#'
#' @title Returns the dimension of a data frame or matrix
#' @description This function is similar to R function \code{dim}.
#' @details The function returns the dimension of the input dataframe or matrix
#' @param x a string character, the name of a dataframe or matrix
#' @return the dimension of the input object
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#' 
dimDS.o <- function(x){

  #############################################################
  #MODULE 1: CAPTURE THE nfilter SETTINGS
  #thr <- listDisclosureSettingsDS.o()
  #nfilter.tab<-as.numeric(thr$nfilter.tab)
  #nfilter.glm<-as.numeric(thr$nfilter.glm)
  #nfilter.subset<-as.numeric(thr$nfilter.subset)
  #nfilter.string<-as.numeric(thr$nfilter.string)
  #############################################################

  # find the dim of the input dataframe or matrix
  out <- dim(x)

  # return the dimension
  return(out)

}
#AGGREGATE FUNCTION
# dimDS.o
