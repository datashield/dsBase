#' 
#' @title Calculates the kurtosis of a numeric variable
#' @description This function calculates the kurtosis of a numeric variable for each study separately.
#' @details The function calculates the kurtosis of an input variable x with three different methods. 
#' The method is specified by the argument \code{method} in the client-side \code{ds.kurtosis} function. 
#' @param x a string character, the name of a numeric variable.
#' @param method an integer between 1 and 3 selecting one of the algorithms for computing kurtosis
#' detailed in the headers of the client-side \code{ds.kurtosis} function.
#' @return a list including the kurtosis of the input numeric variable, the number of valid observations and
#' the study-side validity message.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
kurtosisDS1 <- function (x, method){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #############################################################
  
  x <- eval(parse(text=x), envir = parent.frame())
  x <- x[stats::complete.cases(x)]
  
  if(length(x) < nfilter.tab){
    kurtosis.out <- NA
    studysideMessage <- "FAILED: Nvalid less than nfilter.tab"
  }else{
    
    g2 <- length(x) * sum((x - mean(x))^4)/(sum((x - mean(x))^2)^2) - 3
    
    if(method==1){
      kurtosis.out <- g2
      studysideMessage <- "VALID ANALYSIS"
    }
    if(method==2){
      kurtosis.out <- ((length(x) + 1) * g2 + 6) * (length(x) - 1)/((length(x) - 2) * (length(x) - 3))
      studysideMessage <- "VALID ANALYSIS"
    }
    if(method==3){
      kurtosis.out <- (g2 + 3) * (1 - 1/length(x))^2 - 3
      studysideMessage <- "VALID ANALYSIS"
    }
  }
  
  out.obj <- list(Kurtosis=kurtosis.out, Nvalid=length(x), ValidityMessage=studysideMessage)
  return(out.obj)
  
}
#AGGREGATE FUNCTION
# kurtosisDS1
