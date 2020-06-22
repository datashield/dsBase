#' 
#' @title Calculates the skewness of a numeric variable
#' @description This function calculates summary statistics that are returned to the client-side and 
#' used for the estimation of the combined skewness of a numeric variable across all studies.
#' @details The function calculates the sum of squares, the sum of cubes and the number of valid observations
#' of the input variable x. 
#' @param x a string character, the name of a numeric variable.
#' @param global.mean a numeric, the combined mean of the input variable across all studies.
#' @return a list including the sum of cubes of variable x, the sum of squares of variable x, the number of
#' valid observations and a validity message.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @export
#' 
skewnessDS2 <- function(x, global.mean){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #############################################################
  
  x <- eval(parse(text=x))
  x <- x[stats::complete.cases(x)]
  
  if(length(x) < nfilter.tab){
    sum_cubes.out <- NA
    sum_squares.out <- NA
    studysideMessage <- "FAILED: Nvalid less than nfilter.tab"
  }else{
    sum_cubes.out <- sum((x - global.mean)^3)
    sum_squares.out <- sum((x - global.mean)^2)
    studysideMessage <- "VALID ANALYSIS"
  }
  
  out.obj <- list(Sum.cubes=sum_cubes.out, Sum.squares=sum_squares.out, Nvalid=length(x), ValidityMessage=studysideMessage)
  return(out.obj)
  
}
#AGGREGATE FUNCTION
# skewnessDS2

