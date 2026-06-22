#' 
#' @title Calculates the skewness of a numeric variable
#' @description This function calculates the skewness of a numeric variable for each study separately.
#' @details The function calculates the skewness of an input variable x with three different methods. 
#' The method is specified by the argument \code{method} in the client-side \code{ds.skewness} function. 
#' @param x a string character, the name of a numeric variable.
#' @param method an integer between 1 and 3 selecting one of the algorithms for computing skewness
#' detailed in the headers of the client-side \code{ds.skewness} function.
#' @return a list including the skewness of the input numeric variable, the number of valid observations and
#' \code{class}, the class of the input object for client-side consistency checking.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#' 
skewnessDS1 <- function(x, method){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #############################################################
  
  x.val <- .loadServersideObject(x)
  .checkClass(obj = x.val, obj_name = x, permitted_classes = c("numeric", "integer"))
  x <- x.val[stats::complete.cases(x.val)]
  
  if(length(x) < nfilter.tab){
    skewness.out <- NA
    studysideMessage <- "FAILED: Nvalid less than nfilter.tab"
    stop(studysideMessage, call. = FALSE)
  }else{
    
    g1 <- ( sum((x - mean(x))^3) / length(x) ) / ( sum((x - mean(x))^2) / length(x) )^(3/2)
    
    if(method==1){
      skewness.out <- g1
    }
    if(method==2){
      skewness.out <- g1 * sqrt(length(x)*(length(x)-1))/(length(x)-2)
    }
    if(method==3){
      skewness.out <- g1 * ((length(x)-1)/(length(x)))^(3/2)
    }
  }

  out.obj <- list(Skewness=skewness.out, Nvalid=length(x), class=class(x.val))
  return(out.obj)
  
}
#AGGREGATE FUNCTION
# skewnessDS1

