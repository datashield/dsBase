#' 
#' @title Calculates the kurtosis of a numeric variable
#' @description This function calculates summary statistics that are returned to the client-side and 
#' used for the estimation of the combined kurtosis of a numeric variable across all studies.
#' @details The function calculates the sum of squared differences between the values of x and the global
#' mean of x across all studies, the sum of quatric differences between the values of x and the global mean
#' of x across all studies and the number of valid observations of the input variable x. 
#' @param x a string character, the name of a numeric variable.
#' @param global.mean a numeric, the combined mean of the input variable across all studies.
#' @return a list including the sum of quartic differences between the values of x and the global mean of x across
#' all studies, the sum of squared differences between the values of x and the global mean of x across all studies,
#' the number of valid observations (i.e. the length of x after excluding missing values), and \code{class},
#' the class of the input object for client-side consistency checking.
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#' 
kurtosisDS2 <- function(x, global.mean){

  #############################################################
  # MODULE 1: CAPTURE THE nfilter SETTINGS
  thr <- dsBase::listDisclosureSettingsDS()
  nfilter.tab <- as.numeric(thr$nfilter.tab)
  #############################################################
  
  x.val <- .loadServersideObject(x)
  .checkClass(obj = x.val, obj_name = x, permitted_classes = c("numeric", "integer"))
  x <- x.val[stats::complete.cases(x.val)]
  
  if(length(x) < nfilter.tab){
    stop("FAILED: Nvalid less than nfilter.tab", call. = FALSE)
  }

  sum_quartics.out <- sum((x - global.mean)^4)
  sum_squares.out <- sum((x - global.mean)^2)

  out.obj <- list(Sum.quartics=sum_quartics.out, Sum.squares=sum_squares.out, Nvalid=length(x), class=class(x.val))
  return(out.obj)
  
}
#AGGREGATE FUNCTION
# kurtosisDS2
