#' 
#' @title Recodes the levels of a categorical variables
#' @description The functions uses the input factor and generates a new factor 
#' with new levels.
#' @param x a factor vector
#' @param classes a character vector the levels of the  newt factor vector
#' @return a factor vector with the new levels
#' @author Gaye, A.
#' @export
#'
recodeLevelsDS <- function (x=NULL, classes=NULL){
  
  # check if the input vector is valid (i.e. respect DataSHIELD conditions)
  check <- isValid.ds(x)
  
  if(check){
    # generate the new variable with the specified levels
    levels(x) <- classes
    return(x)
  }else{
    return(NULL)
  }
}
