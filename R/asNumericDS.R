#' 
#' @title Coerces an R object into class numeric
#' @description This function is based on the native R function \code{as.numeric}.
#' @details See help for function \code{as.numeric} in native R, and details section
#' in the help file of the clientside function \code{ds.asNumeric}.
#' @param x.name the name of the input object to be coerced to class
#' numeric. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' \code{ds.asNumeric}.
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.num) which is written to the serverside. For further
#' details see help on the clientside function \code{ds.asNumeric}.
#' @author Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
asNumericDS <- function(x.name){

  if(is.character(x.name)){
    x <- eval(parse(text=x.name), envir = parent.frame())
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    stop(studysideMessage, call. = FALSE)
  }
  
  # Check that it doesn't match any non-number
  numbers_only <- function(vec) !grepl("\\D", vec)
  
  logical <- numbers_only(x)
  
  if((is.factor(x) & any(logical==FALSE)==FALSE) | (is.character(x) & any(logical==FALSE)==FALSE)){
    output <- as.numeric(as.character(x))
  }else if((is.factor(x) & any(logical==FALSE)==TRUE) | (is.character(x) & any(logical==FALSE)==TRUE)){
    output <- as.numeric(as.factor(x))
  }else{
    output <- as.numeric(x)
  }
  
  return(output)

}
# ASSIGN FUNCTION
# asNumericDS
