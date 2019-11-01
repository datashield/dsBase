#' 
#' @title Coerces an R object into class numeric
#' @description This function is based on the native R function \type{as.numeric}.
#' @details See help for function \type{as.numeric} in native R, and details section
#' in the help file of the clientside function type{ds.asNumeric}.
#' @param x.name the name of the input object to be coerced to class
#' numeric. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' \type{ds.asNumeric}.
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.num) which is written to the serverside. For further
#' details see help on the clientside function \type{ds.asNumeric}.
#' @author Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
asNumericDS <- function(x.name){

  if(is.character(x.name)){
    x <- eval(parse(text=x.name))
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    return(list(studysideMessage=studysideMessage))
  }

  output <- as.numeric(as.character(x))

  return(output)

}
# ASSIGN FUNCTION
# asNumericDS
