#' 
#' @title Coerces an R object into class integer
#' @description This function is based on the native R function \code{as.integer}.
#' @details See help for function \code{as.integer} in native R, and details section
#' in the help file of the clientside function \code{ds.asInteger}.
#' @param x.name the name of the input object to be coerced to class
#' integer. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' \code{ds.asInteger}.
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.int) which is written to the serverside. For further
#' details see help on the clientside function \code{ds.asInteger}.
#' @author Amadou Gaye, Paul Burton, Demetris Avraam, for DataSHIELD Development Team
#' @export
#'
asIntegerDS <- function(x.name){

  if(is.character(x.name)){
    x <- eval(parse(text=x.name), envir = parent.frame())
  }else{
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    stop(studysideMessage, call. = FALSE)
  }

  output <- as.integer(as.character(x))

  return(output)

}
# ASSIGN FUNCTION
# asIntegerDS
