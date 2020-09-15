#' @title Coerces an R object into class numeric
#' @description this function is based on the native R function {as.numeric}
#' @details See help for function {as.logical} in native R
#' @param x.name the name of the input object to be coerced to class
#' numeric. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' {ds.aslogical}
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.logic) which is written to the serverside. For further
#' details see help on the clientside function {ds.asLogical}
#' @author Amadou Gaye, Paul Burton for DataSHIELD Development Team
#' @export
asLogicalDS <- function (x.name){

if(is.character(x.name)){
	x<-eval(parse(text=x.name), envir = parent.frame())

	}else{
   studysideMessage<-"ERROR: x.name must be specified as a character string"
   return(list(studysideMessage=studysideMessage))
   }

  if(!is.numeric(x)&&!is.integer(x)&&!is.character(x)&&!is.matrix(x)){
  studysideMessage<-"ERROR: for ds.asLogical function, x.name must specify an input object of class numeric, integer, character or matrix"
  return(list(studysideMessage=studysideMessage))
  }

  output <- as.logical(x)

  return(output)
}
#ASSIGN FUNCTION
# asLogicalDS
