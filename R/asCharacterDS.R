#' @title Coerces an R object into class character
#' @description this function is based on the native R function {as.character}
#' @details See help for function {as.character} in native R
#' @param x.name the name of the input object to be coerced to class
#' integer. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' {ds.asCharacter.o}
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.char) which is written to the serverside. For further
#' details see help on the clientside function {ds.asCharacter.o}
#' @author Amadou Gaye, Paul Burton for DataSHIELD Development Team
#' @export
asCharacterDS.o <- function (x.name){

if(is.character(x.name)){
	x<-eval(parse(text=x.name))

	}else{
   studysideMessage<-"ERROR: x.name must be specified as a character string"
   return(list(studysideMessage=studysideMessage))
   } 

  output <- as.character(x) 

  return(output)
}
#ASSIGN FUNCTION
# asCharacterDS.o
