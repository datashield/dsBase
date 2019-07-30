#' @title Coerces an R object into a matrix
#' @description this function is based on the native R function {as.matrix}
#' @details See help for function {as.matrix} in native R
#' @param x.name the name of the input object to be coerced to class
#' matrix. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' {ds.asMatrix.o}
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.mat) which is written to the serverside. For further
#' details see help on the clientside function {ds.asMatrix.o} 
#' @author Amadou Gaye, Paul Burton for DataSHIELD Development Team
#' @export
asMatrixDS.o <- function (x.name){

if(is.character(x.name)){
	x<-eval(parse(text=x.name))

	}else{
   studysideMessage<-"ERROR: x.name must be specified as a character string"
   return(list(studysideMessage=studysideMessage))
   } 

  output <- as.matrix(x) 

  return(output)
}
#ASSIGN FUNCTION
# asMatrixDS.o
