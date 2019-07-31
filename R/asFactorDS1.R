#' 
#' @title Determines the levels of the input variable in each single study
#' @description This function is an aggregate DataSHIELD function that returns the 
#' levels of the input variable from each single study to the client-side function.
#' @details The function encodes the input vector as factor and returns its levels in
#' ascending order if the levels are numerical or in alphabetical order if the levels
#' are of type character.
#' @param input.var.name the name of the variable that is to be converted to a factor.
#' @return the levels of the input variable.
#' @export
#'
asFactorDS1.o <- function(input.var.name=NULL){

  input.var <- eval(parse(text=input.var.name))
  factor.levels.present.in.source <- levels(factor(input.var)) 

  return(factor.levels.present.in.source)

}
#AGGREGATE FUNCTION
# asFactorDS1.o





