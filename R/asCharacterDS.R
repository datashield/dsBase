#'
#' @title Coerces an R object into class character
#' @description this function is based on the native R function \code{as.character}
#' @details See help for function \code{as.character} in native R
#' @param x.name the name of the input object to be coerced to class
#' character. Must be specified in inverted commas. But this argument is
#' usually specified directly by \code{x.name} argument of the clientside function
#' \code{ds.asCharacter}
#' @return the object specified by the \code{newobj} argument (or its default name
#' "ascharacter.newobj") which is written to the serverside. For further
#' details see help on the clientside function \code{ds.asCharacter}
#' @author Amadou Gaye, Paul Burton, Demetris Avraam for DataSHIELD Development Team
#' @export
#'
asCharacterDS <- function(x.name) {
  x <- eval(parse(text = x.name), envir = parent.frame())

  output <- as.character(x)
  return(output)
}
# ASSIGN FUNCTION
# asCharacterDS
