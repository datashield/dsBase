#' @title asListDS a serverside assign function called by ds.asList
#' @description Coerces an R object into a list
#' @details This assign function is based on the native R function \code{as.list}
#' and so additional information can be found in the help for \code{as.list}
#' @param x.name the name of the input object to be coerced to class
#' data.matrix. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' \code{ds.asList}
#' @return the input object coerced to a list, which \code{ds.asList} writes to the
#' serverside. The way that \code{as.list} coerces objects to list depends on the class
#' of the object
#' @author Amadou Gaye, Paul Burton for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
asListDS <- function (x.name){
  x <- .loadServersideObject(x.name)

  as.list(x)
}
# ASSIGN FUNCTION
# asListDS
