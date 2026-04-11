#' @title Coerces an R object into class logical
#' @description this function is based on the native R function \code{as.logical}
#' @details See help for function \code{as.logical} in native R
#' @param x.name the name of the input object to be coerced to class
#' logical. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' \code{ds.asLogical}
#' @return the object specified by the <newobj> argument (or its default name
#' <x.name>.logic) which is written to the serverside. For further
#' details see help on the clientside function \code{ds.asLogical}
#' @author Amadou Gaye, Paul Burton for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
asLogicalDS <- function (x.name){
  x <- .loadServersideObject(x.name)
  .checkClass(obj = x, obj_name = x.name, permitted_classes = c("numeric", "integer", "character", "matrix"))

  output <- as.logical(x)
  return(output)
}
#ASSIGN FUNCTION
# asLogicalDS
