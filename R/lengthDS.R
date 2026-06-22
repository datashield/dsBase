#'
#' @title Returns the length of a vector or list
#' @description This function is similar to R function \code{length}.
#' @details The function returns the length of the input vector or list.
#' @param x a string character, the name of a vector or list
#' @return a list with two elements: \code{length} (the number of elements of the input
#'   vector or list) and \code{class} (the class of the input object, for client-side
#'   consistency checking)
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
lengthDS <- function(x){
  x.val <- .loadServersideObject(x)
  .checkClass(obj = x.val, obj_name = x, permitted_classes = c("character", "factor", "integer", "logical", "numeric", "list", "data.frame", "array", "matrix"))
  list(length = length(x.val), class = class(x.val))
}
#AGGREGATE FUNCTION
# lengthDS
