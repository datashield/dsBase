#'
#' @title Returns the dimension of a data frame or matrix
#' @description This function is similar to R function \code{dim}.
#' @details The function returns the dimension of the input dataframe or matrix
#' @param x a string character, the name of a dataframe or matrix
#' @return a list with two elements: \code{dim} (the dimension of the input object)
#'   and \code{class} (the class of the input object, for client-side consistency checking)
#' @author Demetris Avraam, for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
dimDS <- function(x){
  x.val <- .loadServersideObject(x)
  .checkClass(obj = x.val, obj_name = x, permitted_classes = c("data.frame", "matrix"))
  list(dim = dim(x.val), class = class(x.val))
}
#AGGREGATE FUNCTION
# dimDS
