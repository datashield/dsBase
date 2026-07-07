#'
#' @title Checks if a vector is empty
#' @description this function is similar to R function \code{is.na} but instead of a vector
#' of booleans it returns just one boolean to tell if all the element are missing values.
#' @param x a character string, the name of a server-side vector
#' @return a list with two elements: \code{is.na} (TRUE if the vector contains
#'   only NAs, FALSE otherwise) and \code{class} (the class of the input object,
#'   for client-side consistency checking)
#' @author Gaye, A.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
isNaDS <- function(x){
  xvect <- .loadServersideObject(x)
  .checkClass(obj = xvect, obj_name = x, permitted_classes = c("character", "factor", "integer", "logical", "numeric", "data.frame", "matrix"))
  out <- is.na(xvect)
  total <- sum(out, na.rm=TRUE)
  is_na <- total == (1 * length(out))
  list(is.na = is_na, class = class(xvect))
}
