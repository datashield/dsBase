#' 
#' @title Coerce objects into a list
#' @description this function is similar to R function 'list'
#' @details Unlike the R function 'list' it takes also a vector of characters,
#' the names of the elements in the output list.
#' @param x.names a character vector of object names to coerce into a list.
#' @param eltnames a character vector, the names of the elements in the list.
#' @return a list
#' @author Gaye, A.
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
listDS <- function (x.names = NULL, eltnames = NULL) {

  mylist <- list()
  for (i in seq_along(x.names)) {
    mylist[[i]] <- .loadServersideObject(x.names[i])
  }
  names(mylist) <- unlist(eltnames)

  return(mylist)
}