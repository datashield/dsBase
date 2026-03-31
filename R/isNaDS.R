#'
#' @title Checks if a vector is empty
#' @description this function is similar to R function \code{is.na} but instead of a vector
#' of booleans it returns just one boolean to tell if all the element are missing values.
#' @param x a character string, the name of a server-side vector
#' @return TRUE if the vector contains only NAs, FALSE otherwise
#' @author Gaye, A.
#' @export
#'
isNaDS <- function(x){
  xvect <- .loadServersideObject(x)
  .checkClass(obj = xvect, obj_name = x, permitted_classes = c("character", "factor", "integer", "logical", "numeric", "data.frame", "matrix"))
  out <- is.na(xvect)
  total <- sum(out, na.rm=TRUE)
  if(total == (1 * length(out))){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
