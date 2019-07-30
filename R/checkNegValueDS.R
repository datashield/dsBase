#'
#' @title Checks if a numeric variable has negative values
#' @description this function is only called by the client function \code{ds.glm}.
#' @details if a user sets the parameter 'weights' on the client site function \code{ds.glm} this 
#' server side function is called to verify that the 'weights' vector does not have negative values
#' because no negative are allowed in weights.
#' @param weights a numeric vector
#' @return a boolean; TRUE if the vector has one or more negative values and FALSE otherwise
#' @author Gaye, A.
#' @export
#'
checkNegValueDS <- function (weights) {
  
  # check if any of the entries is negative and return the output accordingly
  idx <- which(weights < 0)
  length(idx) > 0
  
}
