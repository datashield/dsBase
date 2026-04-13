#'
#' @title Applies the \code{unique} method to a server-side variable.
#' @description This function is similar to R function \code{unique}.
#' @details The function computes the uniques values of a variable.
#' @param x.name.transmit is the name of the variable upon which \code{unique} method will be applied
#' @return the object specified by the \code{newobj} argument
#' which is written to the server-side.
#' @author Stuart Wheater for DataSHIELD Development Team
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export
#'
uniqueDS <- function(x.name.transmit = NULL){
    x.value <- .loadServersideObject(x.name.transmit)
    out <- base::unique(x.value)
    return(out)
}
# ASSIGN FUNCTION
# uniqueDS
