#'
#' @title Applies the \code{unique} method to a server-side variable.
#' @description This function is similar to R function \code{unique}.
#' @details The function computes the uniques values of a variable.
#' @param x.name.transmit is the name of the variable upon which \code{unique} method will be applied
#' @return the object specified by the \code{newobj} argument
#' which is written to the server-side.
#' @author Stuart Wheater for DataSHIELD Development Team
#' @export
#'
uniqueDS <- function(x.name.transmit = NULL){
    # Check 'x.name.transmit' contains a name
    if (is.null(x.name.transmit))
        stop("Variable's name can't be NULL", call. = FALSE)

    if ((! is.character(x.name.transmit)) || (length(x.name.transmit) != 1))
        stop("Variable's name isn't a single character vector", call. = FALSE)

    # Check object exists
    x.value <- eval(parse(text=x.name.transmit), envir = parent.frame())

    if (is.null(x.value))
        stop("Variable can't be NULL", call. = FALSE)

    # Compute the unique's value
    out <- base::unique(x.value)

    # assign the outcome to the data servers
    return(out)
}
# ASSIGN FUNCTION
# uniqueDS
