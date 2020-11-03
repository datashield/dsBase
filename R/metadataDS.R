#'
#' @title Returns the metadata, if any, about the specified variable
#' @description This function returns metadata, if any, about specified variable.
#' @details The function returns the metadata, obtained from attributes function.
#' @param x a string character, containing the name  of the specified variable
#' @return a list containing the metadata. The elements of the list will depend
#' on the meatadata available.
#' @author Stuart Wheater, for DataSHIELD Development Team
#' @export
#'
metadataDS <- function(x)
{

    if (is.null(x))
        stop("Variable's name can't be NULL", call. = FALSE)

    if ((! is.character(x)) || (length(x) != 1))
        stop("Variable's name isn't be single character vector", call. = FALSE)

    if (! exists(x))
        stop(paste0("The variable '", x, "' does not exist"), call. = FALSE)

    x.var <- eval(parse(text=x), envir = parent.frame())

    # find the metadata specified variable
    metadata <- attributes(x.var)

    # return metadata
    return(metadata)

}
#AGGREGATE FUNCTION
# metadataDS
