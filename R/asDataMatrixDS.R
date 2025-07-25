#' @title asDataMatrixDS a serverside assign function called by ds.asDataMatrix
#' @description Coerces an R object into a matrix maintaining original
#' class for all columns in data.frames.
#' @details This assign function is based on the native R function \code{data.matrix}
#' If applied to a data.frame, the native R function \code{as.matrix}
#' coverts all columns into character class. In contrast, if applied to
#' a data.frame the native R function \code{data.matrix} converts
#' the data.frame to a matrix but maintains all data columns in their
#' original class
#' @param x.name the name of the input object to be coerced to class
#' data.matrix. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' \code{ds.asDataMatrix}
#' @return the object specified by the <newobj> argument (or its default name
#' "asdatamatrix.newobj") which is written to the serverside. For further
#' details see help on the clientside function \code{ds.asDataMatrix}
#' @author Paul Burton for DataSHIELD Development Team
#' @export
asDataMatrixDS <- function(x.name) {
  if (is.character(x.name)) {
    x <- eval(parse(text = x.name), envir = parent.frame())
  } else {
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    stop(studysideMessage, call. = FALSE)
  }

  output <- data.matrix(x)

  return(output)
}
# ASSIGN FUNCTION
# asDataMatrixDS
