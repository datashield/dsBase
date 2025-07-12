#' @title asDataFrameDS a serverside assign function called by ds.asDataFrame
#' @description Coerces an R object into a matrix maintaining original
#' class for all columns in data.frames.
#' @details This assign function is based on the native R function \code{data.frame}
#' @param x.name the name of the input object to be coerced to class
#' data.frame. Must be specified in inverted commas. But this argument is
#' usually specified directly by <x.name> argument of the clientside function
#' \code{ds.asDataFrame}
#' @return the object specified by the <newobj> argument (or its default name
#' "asdataframe.newobj") which is written to the serverside. For further
#' details see help on the clientside function \code{ds.asDataMatrix}
#' @author Tim Cadman
#' @export
asDataMatrixDS <- function(x.name) {
  if (is.character(x.name)) {
    x <- eval(parse(text = x.name), envir = parent.frame())
  } else {
    studysideMessage <- "ERROR: x.name must be specified as a character string"
    stop(studysideMessage, call. = FALSE)
  }

  output <- data.frame(x)

  return(output)
}
# ASSIGN FUNCTION
# asDataFrameDS
