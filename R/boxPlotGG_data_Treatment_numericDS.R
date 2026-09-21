#' @title Arrange vector to pass it to the boxplot function
#'
#' @param vector.name \code{character} Name of a server-side numeric vector to arrange to be plotted later
#'
#' @return \code{data frame} with the following structure: \cr
#'
#'  Column 'x': Names on the X axis of the boxplot, aka name of the vector (vector.name argument) \cr
#'  Column 'value': Values for that variable \cr
#'
#' @author Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
#' @export

boxPlotGG_data_Treatment_numericDS <- function(vector.name){

  vector <- .loadServersideObject(vector.name)
  .checkClass(obj = vector, obj_name = vector.name, permitted_classes = c("numeric", "integer"))

  data <- data.frame(x = vector.name, value = vector)

  return(data)

}