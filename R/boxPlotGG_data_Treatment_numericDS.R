#' @title Arrange vector to pass it to the boxplot function
#'
#' @param vector \code{numeric vector} Vector to arrange to be plotted later
#'
#' @return \code{data frame} with the following structure: \cr
#' 
#'  Column 'x': Names on the X axis of the boxplot, aka name of the vector (vector argument) \cr
#'  Column 'value': Values for that variable \cr
#'  
#' @export

boxPlotGG_data_Treatment_numericDS <- function(vector){
  
  data <- data.frame(x = deparse(substitute(vector)), value = vector)
  
  return(data)
  
}